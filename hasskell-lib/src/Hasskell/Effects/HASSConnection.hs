{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.HASSConnection
  ( -- Connection to Home Assistant
    HASSConnection,
    sendMessage,
    sendMessageGetId,
    receiveEvent,
    runWithHASSWebSocket,
    HASSWebSocketError (..),
  )
where

import Control.Monad
import Control.Placeholder
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Error.Static
import Effectful.TH
import Hasskell.Config
import Hasskell.Effects.BoundedMap (BoundedMap)
import Hasskell.Effects.BoundedMap qualified as BM
import Hasskell.Effects.Counter
import Hasskell.Effects.Logging
import Hasskell.Effects.Utils
import Hasskell.HomeAssistant.API
import Network.WebSockets qualified as WS

data SomeHASSMessage = forall a. (ToJSON a) => SomeMessage a

data HASSConnection :: Effect where
  SendMessage :: (ToJSON a, FromJSON b) => a -> HASSConnection m b
  SendMessageGetId :: (ToJSON a, FromJSON b) => a -> HASSConnection m (CorrelationId, b)
  ReceiveEvent :: CorrelationId -> HASSConnection m HASSEvent

makeEffect ''HASSConnection

type SourceJSONText = Text

type MessageText = Text

data HASSWebSocketError
  = ParserError SourceJSONText MessageText
  | InvalidAuthentication Text
  | WebSocketLogError LogError
  | CommandFailure HASSFailure
  | WebSocketDied
  deriving (Eq, Show)

runWithHASSWebSocket ::
  ( IOE :> es,
    Counter :> es,
    Concurrent :> es,
    Error HASSWebSocketError :> es,
    Logger :> es,
    Configured :> es
  ) =>
  Eff (HASSConnection : es) a ->
  Eff es a
runWithHASSWebSocket action = do
  receiveMap <- BM.newBoundedMap 100
  eventMap <- BM.newBoundedMap 100
  sendQueue <- atomically $ newTBQueue 100

  let websocket = runWebSocket receiveMap eventMap sendQueue
      interpreter =
        interpretWith_ action $ \case
          SendMessage message -> snd <$> handleSendMessage sendQueue receiveMap message
          SendMessageGetId message -> handleSendMessage sendQueue receiveMap message
          ReceiveEvent subscriptionId -> BM.remove subscriptionId eventMap

  race websocket interpreter >>= \result -> do
    case result of
      Left () -> do
        logError "Web socket finished before the action"
        throwError WebSocketDied
      Right b -> do
        logDebug "interpreter finished, killing web socket..."
        pure b

handleSendMessage ::
  ( ToJSON a,
    Error HASSWebSocketError :> es,
    Counter :> es,
    Concurrent :> es,
    FromJSON b
  ) =>
  TBQueue (Envelope SomeHASSMessage) ->
  BoundedMap CorrelationId (HASSResult Value) ->
  a ->
  Eff es (CorrelationId, b)
handleSendMessage sendQueue receiveMap message = do
  requestId <- newId

  atomically $
    writeTBQueue sendQueue (Envelope requestId (SomeMessage message))

  response <- BM.remove requestId receiveMap

  result <- liftEither $ leftMap CommandFailure $ value response

  case fromJSON result of
    Error errorMessage ->
      throwError $
        ParserError
          (TE.decodeUtf8 $ BL.toStrict $ encodePretty result)
          (T.pack errorMessage)
    Success value -> pure (requestId, value)

leftMap :: (a -> c) -> Either a b -> Either c b
leftMap f (Left l) = Left (f l)
leftMap _ (Right r) = Right r

runWebSocket ::
  ( Configured :> es,
    Error HASSWebSocketError :> es,
    Logger :> es,
    IOE :> es
  ) =>
  BoundedMap CorrelationId (HASSResult Value) ->
  BoundedMap CorrelationId HASSEvent ->
  TBQueue (Envelope SomeHASSMessage) ->
  Eff es ()
runWebSocket receiveMap eventMap sendQueue = do
  config <- getConfig
  let logConfig = logging config
      socketUrl = T.unpack $ baseUrl config
      websocket =
        runEff
          . runConfigured config
          . runErrorNoCallStack @HASSWebSocketError
          . runMapError WebSocketLogError
          . runLogger logConfig
          . runConcurrent
          . handleWebsocket receiveMap eventMap sendQueue
  logDebug "starting websocket..."
  ( liftIO $
      WS.runClient
        socketUrl
        80
        "/api/websocket"
        websocket
    )
    >>= liftEither

handleWebsocket ::
  ( IOE :> es,
    Concurrent :> es,
    Configured :> es,
    Logger :> es,
    Error HASSWebSocketError :> es
  ) =>
  BoundedMap CorrelationId (HASSResult Value) ->
  BoundedMap CorrelationId HASSEvent ->
  TBQueue (Envelope SomeHASSMessage) ->
  WS.Connection ->
  Eff es ()
handleWebsocket receiveMap eventMap sendQueue connection = do
  logDebug "authenticating..."
  authenticate connection
  logDebug "authenticated!"

  let receiver =
        forever $ do
          responseData <- liftIO (WS.receiveData connection)
          Envelope responseId response <- liftEither responseData
          case response :: HASSResponse of
            ResponseResult result -> BM.insert responseId result receiveMap
            ResponseEvent event -> BM.insert responseId event eventMap

      sender = forever $ do
        Envelope envelopeId (SomeMessage message) <- atomically $ readTBQueue sendQueue
        send connection (Envelope envelopeId message)

  concurrently_ receiver sender

-- | Authenticates the Home Assistant websocket.
authenticate ::
  ( IOE :> es,
    Configured :> es,
    Logger :> es,
    Error HASSWebSocketError :> es
  ) =>
  WS.Connection ->
  Eff es ()
authenticate connection = do
  let handleAuthResponse message = do
        logDebug $ T.concat ["got auth response ", T.show message]
        response <- liftEither message
        case response of
          ResponseAuthRequired _ -> do
            hassToken <- token <$> getConfig
            send connection $ MessageAuth hassToken
            authMessage <- receive connection
            handleAuthResponse authMessage
          ResponseAuthInvalid errorMessage -> throwError $ InvalidAuthentication errorMessage
          ResponseAuthOk _ -> pure ()

  initialMessage <- receive connection
  handleAuthResponse initialMessage

-- | Sends data to Home Assistant.
send :: (IOE :> es, Logger :> es, WS.WebSocketsData a, ToJSON a) => WS.Connection -> a -> Eff es ()
send connection value = do
  logDebug $ T.concat ["sending: ", TE.decodeUtf8 $ BL.toStrict $ encodePretty value]
  sendTextData value
  where
    sendTextData :: (IOE :> es, WS.WebSocketsData a) => a -> Eff es ()
    sendTextData = liftIO . WS.sendTextData connection

receive :: (IOE :> es, WS.WebSocketsData a) => WS.Connection -> Eff es a
receive = liftIO . WS.receiveData

liftEither :: (Error e :> es, Show e) => Either e a -> Eff es a
liftEither value = case value of
  Left err -> throwError err
  Right result -> pure result

parseEither :: (FromJSON a) => BL.ByteString -> Either HASSWebSocketError a
parseEither s = case eitherDecode s of
  Left errorMessage -> Left . ParserError (TL.toStrict $ TLE.decodeUtf8 s) $ T.pack errorMessage
  Right response -> Right response

instance (FromJSON a) => WS.WebSocketsData (Either HASSWebSocketError a) where
  fromLazyByteString = parseEither
  toLazyByteString = unimplemented
  fromDataMessage (WS.Text lbs _) = parseEither lbs
  fromDataMessage (WS.Binary lbs) = parseEither lbs

--------------------------------------------------------------------------------
