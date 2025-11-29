{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Monad
  ( -- Connection to Home Assistant
    HASSConnection,
    sendMessage,
    runWithHASSWebSocket,
    HASSWebSocketError (..),
    -- Utils
    runMapError,
  )
where

import Control.Monad (forever, unless)
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
import Hasskell.Effects.Counter
import Hasskell.Effects.Logging
import Hasskell.HomeAssistant.API
import Network.WebSockets qualified as WS

data SomeHASSMessage = forall a. (ToJSON a) => SomeMessage a

data HASSConnection :: Effect where
  SendMessage :: (ToJSON a, FromJSON b) => a -> HASSConnection m b

makeEffect ''HASSConnection

type SourceJSONText = Text

type MessageText = Text

type MismatchedRequestId = CorrelationId

type MismatchedResponseId = CorrelationId

data HASSWebSocketError
  = ParserError SourceJSONText MessageText
  | EnvelopeMismatch MismatchedRequestId MismatchedResponseId
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
  receiveQueue <- atomically $ newTBQueue 100
  sendQueue <- atomically $ newTBQueue 100

  let websocket = runWebSocket receiveQueue sendQueue
      interpreter =
        interpretWith_ action $ \case
          SendMessage message -> handleSendMessage sendQueue receiveQueue message

  race websocket interpreter >>= \result -> do
    logDebug "after websocket/interpreter?"
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
  TBQueue (Envelope (HASSResult Value)) ->
  a ->
  Eff es b
handleSendMessage sendQueue receiveQueue message = do
  requestId <- newId

  atomically $ writeTBQueue sendQueue (Envelope requestId (SomeMessage message))
  Envelope responseId response <- atomically $ readTBQueue receiveQueue

  unless (requestId == responseId) (throwError $ EnvelopeMismatch requestId responseId)

  result <- liftEither $ leftMap CommandFailure $ value response

  case fromJSON result of
    Error errorMessage ->
      throwError $
        ParserError
          (TE.decodeUtf8 $ BL.toStrict $ encodePretty result)
          (T.pack errorMessage)
    Success value -> pure value

leftMap :: (a -> c) -> Either a b -> Either c b
leftMap f (Left l) = Left (f l)
leftMap _ (Right r) = Right r

runWebSocket ::
  ( Configured :> es,
    Error HASSWebSocketError :> es,
    Logger :> es,
    IOE :> es
  ) =>
  TBQueue (Envelope (HASSResult Value)) ->
  TBQueue (Envelope SomeHASSMessage) ->
  Eff es ()
runWebSocket recieveQueue sendQueue = do
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
          . handleWebsocket recieveQueue sendQueue
  logDebug "starting websocket..."
  ( liftIO $
      WS.runClient
        socketUrl
        80
        "/api/websocket"
        websocket
    )
    >>= liftEither

runMapError ::
  (Error e2 :> es, Show e2) =>
  (e1 -> e2) ->
  Eff (Error e1 : es) a ->
  Eff es a
runMapError errConstructor action =
  runErrorNoCallStack action
    >>= either (throwError . errConstructor) pure

handleWebsocket ::
  ( IOE :> es,
    Concurrent :> es,
    Configured :> es,
    Logger :> es,
    Error HASSWebSocketError :> es
  ) =>
  TBQueue (Envelope (HASSResult Value)) ->
  TBQueue (Envelope SomeHASSMessage) ->
  WS.Connection ->
  Eff es ()
handleWebsocket receiveQueue sendQueue connection = do
  logDebug "authenticating..."
  authenticate connection
  logDebug "authenticated!"

  let receiver =
        forever $ do
          response <- liftIO (WS.receiveData connection)
          value <- liftEither response
          atomically $ writeTBQueue receiveQueue value

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
  toLazyByteString = undefined
  fromDataMessage (WS.Text lbs _) = parseEither lbs
  fromDataMessage (WS.Binary lbs) = parseEither lbs

--------------------------------------------------------------------------------
