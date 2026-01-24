{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.HASSConnection
  ( -- Connection to Home Assistant
    HASSConnection,
    sendMessage,
    sendMessageGetId,
    sendRequest,
    receiveChange,
    runWithHASSWebSocket,
    HASSWebSocketError (..),
  )
where

import Control.Monad
import Control.Placeholder
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time
import Data.Time.Format.ISO8601
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Error.Static
import Effectful.Exception
import Effectful.TH
import Hasskell.Config
import Hasskell.Effects.BoundedMap (BoundedMap)
import Hasskell.Effects.BoundedMap qualified as BM
import Hasskell.Effects.Counter
import Hasskell.Effects.Logging
import Hasskell.Effects.Time
import Hasskell.Effects.Utils
import Hasskell.HomeAssistant.API
import Hasskell.HomeAssistant.Version
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.WebSockets qualified as WS

data SomeHASSMessage = forall a. (ToJSON a) => SomeMessage a

data HASSConnection :: Effect where
  SendMessage :: (ToJSON a, FromJSON b) => a -> HASSConnection m b
  SendMessageGetId :: (ToJSON a, FromJSON b) => a -> HASSConnection m (CorrelationId, b)
  SendRequest :: (FromJSON b) => HASSRequest -> HASSConnection m b
  ReceiveChange :: CorrelationId -> HASSConnection m HASSChange

makeEffect ''HASSConnection

type SourceJSONText = Text

type MessageText = Text

data HASSWebSocketError
  = ParserError SourceJSONText MessageText
  | InvalidAuthentication Text
  | WebSocketLogError LogError
  | CommandFailure HASSFailure
  | HttpRequestFailure HttpException
  | IncompatibleVersion HASSVersion
  | WebSocketDied
  deriving (Show)

runWithHASSWebSocket ::
  ( IOE :> es,
    Time :> es,
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
  changeMap <- BM.newBoundedMap 100
  sendQueue <- atomically $ newTBQueue 100

  let websocket = runWebSocket receiveMap changeMap sendQueue
      interpreter =
        interpretWith_ action $ \case
          SendMessage message -> snd <$> handleSendMessage sendQueue receiveMap message
          SendMessageGetId message -> handleSendMessage sendQueue receiveMap message
          SendRequest request -> handleSendRequest request
          ReceiveChange subscriptionId -> BM.remove subscriptionId changeMap

  race websocket interpreter >>= \result -> do
    case result of
      Left () -> do
        logError "Web socket finished before the action"
        throwError WebSocketDied
      Right b -> do
        logDebug "HASSConnection interpreter finished, killing web socket..."
        pure b

handleSendRequest ::
  ( Error HASSWebSocketError :> es,
    IOE :> es,
    Time :> es,
    Configured :> es,
    FromJSON b
  ) =>
  HASSRequest ->
  Eff es b
handleSendRequest = \case
  RequestStateHistory offset entities -> do
    let entityFilter = T.intercalate "," (map (unwrapEntityId . unwrapKnownEntityId) entities)
    currentTime <- getCurrentUTCTime
    let periodStart = addUTCTime (-offset) currentTime
        periodEnd = C8.pack (toUTCString currentTime)
    response <-
      sendHttpRequest
        ["/api/history/period/" <> toUTCString periodStart]
        [ ("filter_entity_id", Just (TE.encodeUtf8 entityFilter)),
          ("end_time", Just periodEnd), -- history up until now
          ("minimal_response", Nothing), -- only return last_changed and state (much faster)
          ("no_attributes", Nothing) -- skip returning attributes (much faster)
        ]
    responseValue <- liftEither (parseEither (getResponseBody response))
    convertFromJSON responseValue

toUTCString :: UTCTime -> String
toUTCString =
  formatShow
    ( utcTimeFormat
        (calendarFormat ExtendedFormat)
        (timeOfDayFormat ExtendedFormat)
    )

sendHttpRequest ::
  ( Error HASSWebSocketError :> es,
    IOE :> es,
    Configured :> es
  ) =>
  [String] ->
  [(B.ByteString, Maybe B.ByteString)] ->
  Eff es (Response BL.ByteString)
sendHttpRequest pathSegments queryParams = do
  base <- baseUrl <$> getConfig
  let path = L.intercalate "/" pathSegments
  result <- try $ do
    request <- parseUrlThrow ("https://" <> T.unpack base <> path)
    token <- TE.encodeUtf8 . token <$> getConfig
    let authorizedRequest = applyBearerAuth token request
        authorizedQueryRequest = setQueryString queryParams authorizedRequest
    httpLBS authorizedQueryRequest
  liftEither $ leftMap HttpRequestFailure $ result

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

  (requestId,) <$> convertFromJSON result

convertFromJSON :: (Error HASSWebSocketError :> es, FromJSON b) => Value -> Eff es b
convertFromJSON string =
  case fromJSON string of
    Error errorMessage ->
      throwError $
        ParserError
          (TE.decodeUtf8 $ BL.toStrict $ encodePretty string)
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
  BoundedMap CorrelationId (HASSResult Value) ->
  BoundedMap CorrelationId HASSChange ->
  TBQueue (Envelope SomeHASSMessage) ->
  Eff es ()
runWebSocket receiveMap changeMap sendQueue = do
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
          . handleWebsocket receiveMap changeMap sendQueue
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
  BoundedMap CorrelationId HASSChange ->
  TBQueue (Envelope SomeHASSMessage) ->
  WS.Connection ->
  Eff es ()
handleWebsocket receiveMap changeMap sendQueue connection = do
  logDebug "authenticating..."
  authenticate connection
  logDebug "authenticated!"

  let receiver =
        forever $ do
          responseData <- liftIO (WS.receiveData connection)
          Envelope responseId response <- liftEither responseData
          case response :: HASSResponse of
            ResponseResult result -> BM.insert responseId result receiveMap
            ResponseChange change -> BM.insert responseId change changeMap

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
          ResponseAuthRequired version -> do
            ensureCompatibleVersion version
            hassToken <- token <$> getConfig
            send connection $ MessageAuth hassToken
            authMessage <- receive connection
            handleAuthResponse authMessage
          ResponseAuthInvalid errorMessage -> throwError $ InvalidAuthentication errorMessage
          ResponseAuthOk version -> ensureCompatibleVersion version

  initialMessage <- receive connection
  handleAuthResponse initialMessage

ensureCompatibleVersion ::
  (Error HASSWebSocketError :> es) =>
  HASSVersion -> Eff es ()
ensureCompatibleVersion version
  | version >= minimumSupportedVersion = pure ()
  | otherwise = throwError (IncompatibleVersion version)

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
