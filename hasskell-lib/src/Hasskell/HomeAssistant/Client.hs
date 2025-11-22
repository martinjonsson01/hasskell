{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client
  ( startWebSocket,
    runClient,
    ClientError,
    HASSAuthResponse,
  )
where

import Control.Concurrent.STM
import Control.Exception.Base
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Deriving.Aeson
import Hasskell.Config
import Hasskell.HomeAssistant.Types
import Hasskell.Monad
import Network.WebSockets qualified as WS
import Text.Show.Pretty

type ClientM = HassM ClientEnv ClientError IO

data ClientEnv = ClientEnv
  { clientConfig :: Config,
    clientMessageCounter :: TVar CorrelationId
  }

instance HasConfig ClientEnv where
  getConfig = clientConfig

data ClientError
  = ParserError Text Text
  | InvalidAuthentication Text
  | UnknownResponse Text
  | EnvelopeMismatch {mismatchedRequestId :: CorrelationId, mismatchedResponseId :: CorrelationId}
  deriving (Generic, Eq, Show)

instance Exception ClientError

makeEnv :: Config -> IO ClientEnv
makeEnv config = do
  counter <- newTVarIO 1 -- Has to start at 1, 0 results in "Message incorrectly formatted."
  pure $ ClientEnv {clientConfig = config, clientMessageCounter = counter}

runClient :: Config -> ClientM a -> IO (Either ClientError a)
runClient config client = do
  env <- makeEnv config
  runExceptT (runReaderT client env)

startWebSocket :: ClientM ()
startWebSocket = do
  socketUrl <- T.unpack <$> asks (baseUrl . clientConfig)
  config <- asks clientConfig
  -- since `webSocketWithTimeout` throws the `ClientError`s, we need to convert them.
  runClientConvertIOError $
    WS.runClient socketUrl 80 "/api/websocket" (webSocketWithTimeout config)

webSocketWithTimeout :: Config -> WS.ClientApp ()
webSocketWithTimeout config connection =
  -- `withPingPong` doesn't allow us to return `ClientError`s so we throw them instead.
  WS.withPingPong WS.defaultPingPongOptions connection (runClientThrowErrorAsIO config . app)

runClientConvertIOError :: IO () -> ClientM ()
runClientConvertIOError client = do
  caughtResult <- liftIO $ try client
  case caughtResult of
    Left (clientError :: ClientError) -> throwError clientError
    Right _ -> pure ()

runClientThrowErrorAsIO :: Config -> ClientM () -> IO ()
runClientThrowErrorAsIO config client = do
  result <- runClient config client
  case result of
    Left clientError -> throwIO clientError
    Right success -> pure success

app :: WS.Connection -> ClientM ()
app connection = do
  logDebug "authenticating..."
  authenticate connection
  logDebug "authenticated!"

  configResult :: HASSResult HASSConfig <- sendCommand connection CommandGetConfig
  logDebug $ T.pack $ ppShow configResult

  _ :: HASSResult [HASSState] <- sendCommand connection CommandGetStates

  _ :: HASSResult (HASSServiceActions) <- sendCommand connection CommandGetServices

  actionResult :: HASSResult HASSActionResult <-
    sendCommand
      connection
      ( CommandCallService
          { commandReturnResponse = False,
            commandTarget =
              Just
                ( Target
                    { targetEntityId = L.singleton "light.flaktlampa",
                      targetDeviceId = mempty,
                      targetLabelId = mempty,
                      targetAreaId = mempty
                    }
                ),
            commandServiceData = Nothing,
            commandService = ServiceName "toggle",
            commandDomain = Domain "light"
          }
      )
  logDebug $ T.pack $ ppShow actionResult

  liftIO $ WS.sendClose connection ("Bye!" :: Text)

-- | Authenticates the Home Assistant websocket.
authenticate :: WS.Connection -> ClientM ()
authenticate connection = do
  let handleAuthResponse message = do
        logDebug $ T.concat ["got auth response ", T.show message]
        case message of
          Left clientError -> throwError clientError
          Right (ResponseAuthRequired _) -> do
            hassToken <- asks (token . clientConfig)
            send connection $ MessageAuth hassToken
            authMessage <- receive connection
            handleAuthResponse authMessage
          Right (ResponseAuthInvalid errorMessage) -> throwError $ InvalidAuthentication errorMessage
          Right (ResponseAuthOk _) -> pure ()

  initialMessage <- liftIO $ WS.receiveData connection
  handleAuthResponse initialMessage

-- | Sends data to Home Assistant.
send :: (WS.WebSocketsData a, ToJSON a) => WS.Connection -> a -> ClientM ()
send connection value = do
  logDebug $ T.concat ["sending: ", TE.decodeUtf8 $ BL.toStrict $ encodePretty value]
  liftIO $ WS.sendTextData connection value

receive :: (FromJSON a) => WS.Connection -> ClientM (Either ClientError a)
receive connection = liftIO $ WS.receiveData connection

-- | Sends a given envelope to Home Assistant.
sendMessage :: (ToJSON a, FromJSON b) => WS.Connection -> Envelope a -> ClientM (Envelope (HASSResult b))
sendMessage connection command = do
  send connection command
  result <- receive connection
  case result of
    Left clientError -> throwError clientError
    Right response -> pure response

-- | Sends a command to Home Assistant.
sendCommand :: (ToJSON a, FromJSON b) => WS.Connection -> a -> ClientM (HASSResult b)
sendCommand connection command = do
  requestId <- incrementMessageCounter
  Envelope responseId payload <- sendMessage connection (Envelope requestId command)
  unless (requestId == responseId) (throwError $ EnvelopeMismatch requestId responseId)
  pure payload

incrementMessageCounter :: ClientM CorrelationId
incrementMessageCounter = do
  counter <- asks clientMessageCounter
  liftIO $ atomically $ do
    count <- readTVar counter
    modifyTVar' counter (+ 1)
    return count

parseEither :: (FromJSON a) => BL.ByteString -> Either ClientError a
parseEither s = case eitherDecode s of
  Left errorMessage -> Left . ParserError (TL.toStrict $ TLE.decodeUtf8 s) $ T.pack errorMessage
  Right response -> Right response

instance (FromJSON a) => WS.WebSocketsData (Either ClientError a) where
  fromLazyByteString = parseEither
  toLazyByteString = undefined
  fromDataMessage (WS.Text lbs _) = parseEither lbs
  fromDataMessage (WS.Binary lbs) = parseEither lbs
