{-# LANGUAGE UndecidableInstances #-}
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
import Control.Monad.Reader.Has
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
import Hasskell.HomeAssistant.API
import Hasskell.Monad
import Network.WebSockets qualified as WS
import Text.Show.Pretty

type ClientM = HassM ClientEnv ClientError IO

data ClientEnv = ClientEnv
  { clientConfig :: Config,
    clientMessageCounter :: TVar CorrelationId
  }
  deriving (Generic, Has Config, Has (TVar CorrelationId))

data ClientError
  = ParserError Text Text
  | InvalidAuthentication Text
  | UnknownResponse Text
  | EnvelopeMismatch {mismatchedRequestId :: CorrelationId, mismatchedResponseId :: CorrelationId}
  | CommandFailure HASSFailure
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

app ::
  ( MonadMutableClient m,
    MonadWS m,
    MonadLog m,
    MonadError ClientError m,
    MonadReader env m,
    Has ClientEnv env
  ) =>
  WS.Connection -> m ()
app connection = do
  logDebug "authenticating..."
  authenticate connection
  logDebug "authenticated!"

  configResult :: HASSConfig <- sendCommand connection CommandGetConfig
  logDebug $ T.pack $ ppShow configResult

  states :: [HASSState] <- sendCommand connection CommandGetStates
  logDebug $ "state count: " <> (T.show $ length $ states)

  actions :: (HASSServiceActions) <- sendCommand connection CommandGetServices
  logDebug $ "action count: " <> (T.show $ length $ actions)

  actionResult :: HASSActionResult <-
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

  send connection ("Bye!" :: Text)

-- | Authenticates the Home Assistant websocket.
authenticate :: (MonadWS m, MonadLog m, Has ClientEnv env, MonadReader env m, MonadError ClientError m) => WS.Connection -> m ()
authenticate connection = do
  let handleAuthResponse message = do
        logDebug $ T.concat ["got auth response ", T.show message]
        response <- liftEither message
        case response of
          ResponseAuthRequired _ -> do
            hassToken :: Text <- asks (token . extract @Config @ClientEnv)
            send connection $ MessageAuth hassToken
            authMessage <- receive connection
            handleAuthResponse authMessage
          ResponseAuthInvalid errorMessage -> throwError $ InvalidAuthentication errorMessage
          ResponseAuthOk _ -> pure ()

  initialMessage <- receive connection
  handleAuthResponse initialMessage

--------------------------------------------------------------------------------

class MonadWS m where
  sendTextData :: (WS.WebSocketsData a) => WS.Connection -> a -> m ()
  receiveData :: (WS.WebSocketsData a) => WS.Connection -> m a

instance (MonadIO m) => MonadWS m where
  sendTextData connection = liftIO . WS.sendTextData connection
  receiveData = liftIO . WS.receiveData

-- | Sends data to Home Assistant.
send :: (MonadWS m, MonadLog m, WS.WebSocketsData a, ToJSON a, Monad m) => WS.Connection -> a -> m ()
send connection value = do
  logDebug $ T.concat ["sending: ", TE.decodeUtf8 $ BL.toStrict $ encodePretty value]
  sendTextData connection value

receive :: (MonadWS m, FromJSON a) => WS.Connection -> m (Either ClientError a)
receive connection = receiveData connection

-- | Sends a given envelope to Home Assistant.
sendMessage ::
  ( MonadLog m,
    MonadWS m,
    MonadError ClientError m,
    ToJSON a,
    FromJSON b
  ) =>
  WS.Connection -> Envelope a -> m (Envelope (HASSResult b))
sendMessage connection command = do
  send connection command
  receive connection >>= liftEither

parseEither :: (FromJSON a) => BL.ByteString -> Either ClientError a
parseEither s = case eitherDecode s of
  Left errorMessage -> Left . ParserError (TL.toStrict $ TLE.decodeUtf8 s) $ T.pack errorMessage
  Right response -> Right response

instance (FromJSON a) => WS.WebSocketsData (Either ClientError a) where
  fromLazyByteString = parseEither
  toLazyByteString = undefined
  fromDataMessage (WS.Text lbs _) = parseEither lbs
  fromDataMessage (WS.Binary lbs) = parseEither lbs

--------------------------------------------------------------------------------

class MonadMutableClient m where
  incrementMessageCounter :: m CorrelationId

instance (MonadIO m, MonadReader env m, Has (TVar CorrelationId) env) => MonadMutableClient m where
  incrementMessageCounter = do
    counter <- ask
    liftIO $ atomically $ do
      count <- readTVar counter
      modifyTVar' counter (+ 1)
      return count

sendCommand ::
  ( MonadMutableClient m,
    MonadError ClientError m,
    MonadWS m,
    MonadLog m,
    ToJSON a,
    FromJSON b
  ) =>
  WS.Connection -> a -> m b
sendCommand connection command = do
  result <- sendCommand' connection command
  liftEither $ leftMap CommandFailure $ value result
  where
    leftMap :: (a -> c) -> Either a b -> Either c b
    leftMap f (Left l) = Left (f l)
    leftMap _ (Right r) = Right r

-- | Sends a command to Home Assistant.
sendCommand' ::
  ( MonadMutableClient m,
    MonadError ClientError m,
    MonadWS m,
    MonadLog m,
    ToJSON a,
    FromJSON b
  ) =>
  WS.Connection -> a -> m (HASSResult b)
sendCommand' connection command = do
  requestId <- incrementMessageCounter
  Envelope responseId payload <- sendMessage connection (Envelope requestId command)
  unless (requestId == responseId) (throwError $ EnvelopeMismatch requestId responseId)
  pure payload

--------------------------------------------------------------------------------
