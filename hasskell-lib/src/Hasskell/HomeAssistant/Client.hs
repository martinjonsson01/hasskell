{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client (startWebSocket, runClient, ClientError, HASSAuthResponse) where

import Control.Exception.Base
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Deriving.Aeson
import Hasskell.Config
import Hasskell.HomeAssistant.Types
import Network.WebSockets qualified as WS

type ClientM = ReaderT Config (ExceptT ClientError IO)

data ClientError
  = ParserError Text Text
  | InvalidAuthentication Text
  deriving (Generic, Eq, Show)

instance Exception ClientError

runClient :: Config -> ClientM a -> IO (Either ClientError a)
runClient config client = runExceptT (runReaderT client config)

startWebSocket :: ClientM ()
startWebSocket = do
  socketUrl <- T.unpack <$> asks baseUrl
  config <- ask
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
  authenticate connection

  liftIO $ WS.sendClose connection ("Bye!" :: Text)

authenticate :: WS.Connection -> ClientM ()
authenticate connection = do
  initialMessage <- liftIO $ WS.receiveData connection
  case initialMessage of
    Left clientError -> throwError clientError
    Right response -> liftIO $ print (response :: HASSAuthResponse)

  hassToken <- asks token
  liftIO $ send connection $ MessageAuth hassToken
  authMessage <- liftIO $ WS.receiveData connection
  case authMessage of
    Left clientError -> throwError clientError
    Right (ResponseAuthInvalid errorMessage) -> throwError $ InvalidAuthentication errorMessage
    Right response -> liftIO $ print (response :: HASSAuthResponse)

send :: (WS.WebSocketsData a, ToJSON a) => WS.Connection -> a -> IO ()
send connection value = do
  BL8.putStrLn $ BL8.concat ["sending: ", encode value]
  WS.sendTextData connection value

parseEither :: BL.ByteString -> Either ClientError HASSAuthResponse
parseEither s = case eitherDecode s of
  Left errorMessage -> Left . ParserError (TL.toStrict $ TLE.decodeUtf8 s) $ T.pack errorMessage
  Right response -> Right response

instance WS.WebSocketsData (Either ClientError HASSAuthResponse) where
  fromLazyByteString = parseEither
  toLazyByteString = undefined
  fromDataMessage (WS.Text lbs _) = parseEither lbs
  fromDataMessage (WS.Binary lbs) = parseEither lbs
