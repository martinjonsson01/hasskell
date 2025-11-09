{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hasskell.HomeAssistant.Client (run, HASSError, HASSAuthResponse) where

import Control.Exception (throwIO)
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

data HASSError = ParserError Text Text
  deriving (Generic, Eq, Show)

type HASS = ReaderT Config IO

run :: Config -> IO ()
run config@(Config {baseUrl}) = WS.runClient (T.unpack baseUrl) 80 "/api/websocket" (app config)

app :: Config -> WS.ClientApp ()
app config connection = do
  putStrLn "connecting..."
  runReaderT (connect connection) config
  putStrLn "connected!"

  putStrLn "exiting..."
  WS.sendClose connection ("Bye!" :: Text)

connect :: WS.Connection -> HASS ()
connect connection = do
  hassToken <- asks token
  initialMessage <- liftIO $ WS.receiveData connection
  case initialMessage of
    Left (ParserError source errorMessage) -> do
      liftIO $ TIO.putStrLn $ T.concat ["error: couldn't parse: ", source, "\ndue to: ", errorMessage]
      liftIO $ throwIO $ userError "Parsing error"
    Right response -> liftIO $ print (response :: HASSAuthResponse)

  liftIO $ send connection $ MessageAuth hassToken
  authMessage <- liftIO $ WS.receiveData connection
  case authMessage of
    Left (ParserError source errorMessage) -> do
      liftIO $ TIO.putStrLn $ T.concat ["error: couldn't parse: ", source, "\ndue to: ", errorMessage]
      liftIO $ throwIO $ userError "Parsing error"
    Right response -> liftIO $ print (response :: HASSAuthResponse)

send :: (WS.WebSocketsData a, ToJSON a) => WS.Connection -> a -> IO ()
send connection value = do
  BL8.putStrLn $ BL8.concat ["sending: ", encode value]
  WS.sendTextData connection value

parseEither :: BL.ByteString -> Either HASSError HASSAuthResponse
parseEither s = case eitherDecode s of
  Left errorMessage -> Left . ParserError (TL.toStrict $ TLE.decodeUtf8 s) $ T.pack errorMessage
  Right response -> Right response

instance WS.WebSocketsData (Either HASSError HASSAuthResponse) where
  fromLazyByteString = parseEither
  toLazyByteString = undefined
  fromDataMessage (WS.Text lbs _) = parseEither lbs
  fromDataMessage (WS.Binary lbs) = parseEither lbs
