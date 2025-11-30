module Hasskell.HomeAssistant.ClientSpec (spec) where

import Data.Text qualified as T
import Hasskell.Config (Config (..), LoggingConfig (..))
import Hasskell.Effects.HASSConnection (HASSWebSocketError (CommandFailure, ParserError))
import Hasskell.HomeAssistant.API
import Hasskell.HomeAssistant.Client
import Hasskell.HomeAssistant.Client (ClientError (ClientWebSocketError))
import System.Environment (lookupEnv)
import Test.Syd

spec :: Spec
spec = do
  describe "HASS Client" $ do
    it "can get config" $ do
      config <- runWithClient getConfig
      configVersion config `shouldSatisfy` ("20" `T.isPrefixOf`)

    it "can get states" $ do
      states <- runWithClient getStates
      length (states) `shouldNotBe` 0

    it "can get entities" $ do
      entities <- runWithClient getEntities
      length (entities) `shouldNotBe` 0

    it "can get devices" $ do
      devices <- runWithClient getDevices
      length (devices) `shouldNotBe` 0

    it "can get services" $ do
      services <- runWithClient getServices
      length (services) `shouldNotBe` 0

runWithClient :: ClientM a -> IO a
runWithClient action = do
  maybeConfig <- runWithClient' action
  case maybeConfig of
    Left (ClientWebSocketError (ParserError source message)) -> expectationFailure $ T.unpack $ T.unlines ["JSON parsing failure:", source, "with error message:", message]
    Left err -> expectationFailure $ ppShow err
    Right value -> pure value

runWithClient' :: ClientM a -> IO (Either ClientError a)
runWithClient' action =
  liftIO $ do
    -- todo: Spawn an actual instance of Home Assistant to use with the test.
    envApiToken <- (T.pack . maybe (error "missing api token var") id) <$> (lookupEnv "HASSKELL_TEST_HASS_API_TOKEN")
    envBaseUrl <- (T.pack . maybe (error "missing url var") id) <$> (lookupEnv "HASSKELL_TEST_HASS_BASE_URL")
    let logging =
          Logging
            { debugLogger = \_ -> pure (), -- putStrLn . T.unpack,
              infoLogger = \_ -> pure (), -- putStrLn . T.unpack,
              errorLogger = putStrLn . T.unpack
            }
    runClient
      (Config {baseUrl = envBaseUrl, token = envApiToken, logging = logging})
      action
