module HasskellSpec (spec) where

import Data.Text qualified as T
import Hasskell
import Hasskell.TestUtils.Specifications
import System.Environment
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  config <- liftIO getConfig

  describe "Hasskell" $ do
    it "runs specification without error" $ do
      let entity = inputBoolean "input_boolean.test"
      runHasskell config (lightAlwaysOn entity)

getConfig :: IO Config
getConfig = do
  let enableDebug = False
  -- todo: Spawn an actual instance of Home Assistant to use with the test.
  envApiToken <- (T.pack . maybe (error "missing api token var") id) <$> (lookupEnv "HASSKELL_TEST_HASS_API_TOKEN")
  envBaseUrl <- (T.pack . maybe (error "missing url var") id) <$> (lookupEnv "HASSKELL_TEST_HASS_BASE_URL")
  let logging =
        Logging
          { debugLogger = if enableDebug then putStrLn . T.unpack else pure . const (),
            infoLogger = putStrLn . T.unpack,
            errorLogger = putStrLn . T.unpack
          }
  pure (Config {baseUrl = envBaseUrl, token = envApiToken, logging = logging, workingDir = Nothing})
