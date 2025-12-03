module Hasskell.TestUtils.Utils
  ( runWithClient,
    shouldBeSubsetOf,
  )
where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Hasskell.Config (Config (..), LoggingConfig (..))
import Hasskell.Effects.HASSConnection (HASSWebSocketError (ParserError))
import Hasskell.HomeAssistant.Client
import System.Environment (lookupEnv)
import Test.Syd

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

-- | Assert that the first list is a subset of the second list
shouldBeSubsetOf :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
as `shouldBeSubsetOf` bs = mapM_ (\a -> shouldSatisfyNamed bs ("should contain\n" <> ppShow a) (a `elem`)) as

infix 1 `shouldBeSubsetOf`
