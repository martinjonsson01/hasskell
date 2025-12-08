module Hasskell.TestUtils.Utils
  ( runWithClient,
    shouldBeSubsetOf,
    sampleDeterministic,
  )
where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Hasskell.Config (Config (..), LoggingConfig (..))
import Hasskell.Effects.HASSConnection (HASSWebSocketError (ParserError))
import Hasskell.HomeAssistant.Client
import Hedgehog (Gen, Seed)
import Hedgehog.Internal.Gen qualified as InternalGen
import Hedgehog.Internal.Tree qualified as InternalTree
import System.Environment (lookupEnv)
import Test.Syd

sampleDeterministic :: Seed -> Gen a -> IO a
sampleDeterministic seed gen =
  let loop n =
        if n <= 0
          then
            expectationFailure "Hedgehog generator failed to produce a value"
          else do
            case InternalGen.evalGen 30 seed gen of
              Nothing ->
                loop (n - 1)
              Just x ->
                pure $ InternalTree.treeValue x
   in loop (100 :: Int)

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
      (Config {baseUrl = envBaseUrl, token = envApiToken, logging = logging, workingDir = Nothing})
      action

-- | Assert that the first list is a subset of the second list
shouldBeSubsetOf :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
as `shouldBeSubsetOf` bs = mapM_ (\a -> shouldSatisfyNamed bs ("should contain\n" <> ppShow a) (a `elem`)) as

infix 1 `shouldBeSubsetOf`
