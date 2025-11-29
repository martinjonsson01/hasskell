{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Hasskell.Config (Config (..), LoggingConfig (..))
import Hasskell.HomeAssistant.Client qualified as HASS
import Import
import RIO.Text qualified as T
import System.IO.Error (userError)
import Text.Show.Pretty (ppShow)

run :: RIO App ()
run = do
  maybeToken <- hassToken <$> asks appOptions
  token <- maybe (throwIO $ userError "No token provided") pure maybeToken
  app <- ask
  let logging =
        Logging
          { debugLogger = \message -> runRIO app (logDebug $ display message),
            infoLogger = \message -> runRIO app (logInfo $ display message),
            errorLogger = \message -> runRIO app (logError $ display message)
          }
  result <-
    liftIO
      $ HASS.runClient
        ( Config
            { baseUrl = "localhost",
              token = token,
              logging = logging
            }
        )
        HASS.doHASSInteractions
  case result of
    Left clientError -> logError $ Utf8Builder $ encodeUtf8Builder $ T.pack $ ppShow clientError
    Right _ -> do
      logInfo "Application finished, exiting..."
