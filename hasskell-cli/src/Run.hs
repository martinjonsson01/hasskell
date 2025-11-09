{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Hasskell.Config
import Hasskell.HomeAssistant.Client qualified as HASS
import Import
import System.IO.Error (userError)

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
        HASS.startWebSocket
  case result of
    Left clientError -> logError $ displayShow clientError
    Right _ -> do
      logInfo "We're inside the application!"
