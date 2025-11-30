{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Hasskell.Config (Config (..), LoggingConfig (..))
import Hasskell.HomeAssistant.API (HASSEntity (..))
import Hasskell.HomeAssistant.Client (HASSDomain (..))
import Hasskell.HomeAssistant.Client qualified as HASS
import Import
import RIO.Map qualified as Map
import RIO.Text qualified as T
import System.IO.Error (userError)
import Text.Show.Pretty (pPrint, ppShow)

run :: RIO App ()
run = do
  maybeToken <- optionsHassToken <$> asks appOptions
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
      $ do
        -- result <- HASS.callService (Domain "light") (ServiceName "toggle") "light.flaktlampa"
        -- services <- HASS.getServices
        -- liftIO $ pPrint (Map.filterWithKey (\(Domain domain) _ -> domain == "light") services)
        entities <- HASS.getEntities
        liftIO $ pPrint (map entityEntityId entities)
  case result of
    Left clientError -> logError $ Utf8Builder $ encodeUtf8Builder $ T.pack $ ppShow clientError
    Right _ -> do
      logInfo "Application finished, exiting..."
