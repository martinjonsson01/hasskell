{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Hasskell.Config (Config (..), LoggingConfig (..))
import Hasskell.Effects.HASS qualified as HASS
import Hasskell.HomeAssistant.API (HASSEntity (..))
import Hasskell.HomeAssistant.Client
import Import
import RIO.List (find)
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
      $ runClient
        ( Config
            { baseUrl = "localhost",
              token = token,
              logging = logging
            }
        )
      $ do
        entities <- HASS.getEntities
        let mbLight = entityEntityId <$> find (\entity -> show (entityEntityId entity) == "EntityId \"light.flaktlampa\"") entities
        case mbLight of
          Just light -> do
            result <- HASS.callService (Domain "light") (ServiceName "toggle") light
            liftIO $ pPrint result
          Nothing -> liftIO $ pPrint ("no light :(" :: Text)
  case result of
    Left clientError -> logError $ Utf8Builder $ encodeUtf8Builder $ T.pack $ ppShow clientError
    Right _ -> do
      logInfo "Application finished, exiting..."
