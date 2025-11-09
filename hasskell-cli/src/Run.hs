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
  liftIO $ HASS.run (Config {baseUrl = "localhost", token = token})
  logInfo "We're inside the application!"
