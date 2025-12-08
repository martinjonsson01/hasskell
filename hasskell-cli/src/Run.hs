{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Hasskell
import Import
import RIO.FilePath
import System.Directory qualified as Dir
import System.IO.Error (userError)

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
  currentDir <- liftIO Dir.getCurrentDirectory
  liftIO
    $ runHasskell
      ( Config
          { baseUrl = "localhost",
            token = token,
            logging = logging,
            workingDir = Just $ currentDir </> "hasskell-cli"
          }
      )
    $ (policy "light is always on" (isOn $ toEntity ("light.flaktlampa" :: Text)))
