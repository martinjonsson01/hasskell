{-# LANGUAGE UndecidableInstances #-}

module Hasskell.Monad
  ( HassM,
    MonadLog (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Reader.Has (Has (extract))
import Data.Text (Text)
import Hasskell.Config

type HassM env err m = ReaderT env (ExceptT err m)

--------------------------------------------------------------------------------

class MonadLog m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logError :: Text -> m ()

writeWithIOLogger :: (MonadReader env m, Has LoggingConfig env, MonadIO m) => (LoggingConfig -> Text -> IO ()) -> Text -> m ()
writeWithIOLogger getLogger text = do
  writeLog <- asks (getLogger . extract)
  liftIO $ writeLog text

instance (MonadReader env m, Has LoggingConfig env, MonadIO m) => MonadLog m where
  logDebug = writeWithIOLogger debugLogger
  logInfo = writeWithIOLogger infoLogger
  logError = writeWithIOLogger errorLogger

--------------------------------------------------------------------------------
