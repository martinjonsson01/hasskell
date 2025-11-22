{-# LANGUAGE UndecidableInstances #-}

module Hasskell.Monad
  ( HassM,
    HasConfig (..),
    MonadLog (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Hasskell.Config

type HassM env err m = ReaderT env (ExceptT err m)

--------------------------------------------------------------------------------

class HasConfig a where
  getConfig :: a -> Config

instance HasConfig Config where
  getConfig = id

--------------------------------------------------------------------------------

class MonadLog m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logError :: Text -> m ()

writeWithIOLogger :: (MonadReader env m, HasConfig env, MonadIO m) => (LoggingConfig -> Text -> IO ()) -> Text -> m ()
writeWithIOLogger getLogger text = do
  writeLog <- asks (getLogger . logging . getConfig)
  liftIO $ writeLog text

instance (MonadReader env m, HasConfig env, MonadIO m) => MonadLog m where
  logDebug = writeWithIOLogger debugLogger
  logInfo = writeWithIOLogger infoLogger
  logError = writeWithIOLogger errorLogger

--------------------------------------------------------------------------------
