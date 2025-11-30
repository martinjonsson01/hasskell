{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( App (..),
    Options (..),
    ColorMode (..),
  )
where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsColor :: !ColorMode,
    optionsHassToken :: !(Maybe Text)
  }

data ColorMode = ColorAuto | ColorAlways | ColorNever
  deriving (Eq, Show)

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
