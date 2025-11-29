{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.Profiling
  ( -- Profiling
    Profiling,
    profile,
    runProfiling,
    runNoProfiling,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import GHC.Clock (getMonotonicTime)

data Profiling :: Effect where
  Profile :: Text -> m a -> Profiling m a

makeEffect ''Profiling

runProfiling :: (IOE :> es) => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action -> localSeqUnliftIO env $ \unlift -> do
    t1 <- liftIO getMonotonicTime
    result <- unlift action
    t2 <- liftIO getMonotonicTime
    liftIO . putStrLn . T.unpack $ mconcat ["Action '", label, "' took ", T.show (t2 - t1), " seconds."]
    pure result

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
  Profile _ action -> localSeqUnlift env $ \unlift -> unlift action
