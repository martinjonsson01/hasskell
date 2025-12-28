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
import Data.Time
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Fmt
import GHC.Clock (getMonotonicTime)
import Hasskell.Effects.Logging

data Profiling :: Effect where
  Profile :: Text -> m a -> Profiling m a

makeEffect ''Profiling

runProfiling :: (IOE :> es, Logger :> es) => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action -> do
    (result, elapsedTime) <- localSeqUnliftIO env $ \unlift -> do
      t1 <- liftIO getMonotonicTime
      result <- unlift action
      t2 <- liftIO getMonotonicTime
      pure (result, prettyDiff t2 t1)
    logDebug $ mconcat ["PROFILE: ", label, ": ", elapsedTime]
    pure result

prettyDiff :: Double -> Double -> Text
prettyDiff t1 t2 =
  let dt = abs (t2 - t1)
      diff = realToFrac dt :: DiffTime
   in fmt (diffF False diff)

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
  Profile _ action -> localSeqUnlift env $ \unlift -> unlift action
