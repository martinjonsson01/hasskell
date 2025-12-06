module Hasskell.Language.Executor
  ( ExecutionReport (..),
    executePlan,
  )
where

import Effectful
import Hasskell.Effects.HASS
import Hasskell.Language.Reconciler

-- | A summary of how the reconciliation went.
data ExecutionReport = MkExecutionReport ()
  deriving (Eq, Show)

-- | Performs the steps described in the execution plan,
-- reporting back how it went.
executePlan :: (HASS :> es) => ReconciliationPlan -> Eff es ExecutionReport
executePlan (MkReconciliationPlan steps) = do
  mapM_ executeStep steps
  pure $ MkExecutionReport ()

executeStep :: (HASS :> es) => ReconciliationStep -> Eff es ()
executeStep = \case
  TurnOnEntity entity -> turnOnLight entity
