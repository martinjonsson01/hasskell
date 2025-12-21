module Hasskell.Language.ReconciliationData
  ( ReconciliationPlan (..),
    isPlanEmpty,
    ReconciliationStep (..),
    ReconciliationAction (..),
    Reason (..),
    DerivedDesire (..),
    Observation (..),
    Derivation (..),
  )
where

import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.World

-- | A plan describing how to transform one world into another.
data ReconciliationPlan = MkReconciliationPlan [ReconciliationStep]
  deriving (Eq, Ord, Show)

isPlanEmpty :: ReconciliationPlan -> Bool
isPlanEmpty (MkReconciliationPlan steps) = null steps

-- | A step that can be taken to change the world, along with a motivation as to why.
data ReconciliationStep = JustifyAction
  { stepAction :: ReconciliationAction,
    stepReason :: Reason
  }
  deriving (Eq, Ord, Show)

instance HasPositions ReconciliationStep where
  extractPositions JustifyAction {stepReason} = extractPositions stepReason

-- | An action that can be taken to alter the world.
data ReconciliationAction = TurnOnEntity EntityId
  deriving (Eq, Ord, Show)

-- | Information about what caused a step to be generated.
data Reason = ReconciliationNeeded EntityId Observation DerivedDesire
  deriving (Eq, Ord, Show)

instance HasPositions Reason where
  extractPositions = \case
    ReconciliationNeeded _ _ deriv -> extractPositions deriv

-- | A desired world state that has been derived from some sequence of logical steps.
data DerivedDesire = JustifyObservation Positions Observation Derivation
  deriving (Eq, Ord, Show)

instance HasPositions DerivedDesire where
  extractPositions = \case
    JustifyObservation pos _ deriv -> pos : extractPositions deriv

-- | An observation about the world.
data Observation = StateObservation EntityId ToggleState
  deriving (Eq, Ord, Show)

-- | A derivation tree, giving reasons for each derivation step.
data Derivation
  = DeclaredState Positions EntityId ToggleState Derivation
  | DeclaredPolicy Policy
  deriving (Eq, Ord, Show)

instance HasPositions Derivation where
  extractPositions = \case
    DeclaredState location _ _ _ -> [location]
    DeclaredPolicy _ -> []
