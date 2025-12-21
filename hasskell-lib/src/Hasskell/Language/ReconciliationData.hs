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

instance HasLocations ReconciliationStep where
  extractLocations JustifyAction {stepReason} = extractLocations stepReason

-- | An action that can be taken to alter the world.
data ReconciliationAction = TurnOnEntity EntityId
  deriving (Eq, Ord, Show)

-- | Information about what caused a step to be generated.
data Reason = ReconciliationNeeded EntityId Observation DerivedDesire
  deriving (Eq, Ord, Show)

instance HasLocations Reason where
  extractLocations = \case
    ReconciliationNeeded _ _ deriv -> extractLocations deriv

-- | A desired world state that has been derived from some sequence of logical steps.
data DerivedDesire = JustifyObservation Location Observation Derivation
  deriving (Eq, Ord, Show)

instance HasLocations DerivedDesire where
  extractLocations = \case
    JustifyObservation pos _ deriv -> pos : extractLocations deriv

-- | An observation about the world.
data Observation = StateObservation EntityId ToggleState
  deriving (Eq, Ord, Show)

-- | A derivation tree, giving reasons for each derivation step.
data Derivation
  = DeclaredState Location EntityId ToggleState Derivation
  | DeclaredPolicy Policy
  deriving (Eq, Ord, Show)

instance HasLocations Derivation where
  extractLocations = \case
    DeclaredState location _ _ _ -> [location]
    DeclaredPolicy _ -> []
