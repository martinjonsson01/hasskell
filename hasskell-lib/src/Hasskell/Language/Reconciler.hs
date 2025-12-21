module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep (..),
    ReconciliationAction (..),
    Reason (..),
    Observation (..),
    Derivation (..),
    isPlanEmpty,
    reconcile,
    -- Reports
    ReconciliationReport,
    hasWarnings,
    renderReport,
  )
where

import Data.Either
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Diagnostic
import Hasskell.Language.World

--------------------------------------------------------------------------------

-- | A plan describing how to transform one world into another.
data ReconciliationPlan = MkReconciliationPlan [ReconciliationStep]
  deriving (Eq, Ord, Show)

isPlanEmpty :: ReconciliationPlan -> Bool
isPlanEmpty (MkReconciliationPlan steps) = null steps

-- | Information about what caused a step to be generated.
data Reason = ReconciliationNeeded EntityId Observation Derivation
  deriving (Eq, Ord, Show)

-- | An observation about the world.
data Observation = StateObservation EntityId ToggleState
  deriving (Eq, Ord, Show)

-- | A derivation tree, giving reasons for each derivation step.
data Derivation
  = DeclaredState EntityId ToggleState Derivation
  | DeclaredPolicy Policy
  deriving (Eq, Ord, Show)

-- | A step that can be taken to change the world, along with a motivation as to why.
data ReconciliationStep = JustifyAction
  { stepAction :: ReconciliationAction,
    stepReason :: Reason
  }
  deriving (Eq, Ord, Show)

-- | An action that can be taken to alter the world.
data ReconciliationAction = TurnOnEntity EntityId
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Computes the steps necessary to transform
-- the observed world state into the specified world state.
reconcile :: ObservedWorld -> Specification -> (ReconciliationPlan, ReconciliationReport)
reconcile observed spec =
  let (unknowns, toTurnOn) = extractAllEntitiesToTurnOn spec observed
      steps = turnOnEntities toTurnOn observed
   in ( MkReconciliationPlan steps,
        reportFromList unknowns
      )

turnOnEntities :: [(Policy, EntityId)] -> ObservedWorld -> [ReconciliationStep]
turnOnEntities toTurnOn (MkObserved _ world) =
  mapMaybe turnOn (worldToggleables world)
  where
    turnOn entity
      | Just (turnOnPolicy, entityId) <- List.find ((== toggleableId entity) . snd) toTurnOn =
          do
            pure $
              JustifyAction
                (TurnOnEntity entityId)
                ( ReconciliationNeeded
                    entityId
                    (StateObservation entityId Off)
                    (DeclaredState entityId On (DeclaredPolicy turnOnPolicy))
                )
      | otherwise = Nothing

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> ([ReconciliationDiagnostic], [(Policy, EntityId)])
extractAllEntitiesToTurnOn Specification {specPolicies} (MkObserved _ world) =
  partitionEithers $ concatMap (extractEntitiesToTurnOn entityMap) specPolicies
  where
    entityMap = worldToggleableMap world

extractEntitiesToTurnOn ::
  Map EntityId Toggleable ->
  Policy ->
  [Either ReconciliationDiagnostic (Policy, EntityId)]
extractEntitiesToTurnOn entityMap onPolicy@(Policy _ (EIsOn _ (EEntity positions entityId))) =
  maybe
    [Left (warnUnknownEntity positions entityId (Map.keys entityMap))]
    (\t -> [Right (onPolicy, toggleableId t) | toggleableState t /= On])
    (Map.lookup entityId entityMap)

worldToggleableMap :: World -> Map EntityId Toggleable
worldToggleableMap world =
  Map.fromList
    [ (toggleableId t, t)
    | t <- worldToggleables world
    ]
