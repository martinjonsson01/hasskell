module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep (..),
    ReconciliationAction (..),
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
import Data.Tuple.HT
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Diagnostic
import Hasskell.Language.Provenance
import Hasskell.Language.World

--------------------------------------------------------------------------------

-- | A plan describing how to transform one world into another.
data ReconciliationPlan = MkReconciliationPlan [ReconciliationStep]
  deriving (Eq, Ord, Show)

isPlanEmpty :: ReconciliationPlan -> Bool
isPlanEmpty (MkReconciliationPlan steps) = null steps

-- | A step that can be taken to change the world, along with a motivation as to why.
data ReconciliationStep = JustifyAction
  { stepAction :: ReconciliationAction,
    stepReason :: Explanation
  }
  deriving (Eq, Ord, Show)

instance HasLocations ReconciliationStep where
  extractLocations JustifyAction {stepReason} = extractLocations stepReason

-- | An action that can be taken to alter the world.
data ReconciliationAction = TurnOnEntity EntityId
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Computes the steps necessary to transform
-- the observed world state into the specified world state.
reconcile :: ObservedWorld -> Specification -> (ReconciliationPlan, ReconciliationReport)
reconcile observedWorld spec =
  let (unknowns, toTurnOn) = extractAllEntitiesToTurnOn spec observedWorld
      steps = turnOnEntities toTurnOn observedWorld
   in ( MkReconciliationPlan steps,
        reportFromList unknowns
      )

turnOnEntities :: [(Policy, Location, EntityId)] -> ObservedWorld -> [ReconciliationStep]
turnOnEntities toTurnOn (MkObserved _ world) =
  mapMaybe turnOn (worldToggleables world)
  where
    turnOn entity
      | Just (_, turnOnLoc, entityId) <- List.find ((== toggleableId entity) . thd3) toTurnOn =
          do
            pure $
              JustifyAction
                (TurnOnEntity entityId)
                ( Explain
                    (differs turnOnLoc entityId Off On)
                    [ Explain (observed entityId Off) [],
                      Explain (desired turnOnLoc entityId On) []
                    ]
                )
      | otherwise = Nothing

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> ([ReconciliationDiagnostic], [(Policy, Location, EntityId)])
extractAllEntitiesToTurnOn Specification {specPolicies} (MkObserved _ world) =
  partitionEithers $ concatMap (extractEntitiesToTurnOn entityMap) specPolicies
  where
    entityMap = worldToggleableMap world

extractEntitiesToTurnOn ::
  Map EntityId Toggleable ->
  Policy ->
  [Either ReconciliationDiagnostic (Policy, Location, EntityId)]
extractEntitiesToTurnOn entityMap onPolicy@(Policy _ (EIsOn (EEntity entityId :@ positions) :@ isOnLocation)) =
  maybe
    [Left (warnUnknownEntity positions entityId (Map.keys entityMap))]
    (\t -> [Right (onPolicy, isOnLocation, toggleableId t) | toggleableState t /= On])
    (Map.lookup entityId entityMap)

worldToggleableMap :: World -> Map EntityId Toggleable
worldToggleableMap world =
  Map.fromList
    [ (toggleableId t, t)
    | t <- worldToggleables world
    ]
