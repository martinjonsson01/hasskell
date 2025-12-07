module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep (..),
    isPlanEmpty,
    reconcile,
    -- Reports
    ReconciliationReport,
    hasWarnings,
    renderReport,
  )
where

import Data.Either
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

-- | A step that can be taken to alter the world.
data ReconciliationStep = TurnOnEntity EntityId
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

turnOnEntities :: [EntityId] -> ObservedWorld -> [ReconciliationStep]
turnOnEntities toTurnOn (MkObserved _ world) =
  mapMaybe turnOn (worldToggleables world)
  where
    turnOn entity
      | toggleableId entity `elem` toTurnOn = pure $ TurnOnEntity (toggleableId entity)
      | otherwise = Nothing

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> ([ReconciliationDiagnostic], [EntityId])
extractAllEntitiesToTurnOn Specification {specPolicies} (MkObserved _ world) =
  partitionEithers $ concatMap (extractEntitiesToTurnOn entityMap) specPolicies
  where
    entityMap = worldToggleableMap world

extractEntitiesToTurnOn ::
  Map EntityId Toggleable ->
  Policy ->
  [Either ReconciliationDiagnostic EntityId]
extractEntitiesToTurnOn entityMap (Policy _ (SomeExp (EIsOn _ (EEntity entityBlame entityId)))) =
  maybe
    [Left (warnUnknownEntity entityBlame entityId)]
    (\t -> [Right (toggleableId t) | toggleableState t /= On])
    (Map.lookup entityId entityMap)
extractEntitiesToTurnOn _ _ = []

worldToggleableMap :: World -> Map EntityId Toggleable
worldToggleableMap world =
  Map.fromList
    [ (toggleableId t, t)
    | t <- worldToggleables world
    ]
