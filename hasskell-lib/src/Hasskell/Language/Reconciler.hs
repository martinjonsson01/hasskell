module Hasskell.Language.Reconciler
  ( ReconciliationPlan (..),
    ReconciliationStep (..),
    isPlanEmpty,
    reconcile,
  )
where

import Data.Maybe
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.World

-- | A plan describing how to transform one world into another.
data ReconciliationPlan = MkReconciliationPlan [ReconciliationStep]
  deriving (Eq, Show)

isPlanEmpty :: ReconciliationPlan -> Bool
isPlanEmpty (MkReconciliationPlan steps) = null steps

-- | A step that can be taken to alter the world.
data ReconciliationStep = TurnOnEntity EntityId
  deriving (Eq, Show)

-- | Computes the steps necessary to transform
-- the observed world state into the specified world state.
reconcile :: ObservedWorld -> Specification -> ReconciliationPlan
reconcile observed spec =
  MkReconciliationPlan $
    turnOnEntities (extractAllEntitiesToTurnOn spec observed) observed

turnOnEntities :: [EntityId] -> ObservedWorld -> [ReconciliationStep]
turnOnEntities toTurnOn (MkObserved _ world) =
  mapMaybe turnOn (worldToggleables world)
  where
    turnOn entity
      | toggleableId entity `elem` toTurnOn = pure $ TurnOnEntity (toggleableId entity)
      | otherwise = Nothing

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> [EntityId]
extractAllEntitiesToTurnOn Specification {specPolicies} world =
  concatMap (extractEntitiesToTurnOn world) specPolicies

extractEntitiesToTurnOn :: ObservedWorld -> Policy -> [EntityId]
extractEntitiesToTurnOn (MkObserved _ world) (Policy _ (SomeExp (EIsOn (EEntity entityId)))) =
  [ toggleableId
  | Toggleable {toggleableId, toggleableState} <- worldToggleables world,
    toggleableId == entityId,
    toggleableState /= On
  ]
extractEntitiesToTurnOn _ _ = []
