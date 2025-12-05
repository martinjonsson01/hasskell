module Hasskell.Language.Reconciler
  ( ReconciliationPlan (..),
    ReconciliationStep (..),
    isPlanEmpty,
    reconcile,
  )
where

import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hasskell.HomeAssistant.API
import Hasskell.Language.Interpreter
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
-- the observed world state into the desired world state.
reconcile :: ObservedWorld -> DesiredWorld -> ReconciliationPlan
reconcile (MkObserved _ observed) (MkDesired desired) =
  MkReconciliationPlan $
    if observed == desired
      then []
      else
        map
          TurnOnEntity
          ( findOffOnTransitions
              (createToggleMap observed)
              (createToggleMap desired)
          )

createToggleMap :: World -> Map EntityId ToggleState
createToggleMap (MkWorld {worldToggleables}) = Map.fromList $ map (\Toggleable {toggleableId, toggleableState} -> (toggleableId, toggleableState)) worldToggleables

findOffOnTransitions :: Map EntityId ToggleState -> Map EntityId ToggleState -> [EntityId]
findOffOnTransitions before after =
  Map.elems $
    Merge.merge
      Merge.dropMissing -- Ignore entities in `before` but not in `after`
      Merge.dropMissing -- Ignore entities in `after` but not in `before`
      (Merge.zipWithMaybeMatched toOffOnTransition) -- Keep only Off -> On transitions
      before
      after
  where
    toOffOnTransition :: EntityId -> ToggleState -> ToggleState -> Maybe EntityId
    toOffOnTransition entityId Off On = Just entityId
    toOffOnTransition _ _ _ = Nothing
