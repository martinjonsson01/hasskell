module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep (..),
    ReconciliationAction (..),
    Reason (..),
    Observation (..),
    DerivedDesire (..),
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
import Data.Tuple.HT
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Diagnostic
import Hasskell.Language.ReconciliationData
import Hasskell.Language.World

--------------------------------------------------------------------------------

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

turnOnEntities :: [(Policy, Positions, EntityId)] -> ObservedWorld -> [ReconciliationStep]
turnOnEntities toTurnOn (MkObserved _ world) =
  mapMaybe turnOn (worldToggleables world)
  where
    turnOn entity
      | Just (onPolicy, location, entityId) <- List.find ((== toggleableId entity) . thd3) toTurnOn =
          do
            pure $
              JustifyAction
                (TurnOnEntity entityId)
                ( ReconciliationNeeded
                    entityId
                    (StateObservation entityId Off)
                    ( JustifyObservation
                        location
                        (StateObservation entityId On)
                        (DeclaredState location entityId On (DeclaredPolicy onPolicy))
                    )
                )
      | otherwise = Nothing

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> ([ReconciliationDiagnostic], [(Policy, Positions, EntityId)])
extractAllEntitiesToTurnOn Specification {specPolicies} (MkObserved _ world) =
  partitionEithers $ concatMap (extractEntitiesToTurnOn entityMap) specPolicies
  where
    entityMap = worldToggleableMap world

extractEntitiesToTurnOn ::
  Map EntityId Toggleable ->
  Policy ->
  [Either ReconciliationDiagnostic (Policy, Positions, EntityId)]
extractEntitiesToTurnOn entityMap onPolicy@(Policy _ (EIsOn isOnPositions (EEntity positions entityId))) =
  maybe
    [Left (warnUnknownEntity positions entityId (Map.keys entityMap))]
    (\t -> [Right (onPolicy, isOnPositions, toggleableId t) | toggleableState t /= On])
    (Map.lookup entityId entityMap)

worldToggleableMap :: World -> Map EntityId Toggleable
worldToggleableMap world =
  Map.fromList
    [ (toggleableId t, t)
    | t <- worldToggleables world
    ]
