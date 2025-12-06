module Hasskell.Language.Interpreter
  ( interpret,
    DesiredWorld (..),
  )
where

import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.World

-- | Describes the state of an ideal world.
newtype DesiredWorld = MkDesired World
  deriving (Eq, Show)

-- | Derives a desired world state from an observed one,
-- given a certain specification.
interpret :: Specification -> ObservedWorld -> DesiredWorld
interpret spec observed =
  turnOnEntities
    (extractAllEntitiesToTurnOn spec observed)
    observed

turnOnEntities :: [EntityId] -> ObservedWorld -> DesiredWorld
turnOnEntities toTurnOn (MkObserved _ world) =
  MkDesired $
    world
      { worldToggleables = map turnOn (worldToggleables world)
      }
  where
    turnOn entity
      | toggleableId entity `elem` toTurnOn = entity {toggleableState = On}
      | otherwise = entity

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> [EntityId]
extractAllEntitiesToTurnOn Specification {specPolicies} world =
  concatMap (extractEntitiesToTurnOn world) specPolicies

extractEntitiesToTurnOn :: ObservedWorld -> Policy -> [EntityId]
extractEntitiesToTurnOn (MkObserved _ world) (Policy _ (SomeExp (EIsOn (EEntity entityId)))) =
  filter (== entityId) $ map toggleableId $ worldToggleables world
extractEntitiesToTurnOn _ _ = []
