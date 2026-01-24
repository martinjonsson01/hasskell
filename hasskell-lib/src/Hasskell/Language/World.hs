module Hasskell.Language.World
  ( World (..),
    ToggleState (..),
    toggle,
    -- Observations
    ObservedChange (..),
    ObservedWorld (..),
    ObservedEntity (..),
    -- Operations
    collectCurrentState,
    lookupEntity,
    updateWorld,
    -- Classes
    HasKnownEntityId (..),
    HasCurrentState (..),
  )
where

import Control.Monad
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.Hashable
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Time
import Effectful
import GHC.Generics
import Hasskell.Effects.HASS
import Hasskell.Effects.Time
import Hasskell.HomeAssistant.API
import Hasskell.Language.Entity
import Prettyprinter

-- | A distilled representation of entities and devices,
-- based on data from Home Assistant.
data World = MkWorld
  { worldEntities :: HashMap KnownEntityId ObservedEntity
  }
  deriving (Eq, Ord, Show)

-- | On or off.
data ToggleState = On | Off
  deriving (Eq, Ord, Show, Generic, Hashable)

toggle :: ToggleState -> ToggleState
toggle On = Off
toggle Off = On

instance Pretty ToggleState where
  pretty On = "on"
  pretty Off = "off"

-- | Something that has happened that may affect the world state.
data ObservedChange = StateChanged KnownEntityId ToggleState
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | A recorded observation of a given world state.
data ObservedWorld = MkObserved
  { observedTimeOfDay :: TimeOfDay,
    observedWorld :: World
  }
  deriving (Eq, Ord, Show)

-- | Gathers information about the current state of the world from Home Assistant.
--
-- The returned `World` is a snapshot of the moment in time at which the function
-- was invoked.
collectCurrentState :: (Time :> es, HASS :> es, HasReferencedEntities eIds) => eIds -> Eff es ObservedWorld
collectCurrentState entitySource = do
  let interestingEntities = referencedEntitiesIn entitySource
  allStates <- getStates
  time <- getCurrentTimeOfDay
  let states = filterStates interestingEntities allStates
  supportedServices <- HM.fromList <$> mapM (getServicesOf) (toList interestingEntities)

  pure $
    MkObserved time $
      MkWorld
        { worldEntities = constructEntities states supportedServices
        }

getServicesOf ::
  (HASS :> es) =>
  EntityId ->
  Eff
    es
    (KnownEntityId, HashMap HASSDomain (HashSet HASSServiceName))
getServicesOf eId = (knownEId,) <$> getSupportedServicesOf knownEId
  where
    knownEId = makeKnownEntityIdUnsafe eId

constructEntities ::
  HashMap KnownEntityId ToggleState ->
  HashMap KnownEntityId (HashMap HASSDomain (HashSet HASSServiceName)) ->
  HashMap KnownEntityId ObservedEntity
constructEntities =
  HM.intersectionWithKey
    ( \eId state supportedServices ->
        ObservedEntity
          eId
          (HM.keysSet supportedServices)
          state
    )

filterStates :: Set EntityId -> [HASSState] -> HashMap KnownEntityId ToggleState
filterStates interestingEntities =
  HM.fromList
    . mapMaybe
      ( \state -> do
          toggleState <- case stateState state of
            "on" -> pure On
            "off" -> pure Off
            _ -> Nothing

          let eId = stateEntityId state
          unless (idOf eId `S.member` interestingEntities) $ Nothing

          pure $
            ( stateEntityId state,
              toggleState
            )
      )

-- | Updates the state of the world with a new observation.
updateWorld :: ObservedWorld -> ObservedChange -> ObservedWorld
updateWorld world@MkObserved {observedWorld = observedWorld@MkWorld {worldEntities}} (StateChanged eId newState) =
  world
    { observedWorld =
        observedWorld
          { worldEntities =
              HM.adjust (\entity -> entity {entityState = newState}) eId worldEntities
          }
    }

--------------------------------------------------------------------------------

-- | An entity that exists in the world.
data ObservedEntity = ObservedEntity
  { entityId :: KnownEntityId,
    entityDomains :: HashSet HASSDomain,
    entityState :: ToggleState
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

instance HasKnownEntityId ObservedEntity where
  knownIdOf (ObservedEntity eId _ _) = eId

-- | Things that have a known current state.
class HasCurrentState a where
  stateOf :: a -> ToggleState

instance HasCurrentState ObservedEntity where
  stateOf (ObservedEntity _ _ state) = state

-- | Finds an entity in the world, if it exists.
lookupEntity :: EntityId -> ObservedWorld -> Maybe ObservedEntity
lookupEntity eId observed =
  HM.lookup
    (makeKnownEntityIdUnsafe eId)
    (worldEntities (observedWorld observed))
