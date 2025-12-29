module Hasskell.Language.World
  ( World (..),
    ToggleState (..),
    toggle,
    ObservedEvent (..),
    ObservedWorld (..),
    collectCurrentState,
    updateWorld,
  )
where

import Control.Placeholder
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict qualified as HMap
import Data.Maybe (mapMaybe)
import Data.Time
import Effectful
import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Prettyprinter

-- | A distilled representation of entities and devices,
-- based on data from Home Assistant.
data World = MkWorld
  { worldToggleables :: HashMap EntityId ToggleState
  }
  deriving (Eq, Ord, Show)

-- | On or off.
data ToggleState = On | Off
  deriving (Eq, Ord, Show)

toggle :: ToggleState -> ToggleState
toggle On = Off
toggle Off = On

instance Pretty ToggleState where
  pretty On = "on"
  pretty Off = "off"

-- | Something that has happened that may affect the world state.
data ObservedEvent = StateChanged EntityId ToggleState
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
collectCurrentState :: (IOE :> es, HASS :> es) => Eff es ObservedWorld
collectCurrentState = do
  states <- getStates
  time <- liftIO $ do
    time <- getCurrentTime
    timeZone <- getTimeZone time
    let utcTimeOfDay = timeToTimeOfDay (utctDayTime time)
        (_dayOffset, timeOfDay) = localToUTCTimeOfDay timeZone utcTimeOfDay
    pure timeOfDay
  pure $
    MkObserved time $
      MkWorld
        { worldToggleables = HMap.fromList (filterToggleables states)
        }

filterToggleables :: [HASSState] -> [(EntityId, ToggleState)]
filterToggleables = mapMaybe $ \state -> do
  toggleState <- case stateState state of
    "on" -> pure On
    "off" -> pure Off
    _ -> Nothing

  pure $
    ( stateEntityId state,
      toggleState
    )

-- | Updates the state of the world with a new observation.
updateWorld :: ObservedWorld -> ObservedEvent -> ObservedWorld
updateWorld world@MkObserved {observedWorld = observedWorld@MkWorld {worldToggleables}} (StateChanged entity newState) =
  world
    { observedWorld =
        observedWorld
          { worldToggleables =
              HM.insert entity newState worldToggleables
          }
    }
