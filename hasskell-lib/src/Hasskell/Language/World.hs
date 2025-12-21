module Hasskell.Language.World
  ( World (..),
    ToggleState (..),
    Toggleable (..),
    ObservedWorld (..),
    collectCurrentState,
  )
where

import Data.Maybe (mapMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Effectful
import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Prettyprinter

-- | A distilled representation of entities and devices,
-- based on data from Home Assistant.
data World = MkWorld
  { worldToggleables :: [Toggleable]
  }
  deriving (Eq, Ord, Show)

-- | On or off.
data ToggleState = On | Off
  deriving (Eq, Ord, Show)

instance Pretty ToggleState where
  pretty On = "on"
  pretty Off = "off"

-- | Something that can be toggled on or off, like a light switch or a relay.
data Toggleable = Toggleable
  { toggleableId :: EntityId,
    toggleableState :: ToggleState
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | A recorded observation of a given world state.
data ObservedWorld = MkObserved UTCTime World
  deriving (Eq, Ord, Show)

-- | Gathers information about the current state of the world from Home Assistant.
--
-- The returned `World` is a snapshot of the moment in time at which the function
-- was invoked.
collectCurrentState :: (IOE :> es, HASS :> es) => Eff es ObservedWorld
collectCurrentState = do
  states <- getStates
  time <- liftIO $ getCurrentTime
  pure $
    MkObserved time $
      MkWorld
        { worldToggleables = filterToggleables states
        }

filterToggleables :: [HASSState] -> [Toggleable]
filterToggleables = mapMaybe $ \state -> do
  toggleState <- case stateState state of
    "on" -> pure On
    "off" -> pure Off
    _ -> Nothing

  pure $
    Toggleable
      { toggleableId = stateEntityId state,
        toggleableState = toggleState
      }
