module Hasskell.Language.Watcher
  ( watchStates,
  )
where

import Control.Monad
import Effectful
import Effectful.Concurrent.STM
import Hasskell.Effects.HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.World

-- | Watches for any relevant state changes that may be affected by the specification.
watchStates ::
  ( HASS :> es,
    Concurrent :> es,
    HasReferencedEntities e
  ) =>
  e ->
  Eff es (TBQueue ObservedChange)
watchStates toWatch = do
  changeQueue <- atomically $ newTBQueue 100
  let entities = referencedEntitiesIn toWatch
  forM_ entities $ \entity -> do
    subscribeToStateOf
      (makeKnownEntityIdUnsafe entity)
      (nothingToNoop (writeTBQueue changeQueue) . parseChange)
  pure changeQueue

nothingToNoop :: (Applicative m) => (a -> m ()) -> Maybe a -> m ()
nothingToNoop f = maybe (pure ()) f

parseChange :: HASSChange -> Maybe ObservedChange
parseChange change = do
  let triggered = variablesTrigger (changeVariables change)
      eId = triggeredEntityId triggered
      toState = stateState (triggeredToState triggered)
  newState <- parseState toState
  pure (StateChanged eId newState)

parseState :: HASSStateValue -> Maybe ToggleState
parseState state = case state of
  "on" -> pure On
  "off" -> pure Off
  _ -> mzero
