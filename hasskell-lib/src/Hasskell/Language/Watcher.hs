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

-- | Watches for any relevant state changes that may be affected by the specification.
watchStates ::
  ( HASS :> es,
    Concurrent :> es,
    HasReferencedEntities e
  ) =>
  e ->
  Eff es (TBQueue HASSEvent)
watchStates toWatch = do
  eventQueue <- atomically $ newTBQueue 100
  let entities = referencedEntitiesIn toWatch
  forM_ entities $ \entity -> do
    subscribeToStateOf (makeKnownEntityIdUnsafe entity) (writeTBQueue eventQueue)
  pure eventQueue
