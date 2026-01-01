module Hasskell.Language.WatcherSpec (spec) where

import Control.Concurrent.STM qualified as STM
import Data.List qualified as L
import Data.Maybe
import Hasskell.Language.AST
import Hasskell.Language.Watcher
import Hasskell.Language.World
import Hasskell.TestUtils.Gen
import Hasskell.TestUtils.MockHASS
import Hasskell.TestUtils.Specifications
import Hasskell.TestUtils.Utils
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Watcher" $ do
    specify "does not subscribe to anything for a noop spec" $
      property $ do
        let noop = policy "do nothing" nothing
        (_, executedCommands) <- liftIO $ recordConcurrentHASSCommands (watchStates noop)
        executedCommands === []

    specify "subscribes to light state changes" $
      property $ do
        state <- forAll genToggleState
        SomeToggleable entity (ObservedEntity eId _ _) <- forAll genEntity
        let lightPolicy = lightAlways state entity
        (_, executedCommands) <- liftIO $ recordConcurrentHASSCommands (watchStates lightPolicy)
        executedCommands === [EntitySubscribe eId anyHandler]

    setTimeout 1 $
      specify "adds state change events to queue" $
        property $ do
          state <- forAll genToggleState
          SomeToggleable entity (ObservedEntity eId _ _) <- forAll genEntity
          let lightPolicy = lightAlways state entity
          (eventQueue, executedCommands) <- liftIO $ recordConcurrentHASSCommands (watchStates lightPolicy)
          case fromJust $ L.uncons executedCommands of
            (EntitySubscribe _ (Handler triggerEvent), _) -> do
              event <- forAll (genStateChangeEvent eId Off On)
              liftIO $ STM.atomically $ triggerEvent event
              queuedEvents <- liftIO $ STM.atomically $ STM.readTBQueue eventQueue
              queuedEvents === event
            _ -> failure
