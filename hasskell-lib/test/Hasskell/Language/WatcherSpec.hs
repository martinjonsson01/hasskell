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
      specify "adds state changes to queue" $
        property $ do
          state <- forAll genToggleState
          SomeToggleable entity (ObservedEntity eId _ _) <- forAll genEntity
          let lightPolicy = lightAlways state entity
          (changeQueue, executedCommands) <- liftIO $ recordConcurrentHASSCommands (watchStates lightPolicy)
          case fromJust $ L.uncons executedCommands of
            (EntitySubscribe _ (Handler triggerChange), _) -> do
              change <- forAll (genStateChange eId Off On)
              observedChange <- forAll (genObservedChange eId On)
              liftIO $ STM.atomically $ triggerChange change
              queuedChanges <- liftIO $ STM.atomically $ STM.readTBQueue changeQueue
              queuedChanges === observedChange
            _ -> failure
