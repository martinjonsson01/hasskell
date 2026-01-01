module Hasskell.Language.ExecutorSpec (spec) where

import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Executor
import Hasskell.Language.Provenance
import Hasskell.Language.Reconciler
import Hasskell.Language.World
import Hasskell.TestUtils.MockHASS
import Hedgehog
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Executor" $ do
    specify "can handle a noop plan" $
      property $ do
        let plan = MkReconciliationPlan []
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === []

    specify "turns on light" $
      property $ do
        let entity = light "entity"
        let entityId = makeKnownEntityIdUnsafe (idOf entity)
        let plan = MkReconciliationPlan [SetEntityState entityId domainLight On `because` NoExplanation]
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === [TurnOn entityId]

    specify "turns off light" $
      property $ do
        let entity = light "entity"
        let entityId = makeKnownEntityIdUnsafe (idOf entity)
        let plan = MkReconciliationPlan [SetEntityState entityId domainLight Off `because` NoExplanation]
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === [TurnOff entityId]
