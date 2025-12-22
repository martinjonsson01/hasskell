module Hasskell.Language.ExecutorSpec (spec) where

import Data.Aeson
import Data.Map.Lazy qualified as M
import Data.Maybe
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Hasskell.Effects.HASS (HASS)
import Hasskell.Effects.HASS qualified as HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.Executor
import Hasskell.Language.Reconciler
import Hasskell.Language.World
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
        let entityId = EntityId "entity"
        let plan = MkReconciliationPlan [JustifyAction (SetEntityState entityId On) undefined]
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === [TurnOnLight entityId]

    specify "turns off light" $
      property $ do
        let entityId = EntityId "entity"
        let plan = MkReconciliationPlan [JustifyAction (SetEntityState entityId Off) undefined]
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === [TurnOffLight entityId]

data HASSOp = Unknown | TurnOnLight EntityId | TurnOffLight EntityId
  deriving (Eq, Show)

recordHASSCommands :: Eff '[HASS] a -> (a, [HASSOp])
recordHASSCommands = runPureEff . runWithFakeHASS

runWithFakeHASS :: Eff (HASS : es) a -> Eff es (a, [HASSOp])
runWithFakeHASS = reinterpret_ (runState []) $ \action -> case action of
  HASS.GetConfig -> do
    modify $ (Unknown :)
    pure $ fromJust $ decode ""
  HASS.GetStates -> do
    modify $ (Unknown :)
    pure []
  HASS.GetEntities -> do
    modify $ (Unknown :)
    pure []
  HASS.GetDevices -> do
    modify $ (Unknown :)
    pure []
  HASS.GetServices -> do
    modify $ (Unknown :)
    pure M.empty
  HASS.TurnOnLight entity -> do
    modify $ (TurnOnLight entity :)
    pure ()
  HASS.TurnOffLight entity -> do
    modify $ (TurnOffLight entity :)
    pure ()
