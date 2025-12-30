module Hasskell.Language.ExecutorSpec (spec) where

import Control.Placeholder
import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Map.Lazy qualified as M
import Data.Maybe
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Hasskell.Effects.HASS (HASS)
import Hasskell.Effects.HASS qualified as HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Executor
import Hasskell.Language.Provenance
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
        let entity = light "entity"
        let entityId = idOf entity
        let plan = MkReconciliationPlan [SetEntityState entityId domainLight On `because` NoExplanation]
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === [TurnOn entityId]

    specify "turns off light" $
      property $ do
        let entity = light "entity"
        let entityId = idOf entity
        let plan = MkReconciliationPlan [SetEntityState entityId domainLight Off `because` NoExplanation]
        let (_, executedCommands) = recordHASSCommands (executePlan plan)
        executedCommands === [TurnOff entityId]

data HASSOp
  = Unknown
  | TurnOn EntityId
  | TurnOff EntityId
  | EntitySubscribe EntityId
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
  HASS.GetSupportedServicesOf _ -> do
    modify $ (Unknown :)
    pure HM.empty
  HASS.TurnOn _ entity -> do
    modify $ (TurnOn entity :)
    pure ()
  HASS.TurnOff _ entity -> do
    modify $ (TurnOff entity :)
    pure ()
  HASS.SubscribeToStateOf entity _ -> do
    modify (EntitySubscribe entity :)
    pure ()

data NoExplanation = NoExplanation

instance IntoExplanation NoExplanation where
  toExplanation NoExplanation = unimplemented
