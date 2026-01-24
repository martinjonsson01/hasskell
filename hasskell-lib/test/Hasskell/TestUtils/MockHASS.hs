module Hasskell.TestUtils.MockHASS
  ( HASSOp (..),
    recordHASSCommands,
    recordConcurrentHASSCommands,
    NoExplanation (..),
    TestStateChangeHandler (..),
    anyHandler,
  )
where

import Control.Placeholder
import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Data.Maybe
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Hasskell.Effects.HASS (HASS)
import Hasskell.Effects.HASS qualified as HASS
import Hasskell.HomeAssistant.API
import Hasskell.Language.Provenance

data HASSOp
  = Unknown
  | TurnOn KnownEntityId
  | TurnOff KnownEntityId
  | EntitySubscribe KnownEntityId TestStateChangeHandler
  deriving (Eq, Show)

newtype TestStateChangeHandler = Handler HASS.StateChangeHandler

instance Eq TestStateChangeHandler where
  (Handler _) == (Handler _) = True

instance Show TestStateChangeHandler where
  show _ = "Some state change handler"

recordConcurrentHASSCommands :: Eff '[HASS, Concurrent, IOE] a -> IO (a, [HASSOp])
recordConcurrentHASSCommands = runEff . runConcurrent . runWithFakeHASS

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
  HASS.GetStateHistoryOf _ _ -> do
    modify $ (Unknown :)
    pure HM.empty
  HASS.TurnOn _ entity -> do
    modify $ (TurnOn entity :)
    pure ()
  HASS.TurnOff _ entity -> do
    modify $ (TurnOff entity :)
    pure ()
  HASS.SubscribeToStateOf entity handler -> do
    modify (EntitySubscribe entity (Handler handler) :)
    pure ()

anyHandler :: TestStateChangeHandler
anyHandler = Handler $ const (pure ())

data NoExplanation = NoExplanation

instance IntoExplanation NoExplanation where
  toExplanation NoExplanation = unimplemented
