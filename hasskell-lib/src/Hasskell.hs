module Hasskell
  ( -- Main entrypoint
    runHasskell,
    runHasskellOnce,
    -- Configuration
    Config (..),
    LoggingConfig (..),
    -- Language
    Specification,
    policy,
    -- Entities
    HasEntityId (..),
    light,
    inputBoolean,
    -- Actions
    on,
    off,
    shouldBe,
    nothing,
    -- State
    ToggleState (..),
    fromState,
    toggledStateOf,
    -- Logic
    if_,
    then_,
    else_,
    -- Operators
    is,
    isGreaterThan,
    isGreaterOrEqualTo,
    isLessThan,
    isLessOrEqualTo,
    -- Time
    currentTime,
    time,
    -- Expression types
    ExprLight,
    ExprInputBoolean,
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Either.HT
import Data.Text qualified as T
import Effectful.Concurrent.STM
import Hasskell.Config
import Hasskell.Effects.Logging
import Hasskell.HomeAssistant.Client
import Hasskell.Language.AST
import Hasskell.Language.Executor
import Hasskell.Language.Reconciler
import Hasskell.Language.Report
import Hasskell.Language.Verifier
import Hasskell.Language.Watcher
import Hasskell.Language.World
import System.Directory qualified as Dir

--------------------------------------------------------------------------------

type ExprLight = Located (Exp Raw 'TEntityLight)

type ExprInputBoolean = Located (Exp Raw 'TEntityInputBoolean)

----------------------------------------------------------------------------------

-- | Executes the given specification against the configured Home Assistant instance once.
runHasskellOnce :: Config -> RawSpecification -> IO ()
runHasskellOnce = runHasskell' (void . innerRunHasskellOnce)

-- | Executes the given specification against the configured Home Assistant instance,
-- re-running on every relevant change in Home Assistant.
runHasskell :: Config -> RawSpecification -> IO ()
runHasskell = runHasskell' innerRunHasskellLoop

runHasskell' :: (RawSpecification -> ClientM ()) -> Config -> RawSpecification -> IO ()
runHasskell' inner config spec = do
  -- Need to set the working dir in order for call stack traces to be correctly
  -- captured (to be shown in diagnostics).
  case workingDir config of
    Just dir -> Dir.setCurrentDirectory dir
    Nothing -> pure ()
  runClient
    config
    (inner spec)
    >>= liftEither . mapLeft (userError . show)

innerRunHasskellOnce :: RawSpecification -> ClientM ObservedWorld
innerRunHasskellOnce spec = do
  initialWorld <- collectCurrentState spec
  syncWorld spec initialWorld
  pure initialWorld

innerRunHasskellLoop :: RawSpecification -> ClientM ()
innerRunHasskellLoop spec = do
  changeQueue <- watchStates spec
  initialWorld <- innerRunHasskellOnce spec
  reconciliationLoop changeQueue initialWorld spec

reconciliationLoop ::
  TBQueue ObservedChange ->
  ObservedWorld ->
  RawSpecification ->
  ClientM ()
reconciliationLoop changeQueue initialWorld spec = do
  let loop world = do
        -- Block until there's been a change.
        change <- atomically $ readTBQueue changeQueue
        let updatedWorld = updateWorld world change
        syncWorld spec updatedWorld
        loop updatedWorld
  loop initialWorld

syncWorld :: RawSpecification -> ObservedWorld -> ClientM ()
syncWorld spec world = do
  let (verifiedPlan, report) = verify world spec
      plan = reconcile world verifiedPlan
  reportText <- renderReport Rich report
  unless (T.null reportText) $ logWarn reportText
  renderedPlan <- renderPlanTrace Rich plan
  logDebug renderedPlan
  _ <- executePlan plan
  pure ()
