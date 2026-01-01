module Hasskell
  ( -- Main entrypoint
    runHasskell,
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
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Either.HT
import Data.Text qualified as T
import Hasskell.Config
import Hasskell.Effects.Logging
import Hasskell.HomeAssistant.Client
import Hasskell.Language.AST
import Hasskell.Language.Executor
import Hasskell.Language.Reconciler
import Hasskell.Language.Report
import Hasskell.Language.Verifier
import Hasskell.Language.World
import System.Directory qualified as Dir

--------------------------------------------------------------------------------

type ExprLight = Located (Exp Raw 'TEntityLight)

----------------------------------------------------------------------------------

-- | Executes the given specification against the configured Home Assistant instance.
runHasskell :: Config -> RawSpecification -> IO ()
runHasskell config spec = do
  -- Need to set the working dir in order for call stack traces to be correctly
  -- captured (to be shown in diagnostics).
  case workingDir config of
    Just dir -> Dir.setCurrentDirectory dir
    Nothing -> pure ()
  runClient
    config
    (innerRunHasskell spec)
    >>= liftEither . mapLeft (userError . show)

innerRunHasskell :: RawSpecification -> ClientM ()
innerRunHasskell spec = do
  observed <- collectCurrentState spec
  let (verifiedPlan, report) = verify observed spec
      plan = reconcile observed verifiedPlan
  reportText <- renderReport Rich report
  unless (T.null reportText) $ logWarn reportText
  renderedPlan <- renderPlanTrace Rich plan
  logDebug renderedPlan
  _ <- executePlan plan
  pure ()
