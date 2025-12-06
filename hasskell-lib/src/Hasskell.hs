module Hasskell
  ( -- Main entrypoint
    runHasskell,
    -- Configuration
    Config (..),
    LoggingConfig (..),
    -- Language
    Specification,
    policy,
    isOn,
    IntoEntity (..),
    ToggleState (..),
  )
where

import Control.Monad.Except
import Data.Either.HT
import Hasskell.Config
import Hasskell.Effects.Logging
import Hasskell.HomeAssistant.Client
import Hasskell.Language.AST
import Hasskell.Language.Executor
import Hasskell.Language.Reconciler
import Hasskell.Language.World

-- | Executes the given specification against the configured Home Assistant instance.
runHasskell :: Config -> Specification -> IO ()
runHasskell config spec =
  runClient config (innerRunHasskell spec) >>= liftEither . mapLeft (userError . show)

innerRunHasskell :: Specification -> ClientM ()
innerRunHasskell spec = do
  observed <- collectCurrentState
  let (plan, report) = reconcile observed spec
  reportText <- renderReport report
  logInfo reportText
  _ <- executePlan plan
  pure ()
