module Hasskell
  ( -- Main entrypoint
    runHasskell,
    -- Configuration
    Config (..),
    LoggingConfig (..),
    -- Language
    policy,
    isOn,
    IntoEntity (..),
    ToggleState (..),
  )
where

import Control.Monad.Except
import Data.Either.HT
import Data.Text qualified as T
import Hasskell.Config
import Hasskell.Effects.Logging
import Hasskell.HomeAssistant.Client
import Hasskell.Language.AST
import Hasskell.Language.Executor
import Hasskell.Language.Reconciler
import Hasskell.Language.World
import Text.Show.Pretty

-- | Executes the given specification against the configured Home Assistant instance.
runHasskell :: Config -> Specification -> IO ()
runHasskell config spec =
  runClient config (innerRunHasskell spec) >>= liftEither . mapLeft (userError . show)

innerRunHasskell :: Specification -> ClientM ()
innerRunHasskell spec = do
  observed <- collectCurrentState
  let plan = reconcile observed spec
  logInfo $
    if isPlanEmpty plan
      then "nothing to reconcile"
      else
        "plan is: " <> (T.pack $ ppShow plan)
  _ <- executePlan plan
  pure ()
