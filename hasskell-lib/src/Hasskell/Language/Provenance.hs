module Hasskell.Language.Provenance
  ( Reason (..),
    Observation (..),
    Derivation (..),
    -- Pretty-printing
    renderStep,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.FileSystem qualified as File
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.ReconciliationData
import Hasskell.Language.Report
import Prettyprinter

-- | Converts the given reason from a data representation into a pretty
-- user-presentable representation.
renderStep :: (MonadIO m) => ReconciliationStep -> m Text
renderStep = liftIO . runEff . File.runFileSystem . innerRenderStep

innerRenderStep :: (File.FileSystem :> es) => ReconciliationStep -> Eff es Text
innerRenderStep step = do
  let positions = extractLocations step
  baseDiagnostic <- loadReferencedFiles positions
  let fullDiagnostic = baseDiagnostic `addReport` (reportStep step)
  pure (renderInANSIColor fullDiagnostic)

reportStep :: ReconciliationStep -> Report PrettyStep
reportStep JustifyAction {stepAction, stepReason} = case stepReason of
  ReconciliationNeeded entity actual (JustifyObservation location desired _) ->
    Warn
      Nothing
      (PrettyAction stepAction)
      [(positionsPrimary location, This (PrettyDelta entity actual desired))]
      []

data PrettyStep
  = PrettyDelta EntityId Observation Observation
  | PrettyAction ReconciliationAction
  deriving (Eq, Ord, Show)

instance Pretty PrettyStep where
  pretty = \case
    PrettyDelta eId (StateObservation _ actual) (StateObservation _ desired) ->
      vsep
        [ "entity" <+> pretty eId <+> "is",
          indent 4 (pretty actual),
          "but should be",
          indent 4 (pretty desired)
        ]
    PrettyAction (TurnOnEntity entity) -> "turn on" <+> pretty entity
