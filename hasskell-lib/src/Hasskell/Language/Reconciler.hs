module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep (..),
    ReconciliationAction (..),
    isPlanEmpty,
    reconcile,
    -- Reports
    ReconciliationReport,
    hasWarnings,
    renderReport,
    -- Traces
    renderPlanTrace,
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Text (Text)
import Effectful
import Effectful.FileSystem qualified as File
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.Diagnostic
import Hasskell.Language.Provenance
import Hasskell.Language.Report
import Hasskell.Language.World
import Prettyprinter
import Prettyprinter.Render.Terminal

--------------------------------------------------------------------------------

-- | A plan describing how to transform one world into another.
data ReconciliationPlan = MkReconciliationPlan [ReconciliationStep]
  deriving (Eq, Ord, Show)

isPlanEmpty :: ReconciliationPlan -> Bool
isPlanEmpty (MkReconciliationPlan steps) = null steps

-- | A step that can be taken to change the world, along with a motivation as to why.
data ReconciliationStep = JustifyAction
  { stepAction :: ReconciliationAction,
    stepReason :: Explanation
  }
  deriving (Eq, Ord, Show)

instance HasLocations ReconciliationStep where
  extractLocations JustifyAction {stepReason} = extractLocations stepReason

-- | An action that can be taken to alter the world.
data ReconciliationAction = SetEntityState EntityId ToggleState
  deriving (Eq, Ord, Show)

-- | Pretty-prints the plan, displaying justification for each step.
renderPlanTrace :: (MonadIO m) => ReconciliationPlan -> m Text
renderPlanTrace = liftIO . runEff . File.runFileSystem . innerRenderPlanTrace

innerRenderPlanTrace :: (File.FileSystem :> es) => ReconciliationPlan -> Eff es Text
innerRenderPlanTrace (MkReconciliationPlan steps) = do
  prettySteps <- mapM prettifyStep steps
  let doc = vsep prettySteps
      rendered = layoutDoc doc
      cleaned = cleanExplanation rendered
  pure cleaned

prettifyStep :: (File.FileSystem :> es) => ReconciliationStep -> Eff es (Doc AnsiStyle)
prettifyStep JustifyAction {stepAction, stepReason} = do
  prettyReason <- prettifyExplanation stepReason
  pure
    (vsep ["will" <+> pretty stepAction, "reason:", indent 4 prettyReason])

instance Pretty ReconciliationAction where
  pretty = \case
    SetEntityState eId state -> "turn entity" <+> pretty eId <+> pretty state

--------------------------------------------------------------------------------

-- | Computes the steps necessary to transform
-- the observed world state into the specified world state.
reconcile :: ObservedWorld -> Specification -> (ReconciliationPlan, ReconciliationReport)
reconcile (MkObserved _ (MkWorld {worldToggleables})) spec =
  let desiredToggleables = HMap.unions (map computeDesiredStates (specPolicies spec))
      unlocatedDesiredToggleables = HMap.mapKeys stripLocation desiredToggleables
      warnings =
        warnOfUnknownEntities
          worldToggleables
          unlocatedDesiredToggleables
          desiredToggleables
      needsToggling = reconcileToggleables worldToggleables unlocatedDesiredToggleables
      steps = generateToggleSteps needsToggling
   in (MkReconciliationPlan steps, reportFromList warnings)

type Detailed a = Explained (Located a)

generateToggleSteps :: HashMap EntityId (Detailed ToggleState) -> [ReconciliationStep]
generateToggleSteps = map toReconciliationStep . HMap.toList
  where
    toReconciliationStep :: (EntityId, Detailed ToggleState) -> ReconciliationStep
    toReconciliationStep (eId, (state :@ _) :£ explanation) =
      JustifyAction
        { stepAction = SetEntityState eId state,
          stepReason = explanation
        }

reconcileToggleables ::
  HashMap EntityId ToggleState ->
  HashMap EntityId (Detailed ToggleState) ->
  HashMap EntityId (Detailed ToggleState)
reconcileToggleables worldToggleables desiredToggleables =
  desiredToggleables `reconcileStates` worldToggleables
  where
    reconcileStates = HMap.differenceWithKey dropIfStatesEqual
    dropIfStatesEqual eId explained@((expected :@ _) :£ _) actual
      | expected /= actual =
          Just
            ( explained
                `elaborate` differs eId expected actual
                `becauseMore` observed eId actual
            )
      | otherwise = Nothing

warnOfUnknownEntities ::
  HashMap EntityId w ->
  HashMap EntityId v ->
  HashMap (Located EntityId) v ->
  [ReconciliationDiagnostic]
warnOfUnknownEntities worldToggleables unlocatedDesiredToggleables desiredToggleables =
  let unlocatedUnknownEntities = unlocatedDesiredToggleables `HMap.difference` worldToggleables
      unknownEntities =
        filter
          ((`HMap.member` unlocatedUnknownEntities) . stripLocation)
          (HMap.keys desiredToggleables)
      knownEntities = HMap.keys worldToggleables
   in map (warnUnknownEntity knownEntities) unknownEntities

computeDesiredStates :: Policy -> HashMap (Located EntityId) (Detailed ToggleState)
computeDesiredStates Policy {expression} = HMap.fromList [computeDesiredState expression]

computeDesiredState :: Located (Exp 'TVoid) -> (Located EntityId, Detailed ToggleState)
computeDesiredState = \case
  (EShouldBe (EEntity eId :@ eLoc) state) :@ loc ->
    ( eId :@ eLoc,
      (state :@ loc) `because` desired loc eId state
    )
