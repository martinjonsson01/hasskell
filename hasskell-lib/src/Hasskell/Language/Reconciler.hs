module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep,
    stepAction,
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

import Control.Applicative
import Data.HashMap.Strict qualified as HMap
import Data.Kind
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem qualified as File
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import Effectful.Writer.Static.Local (Writer)
import Effectful.Writer.Static.Local qualified as Writer
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
type ReconciliationStep = Explained ReconciliationAction

stepAction :: ReconciliationStep -> ReconciliationAction
stepAction (action :£ _) = action

-- | An action that can be taken to alter the world.
data ReconciliationAction = SetEntityState EntityId HASSDomain ToggleState
  deriving (Eq, Ord, Show)

-- | Pretty-prints the plan, displaying justification for each step.
renderPlanTrace :: (MonadIO m) => ReportStyle -> ReconciliationPlan -> m Text
renderPlanTrace style = liftIO . runEff . File.runFileSystem . innerRenderPlanTrace style

innerRenderPlanTrace :: (File.FileSystem :> es) => ReportStyle -> ReconciliationPlan -> Eff es Text
innerRenderPlanTrace style (MkReconciliationPlan steps) = do
  prettySteps <- mapM (prettifyStep style) steps
  let doc = vsep prettySteps
      rendered = layoutDoc style doc
      cleaned = cleanExplanation rendered
  pure cleaned

prettifyStep :: (File.FileSystem :> es) => ReportStyle -> ReconciliationStep -> Eff es (Doc AnsiStyle)
prettifyStep style (action :£ reason) = do
  prettyReason <- prettifyExplanation style reason
  pure
    (vsep ["will" <+> pretty action, "reason:", indent 4 prettyReason])

instance Pretty ReconciliationAction where
  pretty = \case
    SetEntityState eId domain state -> "turn" <+> pretty domain <+> pretty eId <+> pretty state

--------------------------------------------------------------------------------

-- | Computes the steps necessary to transform
-- the observed world state into the specified world state.
reconcile :: ObservedWorld -> Specification -> (ReconciliationPlan, ReconciliationReport)
reconcile observedWorld spec =
  runPureEff
    . Writer.runWriter
    . State.evalState observedWorld
    . State.evalState spec
    $ reconcileInner

reconcileInner ::
  ( State ObservedWorld :> es,
    State Specification :> es,
    Writer ReconciliationReport :> es
  ) =>
  Eff es ReconciliationPlan
reconcileInner = do
  spec <- State.get
  steps <- catMaybes <$> mapM (computeDesiredState . expression) (specPolicies spec)

  pure (MkReconciliationPlan steps)

type Detailed a = Explained (Located a)

data ReconciliationError where
  UnknownEntity :: Located EntityId -> ReconciliationError
  TypeMismatch :: T -> Located T -> ReconciliationError

deriving instance Eq ReconciliationError

deriving instance Ord ReconciliationError

deriving instance Show ReconciliationError

unknownEntity :: (Error ReconciliationError :> es) => Located EntityId -> Eff es a
unknownEntity = Error.throwError . UnknownEntity

type family Denote (t :: T) :: Type where
  Denote 'TBool = Bool
  Denote 'TState = ToggleState
  Denote 'TEntityLight = EntityId
  Denote 'TTime = TimeOfDay

getToggleStateOf ::
  ( State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located EntityId ->
  Eff es ToggleState
getToggleStateOf entity@(eId :@ _) = do
  worldToggleables <- State.gets (worldToggleables . observedWorld)
  case HMap.lookup eId worldToggleables of
    Just state -> pure state
    Nothing -> unknownEntity entity

ensureEntityExists ::
  ( State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located EntityId -> Eff es ()
ensureEntityExists entity = getToggleStateOf entity >> pure () -- getToggleStateOf can throw

computeDesiredState ::
  ( State ObservedWorld :> es,
    Writer ReconciliationReport :> es
  ) =>
  Located (Exp 'TAction) ->
  Eff es (Maybe ReconciliationStep)
computeDesiredState action =
  Error.runErrorNoCallStack
    ( case action of
        ESetState eEntity eDesiredState :@ loc -> do
          (entity@(eId :@ _) :£ _, domain) <- evalToggleable eEntity
          desiredState :@ _ :£ desiredStateExpl <- evalState eDesiredState
          currentState <- getToggleStateOf entity
          pure $
            if desiredState == currentState
              then empty
              else
                Just
                  ( SetEntityState eId domain desiredState
                      `because` (desired loc eId desiredState `explain` desiredStateExpl)
                      `becauseMore` (observed eId currentState)
                  )
        EIf (condExp :@ _) (thenExp :@ thenLoc) (elseExp :@ elseLoc) :@ _ ->
          evalBool condExp >>= \case
            (True :@ _) :£ trueExpl -> evalBranch trueExpl thenLoc thenExp
            (False :@ _) :£ falseExpl -> evalBranch falseExpl elseLoc elseExp
        EDoNothing :@ _ -> pure empty
    )
    >>= \case
      Left (UnknownEntity eId) -> do
        knownEntities <- State.gets (HMap.keys . worldToggleables . observedWorld)
        Writer.tell (warnUnknownEntity knownEntities eId)
        pure Nothing
      Left (TypeMismatch expectedT actualT) -> do
        Writer.tell (errorTypeMismatch expectedT actualT)
        pure Nothing
      Right result -> pure result

evalToggleable ::
  ( Proved Toggleable t,
    State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located (Exp t) ->
  Eff es (Detailed EntityId, HASSDomain)
evalToggleable @t expr =
  case (auto @Toggleable @t) of
    ToggleLight -> (,domainLight) <$> evalEntity expr

evalBranch ::
  ( State ObservedWorld :> es,
    Writer ReconciliationReport :> es
  ) =>
  Explanation ->
  Location ->
  Located (Exp TAction) ->
  Eff es (Maybe ReconciliationStep)
evalBranch conditionExplanation ifLoc expr = do
  maybeDesiredState <- computeDesiredState expr
  pure $
    maybeDesiredState
      >>= pure
        . (`becauseMore` (branched ifLoc `explain` conditionExplanation))

evalBool ::
  ( State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located (Exp 'TBool) ->
  Eff es (Detailed Bool)
evalBool = \case
  EEqual e1 e2 :@ loc -> do
    Value (s1 :@ s1Loc :£ s1Expl) <- evalEq e1
    Value (s2 :@ s2Loc :£ s2Expl) <- evalEq e2
    let areEqual = s1 == s2
    pure $
      (areEqual :@ loc)
        `because` equality loc areEqual
        `becauseMore` (evaluated s1Loc s1 `explain` s1Expl)
        `becauseMore` (evaluated s2Loc s2 `explain` s2Expl)
  ECompare op e1 e2 :@ loc -> do
    Value (s1 :@ s1Loc :£ s1Expl) <- evalComp e1
    Value (s2 :@ s2Loc :£ s2Expl) <- evalComp e2
    let compareVals = toComparator op
        result = compareVals s1 s2
    pure $
      (result :@ loc)
        `because` comparison loc s1 op s2 result
        `becauseMore` (evaluated s1Loc s1 `explain` s1Expl)
        `becauseMore` (evaluated s2Loc s2 `explain` s2Expl)

toComparator :: (Ord a) => ComparisonOp -> (a -> a -> Bool)
toComparator = \case
  GreaterThan -> (>)
  GreaterOrEqual -> (>=)
  LessThan -> (<)
  LessThanOrEqual -> (<=)

data Value t where
  Value ::
    ( Eq (Denote t),
      Pretty (Denote t),
      Show (Denote t),
      Ord (Denote t),
      Typeable (Denote t)
    ) =>
    Detailed (Denote t) -> Value t

evalEq ::
  ( Proved Equatable t,
    State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located (Exp t) ->
  Eff es (Value t)
evalEq @t expr =
  case (auto @Equatable @t) of
    EqState -> Value <$> evalState expr
    EqTime -> Value <$> evalTime expr

evalComp ::
  ( Proved Comparable t,
    State ObservedWorld :> es
  ) =>
  Located (Exp t) ->
  Eff es (Value t)
evalComp @t expr =
  case (auto @Comparable @t) of
    CompTime -> Value <$> evalTime expr

evalState ::
  ( State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located (Exp 'TState) ->
  Eff es (Detailed ToggleState)
evalState = \case
  ELitState s :@ loc -> pure (s :@ loc `because` literal loc)
  EGetState entity :@ eloc -> do
    eId :@ _ :£ _ <- evalEntity entity
    worldToggleables <- State.gets (worldToggleables . observedWorld)
    case HMap.lookup eId worldToggleables of
      Just state -> pure (state :@ eloc `because` observed eId state)
      Nothing -> unknownEntity (eId :@ eloc)

evalTime ::
  (State ObservedWorld :> es) =>
  Located (Exp 'TTime) ->
  Eff es (Detailed TimeOfDay)
evalTime = \case
  ELitTime timeOfDay :@ loc -> pure $ timeOfDay :@ loc `because` literal loc
  EGetTime :@ loc -> do
    timeOfDay <- State.gets observedTimeOfDay
    pure (timeOfDay :@ loc `because` observedTime timeOfDay)

evalEntity ::
  ( State ObservedWorld :> es,
    Error ReconciliationError :> es
  ) =>
  Located (Exp 'TEntityLight) -> Eff es (Detailed EntityId)
evalEntity = \case
  ELitEntityLight eId :@ loc -> do
    ensureEntityExists (eId :@ loc)
    pure (eId :@ loc `because` literal loc)
