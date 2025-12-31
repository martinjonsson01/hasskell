{-# OPTIONS_GHC -Wno-orphans #-}

module Hasskell.Language.Verifier
  ( verify,
    -- Verified types
    VerifiedSpecification,
    KnownEntityId,
    -- Classes
    HasKnownEntityId (..),
    HasCurrentState (..),
    -- Reports
    VerificationReport,
    hasWarnings,
    renderReport,
  )
where

import Control.Applicative
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Set qualified as S
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import Effectful.Writer.Static.Local (Writer)
import Effectful.Writer.Static.Local qualified as Writer
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.Diagnostic
import Hasskell.Language.World

type VerifiedSpecification = Specification Verified

type instance Params "ELitEntityLight" 'Verified = (KnownEntityId, ToggleState)

type instance Params "ELitEntityInputBoolean" 'Verified = (KnownEntityId, ToggleState)

-- | Things that have a known-verified entity ID.
class HasKnownEntityId a where
  knownIdOf :: a -> KnownEntityId

instance (Proved IsEntity t) => HasKnownEntityId (Exp Verified t) where
  knownIdOf = case (auto @IsEntity @t) of
    LightIsEntity -> \(ELitEntityLight (eId, _)) -> eId
    InputBooleanIsEntity -> \(ELitEntityInputBoolean (eId, _)) -> eId

-- | Things that have a known current state.
class HasCurrentState a where
  stateOf :: a -> ToggleState

instance (Proved IsEntity t) => HasCurrentState (Exp Verified t) where
  stateOf = case (auto @IsEntity @t) of
    LightIsEntity -> \(ELitEntityLight (_, s)) -> s
    InputBooleanIsEntity -> \(ELitEntityInputBoolean (_, s)) -> s

instance HasReferencedEntities (Exp Verified t) where
  referencedEntitiesIn = foldExp $ \expr referenced ->
    case expr of
      ELitEntityLight (eId, _) -> S.singleton (idOf eId)
      ELitEntityInputBoolean (eId, _) -> S.singleton (idOf eId)
      _ -> referenced

verify :: ObservedWorld -> RawSpecification -> (VerifiedSpecification, VerificationReport)
verify observedWorld spec =
  runPureEff
    . Writer.runWriter
    . State.evalState observedWorld
    . State.evalState spec
    $ verifyInner

verifyInner ::
  ( State ObservedWorld :> es,
    State RawSpecification :> es,
    Writer VerificationReport :> es
  ) =>
  Eff es VerifiedSpecification
verifyInner =
  State.get >>= verifySpecification

data VerificationError where
  UnknownEntity :: Located EntityId -> VerificationError
  deriving (Eq, Ord, Show)

unknownEntity :: (Error VerificationError :> es) => Located EntityId -> Eff es a
unknownEntity = Error.throwError . UnknownEntity

verifySpecification ::
  ( State ObservedWorld :> es,
    Writer VerificationReport :> es
  ) =>
  Specification Raw ->
  Eff es (Specification Verified)
verifySpecification (Specification steps) =
  (Specification . catMaybes) <$> mapM verifyPolicy steps

verifyPolicy ::
  ( State ObservedWorld :> es,
    Writer VerificationReport :> es
  ) =>
  Policy Raw ->
  Eff es (Maybe (Policy Verified))
verifyPolicy (Policy name action) = do
  mbVerified <- tryVerifyAction action
  case mbVerified of
    Just verified -> pure $ Just (Policy name verified)
    Nothing -> pure empty

tryVerifyAction ::
  ( State ObservedWorld :> es,
    Writer VerificationReport :> es
  ) =>
  Located (Exp Raw 'TAction) ->
  Eff es (Maybe (Located (Exp Verified 'TAction)))
tryVerifyAction action =
  Error.runErrorNoCallStack
    (verifyAction action)
    >>= \case
      Left (UnknownEntity eId) -> do
        knownEntities <- State.gets (HM.keys . worldToggleables . observedWorld)
        Writer.tell (warnUnknownEntity knownEntities eId)
        pure Nothing
      Right result -> pure $ Just result

verifyAction ::
  ( State ObservedWorld :> es,
    Writer VerificationReport :> es,
    Error VerificationError :> es
  ) =>
  Located (Exp Raw 'TAction) ->
  Eff es (Located (Exp Verified 'TAction))
verifyAction = \case
  ESetState eEntity eDesiredState :@ loc -> do
    verifiedEntity <- verifyToggleable eEntity
    verifiedDesiredState <- verifyState eDesiredState
    pure (ESetState verifiedEntity verifiedDesiredState :@ loc)
  EIf condExp (thenExp :@ thenLoc) (elseExp :@ elseLoc) :@ loc -> do
    verifiedCond <- verifyBool condExp
    verifiedThen <- verifyAction thenExp
    verifiedElse <- verifyAction elseExp
    pure (EIf verifiedCond (verifiedThen :@ thenLoc) (verifiedElse :@ elseLoc) :@ loc)
  EDoNothing :@ loc -> pure (EDoNothing :@ loc)

verifyBool ::
  ( State ObservedWorld :> es,
    Error VerificationError :> es
  ) =>
  Located (Located (Exp Raw TBool)) ->
  Eff es (Located (Located (Exp Verified TBool)))
verifyBool = \case
  (EEqual e1 e2 :@ operandLoc) :@ syntaxLoc -> do
    verifiedE1 <- verifyEquatable e1
    verifiedE2 <- verifyEquatable e2
    pure (EEqual verifiedE1 verifiedE2 :@ operandLoc :@ syntaxLoc)
  (ECompare op e1 e2 :@ operandLoc) :@ syntaxLoc -> do
    verifiedE1 <- verifyComparable e1
    verifiedE2 <- verifyComparable e2
    pure (ECompare op verifiedE1 verifiedE2 :@ operandLoc :@ syntaxLoc)

verifyComparable ::
  (Proved Comparable t) =>
  Located (Exp Raw t) ->
  Eff es (Located (Exp Verified t))
verifyComparable @t expr =
  case (auto @Comparable @t) of
    CompTime -> verifyTime expr

verifyTime ::
  Located (Exp Raw TTime) ->
  Eff es (Located (Exp Verified TTime))
verifyTime = \case
  ELitTime timeOfDay :@ loc -> pure (ELitTime timeOfDay :@ loc)
  EGetTime :@ loc -> pure (EGetTime :@ loc)

verifyEquatable ::
  ( Proved Equatable t,
    State ObservedWorld :> es,
    Error VerificationError :> es
  ) =>
  Located (Exp Raw t) ->
  Eff es (Located (Exp Verified t))
verifyEquatable @t expr =
  case (auto @Equatable @t) of
    EqState -> verifyState expr
    EqTime -> verifyTime expr

verifyState ::
  ( State ObservedWorld :> es,
    Error VerificationError :> es
  ) =>
  Located (Exp Raw TState) ->
  Eff es (Located (Exp Verified TState))
verifyState = \case
  ELitState s :@ loc -> pure (ELitState s :@ loc)
  EGetState entity :@ loc -> do
    verifiedEntity <- verifyToggleable entity
    pure (EGetState verifiedEntity :@ loc)

verifyToggleable ::
  ( Proved Toggleable t,
    State ObservedWorld :> es,
    Error VerificationError :> es
  ) =>
  Located (Exp Raw t) ->
  Eff es (Located (Exp Verified t))
verifyToggleable @t (e :@ loc) = do
  let eId = idOf e
  entityData <- verifyEntity (eId :@ loc)
  let makeEntity = case (auto @Toggleable @t) of
        ToggleLight -> ELitEntityLight
        ToggleInputBoolean -> ELitEntityInputBoolean
  pure (makeEntity entityData :@ loc)

verifyEntity ::
  ( State ObservedWorld :> es,
    Error VerificationError :> es
  ) =>
  Located EntityId ->
  Eff es (KnownEntityId, ToggleState)
verifyEntity entity@(eId :@ _) = do
  mbEntity <- State.gets (lookupEntity eId)
  case mbEntity of
    Just result -> pure result
    Nothing -> unknownEntity entity
