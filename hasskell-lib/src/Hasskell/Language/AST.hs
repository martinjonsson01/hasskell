{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- The ValidHour/Minute is marked 'redundant'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hasskell.Language.AST
  ( Specification (..),
    RawSpecification,
    Policy (..),
    policy,
    HasReferencedEntities (..),
    HasLocations (..),
    SomeExp (..),
    T (..),
    Exp (..),
    Params,
    Phase (..),
    -- Folding
    foldExpM,
    foldExp,
    -- Properties
    Proved (..),
    Equatable (..),
    ComparisonOp (..),
    Comparable (..),
    Toggleable (..),
    IsEntity (..),
    negateComparison,
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
    -- Singleton types
    ST (..),
    -- Reexports
    Located (..),
  )
where

import Control.Applicative.HT
import Control.Monad.Identity
import Data.Eq.Singletons
import Data.Kind
import Data.List qualified as List
import Data.Ord.Singletons
import Data.Set qualified as S
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Stack
import GHC.TypeLits hiding (Text)
import GHC.TypeLits qualified as TypeLits
import Hasskell.HomeAssistant.API
import Hasskell.Language.CallStack
import Hasskell.Language.Entity
import Hasskell.Language.World
import Prelude.Singletons
import Prettyprinter

$( singletons
     [d|
       data T
         = TEntityInputBoolean
         | TEntityLight
         | TAction
         | TBool
         | TState
         | TTime

       deriving instance (Eq T)

       deriving instance (Ord T)

       deriving instance (Show T)

       -- \| A stage of the AST processing.
       data Phase = Raw | Verified

       deriving instance (Eq Phase)

       deriving instance (Ord Phase)

       deriving instance (Show Phase)
       |]
 )

-- | A specification defining rules for desired world states.
data Specification (p :: Phase) where
  Specification :: {specPolicies :: [Policy p]} -> Specification p
  deriving (Show)

instance Semigroup (Specification p) where
  (Specification policiesA) <> (Specification policiesB) = Specification $ policiesA <> policiesB

instance Monoid (Specification p) where
  mempty = Specification mempty

type RawSpecification = Specification Raw

data Policy (p :: Phase) where
  Policy :: {name :: Text, expression :: Located (Exp p 'TAction)} -> Policy p
  deriving (Eq, Ord, Show)

-- | Something that can be turned into an action.
class IntoAction a where
  toAction :: a -> Located (Exp Raw 'TAction)

instance (BuildableIf state) => IntoAction (IfBuilder state) where
  toAction = buildIf

instance IntoAction (Located (Exp Raw 'TAction)) where
  toAction = id

-- | Declare a desired state.
policy :: (IntoAction a) => Text -> a -> Specification Raw
policy name expr = Specification . List.singleton $ Policy name (toAction expr)

instance HasReferencedEntities (Specification Raw) where
  referencedEntitiesIn Specification {specPolicies} = foldMap referencedEntitiesIn specPolicies

instance HasReferencedEntities (Policy Raw) where
  referencedEntitiesIn Policy {expression} = referencedEntitiesIn expression

instance HasReferencedEntities (Exp Raw t) where
  referencedEntitiesIn = foldExp $ \expr referenced ->
    case expr of
      ELitEntityLight eId -> S.singleton eId
      ELitEntityInputBoolean eId -> S.singleton eId
      _ -> referenced

data SomeExp :: Type where
  SomeExp :: (SingI (t :: T), SingI (p :: Phase)) => Exp p t -> SomeExp

instance Show (SomeExp) where
  show (SomeExp e) = show e

instance Eq SomeExp where
  SomeExp (e1 :: Exp p1 t1) == SomeExp (e2 :: Exp p2 t2) =
    case (sing @p1) %~ (sing @p2) of
      Proved Refl ->
        case (sing @t1) %~ (sing @t2) of
          Proved Refl -> e1 == e2
          Disproved _ -> False
      Disproved _ -> False

instance Ord SomeExp where
  compare (SomeExp (e1 :: Exp p1 t1)) (SomeExp (e2 :: Exp p2 t2)) =
    case (sing @p1) %~ (sing @p2) of
      Proved Refl ->
        case (sing @t1) %~ (sing @t2) of
          Proved Refl -> compare e1 e2
          Disproved _ -> fromSing $ sCompare (sing @t1) (sing @t2)
      Disproved _ -> fromSing $ sCompare (sing @p1) (sing @p2)

-- | Different ways of comparing totally ordered values.
data ComparisonOp = GreaterThan | GreaterOrEqual | LessThan | LessThanOrEqual
  deriving (Eq, Ord, Show)

instance Pretty ComparisonOp where
  pretty = \case
    GreaterThan -> pretty ">"
    GreaterOrEqual -> pretty ">="
    LessThan -> pretty "<"
    LessThanOrEqual -> pretty "<="

-- | The negated operator is the operator that is true when the original is false, and vice-versa.
negateComparison :: ComparisonOp -> ComparisonOp
negateComparison = \case
  GreaterThan -> LessThanOrEqual
  GreaterOrEqual -> LessThan
  LessThan -> GreaterOrEqual
  LessThanOrEqual -> GreaterThan

-- | The parameters each AST node has depending on the current phase.
type family Params (c :: Symbol) (p :: Phase) :: Type

type instance Params "ELitEntityLight" Raw = EntityId

type instance Params "ELitEntityInputBoolean" Raw = EntityId

data Exp (p :: Phase) (t :: T) :: Type where
  -- Literals
  ELitEntityLight ::
    ( Show (Params "ELitEntityLight" p),
      Eq (Params "ELitEntityLight" p),
      Ord (Params "ELitEntityLight" p)
    ) =>
    Params "ELitEntityLight" p -> Exp p 'TEntityLight
  ELitEntityInputBoolean ::
    ( Show (Params "ELitEntityInputBoolean" p),
      Eq (Params "ELitEntityInputBoolean" p),
      Ord (Params "ELitEntityInputBoolean" p)
    ) =>
    Params "ELitEntityInputBoolean" p -> Exp p 'TEntityInputBoolean
  ELitState :: ToggleState -> Exp p 'TState
  ELitTime :: TimeOfDay -> Exp p 'TTime
  -- Entity properties
  EGetState ::
    (SingI t, SingI p, Proved IsEntity t, Proved Toggleable t) =>
    Located (Exp p t) -> Exp p 'TState
  -- Time
  EGetTime :: Exp p 'TTime
  -- Actions
  ESetState ::
    (SingI t, SingI p, Proved Toggleable t) =>
    Located (Exp p t) ->
    Located (Exp p 'TState) ->
    Exp p 'TAction
  EDoNothing :: Exp p 'TAction
  -- Boolean logic
  EEqual ::
    (SingI t, SingI p, Proved Equatable t) =>
    Located (Exp p t) ->
    Located (Exp p t) ->
    Exp p 'TBool
  EIf :: -- Nested locs to record both the if-then-else syntax and its operands.
    Located (Located (Exp p 'TBool)) ->
    Located (Located (Exp p 'TAction)) ->
    Located (Located (Exp p 'TAction)) ->
    Exp p 'TAction
  ECompare ::
    (SingI t, SingI p, Proved Comparable t) =>
    ComparisonOp ->
    Located (Exp p t) ->
    Located (Exp p t) ->
    Exp p 'TBool

deriving instance Show (Exp p t)

instance Eq (Exp p t) where
  ELitEntityLight x == ELitEntityLight y = x == y
  ELitState x == ELitState y = x == y
  ELitTime x == ELitTime y = x == y
  EGetState x == EGetState y = eqExistentials x y
  EGetTime == EGetTime = True
  ESetState e1 s1 == ESetState e2 s2 =
    eqExistentials e1 e2 && eqExistentials s1 s2
  EDoNothing == EDoNothing = True
  EEqual a1 b1 == EEqual a2 b2 =
    eqExistentials a1 a2 && eqExistentials b1 b2
  EIf eCond1 eThen1 eElse1 == EIf eCond2 eThen2 eElse2 =
    eCond1 == eCond2
      && eThen1 == eThen2
      && eElse1 == eElse2
  ECompare op1 a1 b1 == ECompare op2 a2 b2 =
    op1 == op2
      && eqExistentials a1 a2
      && eqExistentials b1 b2
  _ == _ = False

eqExistentials ::
  (SingI t1, SingI t2) =>
  Located (Exp p t1) ->
  Located (Exp p t2) ->
  Bool
eqExistentials @t1 @t2 e1 e2 =
  case (sing @t1) %~ (sing @t2) of
    Proved Refl -> e1 == e2
    Disproved _ -> False

instance Ord (Exp p t) where
  compare x y =
    case (x, y) of
      -- Literals
      (ELitEntityLight x1, ELitEntityLight x2) -> compare x1 x2
      (ELitEntityInputBoolean x1, ELitEntityInputBoolean x2) -> compare x1 x2
      (ELitState x1, ELitState x2) -> compare x1 x2
      (ELitState _, _) -> LT
      (_, ELitState _) -> GT
      (ELitTime x1, ELitTime x2) -> compare x1 x2
      (ELitTime _, _) -> LT
      (_, ELitTime _) -> GT
      -- Entity properties
      (EGetState e1, EGetState e2) -> compareExistentials e1 e2
      -- Time
      (EGetTime, EGetTime) -> EQ
      -- Actions
      (ESetState e1 s1, ESetState e2 s2) ->
        compareExistentials e1 e2 <> compareExistentials s1 s2
      (ESetState _ _, _) -> LT
      (_, ESetState _ _) -> GT
      (EDoNothing, EDoNothing) -> EQ
      (EDoNothing, _) -> LT
      (_, EDoNothing) -> GT
      -- Boolean logic
      (EEqual a1 b1, EEqual a2 b2) ->
        compareExistentials a1 a2 <> compareExistentials b1 b2
      (EEqual _ _, _) -> LT
      (_, EEqual _ _) -> GT
      (EIf eCond1 eThen1 eElse1, EIf eCond2 eThen2 eElse2) ->
        compare eCond1 eCond2
          <> compare eThen1 eThen2
          <> compare eElse1 eElse2
      (ECompare c1 a1 b1, ECompare c2 a2 b2) ->
        compare c1 c2
          <> compareExistentials a1 a2
          <> compareExistentials b1 b2
    where
      compareExistentials ::
        (SingI t1, SingI t2, SingI p1, SingI p2) =>
        Located (Exp p1 t1) ->
        Located (Exp p2 t2) ->
        Ordering
      compareExistentials @t1 @t2 @p1 @p2 e1 e2 =
        case (sing @p1) %~ (sing @p2) of
          Proved Refl ->
            case (sing @t1) %~ (sing @t2) of
              Proved Refl -> compare e1 e2
              Disproved _ ->
                compare
                  (fromSing (sing @t1))
                  (fromSing (sing @t2))
          Disproved _ ->
            compare
              (fromSing (sing @p1))
              (fromSing (sing @p2))

-- | A general fold over expressions.
foldExpM ::
  forall m result t p.
  (Monad m, Monoid result) =>
  (forall t'. Exp p t' -> result -> m result) ->
  Exp p t ->
  m result
foldExpM step = go
  where
    go :: forall t'. Exp p t' -> m result
    go expr = case expr of
      ELitEntityLight {} -> step expr mempty
      ELitEntityInputBoolean {} -> step expr mempty
      ELitState {} -> step expr mempty
      ELitTime {} -> step expr mempty
      EGetTime {} -> step expr mempty
      EDoNothing {} -> step expr mempty
      EGetState e ->
        lift2
          (<>)
          (step expr mempty)
          (go (stripLocation e))
      ESetState e s ->
        lift3
          (\a b c -> a <> b <> c)
          (step expr mempty)
          (go (stripLocation e))
          (go (stripLocation s))
      EEqual e1 e2 ->
        lift3
          mconcat3
          (step expr mempty)
          (go (stripLocation e1))
          (go (stripLocation e2))
      EIf eCond eThen eElse ->
        lift4
          mconcat4
          (step expr mempty)
          (go (stripLocation . stripLocation $ eCond))
          (go (stripLocation . stripLocation $ eThen))
          (go (stripLocation . stripLocation $ eElse))
      ECompare _ e1 e2 ->
        lift3
          mconcat3
          (step expr mempty)
          (go (stripLocation e1))
          (go (stripLocation e2))
      where
        mconcat3 a b c = a <> b <> c
        mconcat4 a b c d = a <> mconcat3 b c d

-- | A general fold over an expression.
foldExp ::
  forall result t p.
  (Monoid result) =>
  (forall t'. Exp p t' -> result -> result) ->
  Exp p t ->
  result
foldExp step = runIdentity . foldExpM (\expr -> pure . step expr)

--------------------------------------------------------------------------------

class Proved p a where
  auto :: p a

-- | Whether values of a given type can be compared using equality checks.
data Equatable :: T -> Type where
  EqState :: Equatable 'TState
  EqTime :: Equatable 'TTime

instance Proved Equatable 'TState where
  auto = EqState

instance Proved Equatable 'TTime where
  auto = EqTime

-- | Whether values of a given type have a total order.
data Comparable :: T -> Type where
  CompTime :: Comparable 'TTime

instance Proved Comparable 'TTime where
  auto = CompTime

-- | Whether values of a given type can be toggled on/off.
data Toggleable :: T -> Type where
  ToggleLight :: Toggleable 'TEntityLight
  ToggleInputBoolean :: Toggleable 'TEntityInputBoolean

instance Proved Toggleable 'TEntityLight where
  auto = ToggleLight

instance Proved Toggleable 'TEntityInputBoolean where
  auto = ToggleInputBoolean

data IsEntity :: T -> Type where
  LightIsEntity :: IsEntity 'TEntityLight
  InputBooleanIsEntity :: IsEntity 'TEntityInputBoolean

instance (Proved IsEntity t) => HasEntityId (Exp Raw t) where
  idOf = case (auto @IsEntity @t) of
    LightIsEntity -> \(ELitEntityLight eId) -> eId
    InputBooleanIsEntity -> \(ELitEntityInputBoolean eId) -> eId

instance (Proved Toggleable t) => Proved IsEntity t where
  auto = case (auto @Toggleable @t) of
    ToggleLight -> LightIsEntity
    ToggleInputBoolean -> InputBooleanIsEntity

instance (Proved IsEntity t) => HasDomain (Exp p t) where
  domainOf = case (auto @IsEntity @t) of
    LightIsEntity -> const domainLight
    InputBooleanIsEntity -> const domainInputBoolean

--------------------------------------------------------------------------------

-- | An entity representing a light.
light :: (HasCallStack) => Text -> Located (Exp Raw 'TEntityLight)
light = (:@ captureSrcSpan) . ELitEntityLight . EntityId

-- | An entity representing an input boolean helper.
inputBoolean :: (HasCallStack) => Text -> Located (Exp Raw 'TEntityInputBoolean)
inputBoolean = (:@ captureSrcSpan) . ELitEntityInputBoolean . EntityId

--------------------------------------------------------------------------------

-- | A specific state.
fromState :: (HasCallStack) => ToggleState -> Located (Exp Raw 'TState)
fromState = (:@ captureSrcSpan) . ELitState

-- | The 'on' state.
on :: (HasCallStack) => Located (Exp Raw 'TState)
on = fromState On

-- | The 'off' state.
off :: (HasCallStack) => Located (Exp Raw TState)
off = fromState Off

-- | Declare that a given entity should be in a given state.
shouldBe ::
  ( HasCallStack,
    SingI t,
    Proved Toggleable t
  ) =>
  Located (Exp Raw t) ->
  Located (Exp Raw 'TState) ->
  Located (Exp Raw 'TAction)
shouldBe entity state = ESetState entity state :@ captureSrcSpan

-- | A noop.
nothing :: (HasCallStack) => Located (Exp Raw 'TAction)
nothing = EDoNothing :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Gets the current toggle state of the given entity.
toggledStateOf ::
  ( HasCallStack,
    SingI t,
    Proved IsEntity t,
    Proved Toggleable t
  ) =>
  Located (Exp Raw t) ->
  Located (Exp Raw 'TState)
toggledStateOf entity = EGetState entity :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Check for equality.
is ::
  (HasCallStack, SingI t, Proved Equatable t) =>
  Located (Exp Raw t) ->
  Located (Exp Raw t) ->
  Located (Exp Raw 'TBool)
is s1 s2 = EEqual s1 s2 :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Check whether the first expression is strictly greater than the second.
isGreaterThan ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp Raw t) ->
  Located (Exp Raw t) ->
  Located (Exp Raw 'TBool)
isGreaterThan e1 e2 = ECompare GreaterThan e1 e2 :@ captureSrcSpan

-- | Check whether the first expression is greater than or equal to the second.
isGreaterOrEqualTo ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp Raw t) ->
  Located (Exp Raw t) ->
  Located (Exp Raw 'TBool)
isGreaterOrEqualTo e1 e2 = ECompare GreaterOrEqual e1 e2 :@ captureSrcSpan

-- | Check whether the first expression is strictly less than to the second.
isLessThan ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp Raw t) ->
  Located (Exp Raw t) ->
  Located (Exp Raw 'TBool)
isLessThan e1 e2 = ECompare LessThan e1 e2 :@ captureSrcSpan

-- | Check whether the first expression is less than or equal to the second.
isLessOrEqualTo ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp Raw t) ->
  Located (Exp Raw t) ->
  Located (Exp Raw 'TBool)
isLessOrEqualTo e1 e2 = ECompare LessThanOrEqual e1 e2 :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Gets the current world time.
currentTime :: (HasCallStack) => Located (Exp Raw 'TTime)
currentTime = EGetTime :@ captureSrcSpan

-- | An hour in the 0-23 (inclusive) range.
type family ValidHour (h :: Nat) :: Constraint where
  ValidHour h =
    If
      (h <=? 23)
      (() :: Constraint)
      (TypeError (TypeLits.Text "Invalid hour: " :<>: ShowType h))

-- | A minute in the 0-59 (inclusive) range.
type family ValidMinute (m :: Nat) :: Constraint where
  ValidMinute m =
    If
      (m <=? 59)
      (() :: Constraint)
      (TypeError (TypeLits.Text "Invalid minute: " :<>: ShowType m))

-- | Need an orphan instance here to allow it to be pretty-printed in the trace.
-- TODO: use newtype instead
instance Pretty TimeOfDay where
  pretty = pretty . formatShow (timeOfDayFormat ExtendedFormat)

-- | Refer to a dateless time of day (24-hour clock).
time ::
  ( HasCallStack,
    KnownNat h,
    ValidHour h,
    KnownNat m,
    ValidMinute m
  ) =>
  Located (Exp Raw 'TTime)
time @h @m =
  ( ELitTime $
      TimeOfDay
        (fromEnum $ natVal (Proxy :: Proxy h))
        (fromEnum $ natVal (Proxy :: Proxy m))
        0
  )
    :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | An incomplete if that only has a condition.
data IfCond

-- | An if-then statement.
data IfThen

-- | A full if-then-else expression.
data IfThenElse

data IfBuilder stage where
  IfCondB ::
    Located (Located (Exp Raw 'TBool)) ->
    IfBuilder IfCond
  IfThenB ::
    Located (Located (Exp Raw 'TBool)) ->
    Located (Located (Exp Raw 'TAction)) ->
    IfBuilder IfThen
  IfThenElseB ::
    Located (Located (Exp Raw 'TBool)) ->
    Located (Located (Exp Raw 'TAction)) ->
    Located (Located (Exp Raw 'TAction)) ->
    IfBuilder IfThenElse

-- | Make a policy conditional on something.
if_ :: (HasCallStack) => Located (Exp Raw 'TBool) -> IfBuilder IfCond
if_ cond = IfCondB (cond :@ captureSrcSpan)

-- | Define what should happen if the condition holds.
then_ :: (HasCallStack) => IfBuilder IfCond -> Located (Exp Raw 'TAction) -> IfBuilder IfThen
then_ (IfCondB condExp) thenExp = IfThenB condExp (thenExp :@ captureSrcSpan)

-- | Define what should happen if the condition fails.
else_ :: (HasCallStack) => IfBuilder IfThen -> Located (Exp Raw 'TAction) -> IfBuilder IfThenElse
else_ (IfThenB cond thenExp) elseExp = IfThenElseB cond thenExp (elseExp :@ captureSrcSpan)

-- | Defines a stage at which the if-builder can construct a valid if expression.
class BuildableIf stage where
  buildIf :: (HasCallStack) => IfBuilder stage -> Located (Exp Raw 'TAction)

-- | Fully qualified if-then-else expressions are valid.
instance BuildableIf IfThenElse where
  buildIf (IfThenElseB condExp thenExp elseExp) =
    EIf condExp thenExp elseExp :@ captureSrcSpan

-- | If-then expressions can be valid as well (the else is just a null action).
instance BuildableIf IfThen where
  buildIf (IfThenB condExp thenExp) =
    EIf condExp thenExp (nothing :@ captureSrcSpan) :@ captureSrcSpan

--------------------------------------------------------------------------------
