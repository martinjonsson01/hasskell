{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- The ValidHour/Minute is marked 'redundant'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hasskell.Language.AST
  ( Specification (..),
    Policy (..),
    policy,
    HasReferencedEntities (..),
    HasLocations (..),
    SomeExp (..),
    T (..),
    Exp (..),
    -- Properties
    Proved (..),
    Equatable (..),
    ComparisonOp (..),
    Comparable (..),
    Toggleable (..),
    negateComparison,
    -- Entities
    HasEntityId (..),
    light,
    -- State
    fromState,
    on,
    off,
    shouldBe,
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

import Data.Eq.Singletons
import Data.Kind
import Data.List (singleton)
import Data.Ord.Singletons
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
import Hasskell.Language.World
import Prelude.Singletons
import Prettyprinter

$( singletons
     [d|
       data T = TEntityLight | TAction | TBool | TState | TTime

       deriving instance (Eq T)

       deriving instance (Ord T)

       deriving instance (Show T)
       |]
 )

-- | A specification defining rules for desired world states.
data Specification = Specification
  { specPolicies :: [Policy]
  }
  deriving (Show)

instance Semigroup Specification where
  (Specification policiesA) <> (Specification policiesB) = Specification $ policiesA <> policiesB

instance Monoid Specification where
  mempty = Specification []

data Policy = Policy {name :: Text, expression :: Located (Exp 'TAction)}
  deriving (Eq, Ord, Show)

-- | Something that can be turned into an action.
class IntoAction a where
  toAction :: a -> Located (Exp 'TAction)

instance (BuildableIf state) => IntoAction (IfBuilder state) where
  toAction = buildIf

instance IntoAction (Located (Exp 'TAction)) where
  toAction = id

-- | Declare a desired state.
policy :: (IntoAction a) => Text -> a -> Specification
policy name expr = Specification . singleton $ Policy name (toAction expr)

class HasReferencedEntities a where
  referencedEntitiesIn :: a -> [EntityId]

instance (HasReferencedEntities a) => HasReferencedEntities (Located a) where
  referencedEntitiesIn (a :@ _) = referencedEntitiesIn a

instance HasReferencedEntities Specification where
  referencedEntitiesIn Specification {specPolicies} = concatMap referencedEntitiesIn specPolicies

instance HasReferencedEntities Policy where
  referencedEntitiesIn Policy {expression} = referencedEntitiesIn expression

instance HasReferencedEntities (Exp t) where
  referencedEntitiesIn = \case
    ELitEntityLight eId -> [eId]
    ELitState _ -> []
    ELitTime _ -> []
    EGetState e -> referencedEntitiesIn e
    EGetTime -> []
    ESetState expr _ -> referencedEntitiesIn expr
    EDoNothing -> []
    EEqual e1 e2 -> referencedEntitiesIn e1 ++ referencedEntitiesIn e2
    EIf eCond eThen eElse ->
      referencedEntitiesIn eCond
        ++ referencedEntitiesIn eThen
        ++ referencedEntitiesIn eElse
    ECompare _ e1 e2 -> referencedEntitiesIn e1 ++ referencedEntitiesIn e2

instance HasLocations (Exp t) where
  extractLocations = \case
    ELitEntityLight _ -> []
    ELitState _ -> []
    ELitTime _ -> []
    EGetState (_ :@ loc) -> [loc]
    EGetTime -> []
    ESetState expr _ -> extractLocations expr
    EDoNothing -> []
    EEqual e1 e2 -> extractLocations e1 ++ extractLocations e2
    EIf eCond eThen eElse ->
      extractLocations eCond
        ++ extractLocations eThen
        ++ extractLocations eElse
    ECompare _ e1 e2 -> extractLocations e1 ++ extractLocations e2

data SomeExp :: Type where
  SomeExp :: (SingI (t :: T)) => Exp t -> SomeExp

instance Show (SomeExp) where
  show (SomeExp e) = show e

instance Eq SomeExp where
  SomeExp (e1 :: Exp t1) == SomeExp (e2 :: Exp t2) =
    case (sing @t1) %~ (sing @t2) of
      Proved Refl -> e1 == e2
      Disproved _ -> False

instance Ord SomeExp where
  compare (SomeExp (e1 :: Exp t1)) (SomeExp (e2 :: Exp t2)) =
    case (sing @t1) %~ (sing @t2) of
      Proved Refl -> compare e1 e2
      Disproved _ -> fromSing $ sCompare (sing @t1) (sing @t2)

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

data Exp :: T -> Type where
  -- Literals
  ELitEntityLight :: EntityId -> Exp 'TEntityLight
  ELitState :: ToggleState -> Exp 'TState
  ELitTime :: TimeOfDay -> Exp 'TTime
  -- Entity properties
  EGetState :: Located (Exp 'TEntityLight) -> Exp 'TState
  -- Time
  EGetTime :: Exp 'TTime
  -- Actions
  ESetState ::
    (SingI t, Proved Toggleable t) =>
    Located (Exp t) ->
    Located (Exp 'TState) ->
    Exp 'TAction
  EDoNothing :: Exp 'TAction
  -- Boolean logic
  EEqual ::
    (SingI t, Proved Equatable t) =>
    Located (Exp t) ->
    Located (Exp t) ->
    Exp 'TBool
  EIf :: -- Nested locs to record both the if-then-else syntax and its operands.
    Located (Located (Exp 'TBool)) ->
    Located (Located (Exp 'TAction)) ->
    Located (Located (Exp 'TAction)) ->
    Exp 'TAction
  ECompare ::
    (SingI t, Proved Comparable t) =>
    ComparisonOp ->
    Located (Exp t) ->
    Located (Exp t) ->
    Exp 'TBool

deriving instance Show (Exp t)

instance Eq (Exp t) where
  ELitEntityLight x == ELitEntityLight y = x == y
  ELitState x == ELitState y = x == y
  ELitTime x == ELitTime y = x == y
  EGetState x == EGetState y = x == y
  EGetTime == EGetTime = True
  ESetState @t1 e1 s1 == ESetState @t2 e2 s2 =
    case (sing @t1) %~ (sing @t2) of
      Proved Refl -> e1 == e2 && s1 == s2
      Disproved _ -> False
  EDoNothing == EDoNothing = True
  EEqual @t1 a1 b1 == EEqual @t2 a2 b2 =
    case (sing @t1) %~ (sing @t2) of
      Proved Refl -> a1 == a2 && b1 == b2
      Disproved _ -> False
  EIf eCond1 eThen1 eElse1 == EIf eCond2 eThen2 eElse2 =
    eCond1 == eCond2
      && eThen1 == eThen2
      && eElse1 == eElse2
  ECompare @t1 op1 a1 b1 == ECompare @t2 op2 a2 b2 =
    op1 == op2
      && case (sing @t1) %~ (sing @t2) of
        Proved Refl -> a1 == a2 && b1 == b2
        Disproved _ -> False
  _ == _ = False

instance Ord (Exp t) where
  compare x y =
    case (x, y) of
      -- Literals
      (ELitEntityLight x1, ELitEntityLight x2) -> compare x1 x2
      (ELitState x1, ELitState x2) -> compare x1 x2
      (ELitState _, _) -> LT
      (_, ELitState _) -> GT
      (ELitTime x1, ELitTime x2) -> compare x1 x2
      (ELitTime _, _) -> LT
      (_, ELitTime _) -> GT
      -- Entity properties
      (EGetState e1, EGetState e2) -> compare e1 e2
      -- Time
      (EGetTime, EGetTime) -> EQ
      -- Actions
      (ESetState @t1 e1 s1, ESetState @t2 e2 s2) ->
        case (sing @t1) %~ (sing @t2) of
          Proved Refl -> compare e1 e2 <> compare s1 s2
          Disproved _ ->
            compare
              (fromSing (sing @t1))
              (fromSing (sing @t2))
      (ESetState _ _, _) -> LT
      (_, ESetState _ _) -> GT
      (EDoNothing, EDoNothing) -> EQ
      (EDoNothing, _) -> LT
      (_, EDoNothing) -> GT
      -- Boolean logic
      (EEqual @t1 a1 b1, EEqual @t2 a2 b2) ->
        case (sing @t1) %~ (sing @t2) of
          Proved Refl -> compare a1 a2 <> compare b1 b2
          Disproved _ ->
            compare
              (fromSing (sing @t1))
              (fromSing (sing @t2))
      (EEqual _ _, _) -> LT
      (_, EEqual _ _) -> GT
      (EIf eCond1 eThen1 eElse1, EIf eCond2 eThen2 eElse2) ->
        compare eCond1 eCond2
          <> compare eThen1 eThen2
          <> compare eElse1 eElse2
      (ECompare @t1 c1 a1 b1, ECompare @t2 c2 a2 b2) ->
        compare c1 c2
          <> case (sing @t1) %~ (sing @t2) of
            Proved Refl -> compare a1 a2 <> compare b1 b2
            Disproved _ ->
              compare
                (fromSing (sing @t1))
                (fromSing (sing @t2))

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

instance Proved Toggleable 'TEntityLight where
  auto = ToggleLight

--------------------------------------------------------------------------------

-- | Things that have an entity ID.
class HasEntityId a where
  idOf :: a -> EntityId

instance (HasEntityId a) => HasEntityId (Located a) where
  idOf (a :@ _) = idOf a

instance HasEntityId (Exp 'TEntityLight) where
  idOf (ELitEntityLight eId) = eId

-- | An entity that can be toggled.
light :: (HasCallStack) => Text -> Located (Exp 'TEntityLight)
light = (:@ captureSrcSpan) . ELitEntityLight . makeEntityIdUnsafe

--------------------------------------------------------------------------------

-- | A specific state.
fromState :: (HasCallStack) => ToggleState -> Located (Exp 'TState)
fromState = (:@ captureSrcSpan) . ELitState

-- | The 'on' state.
on :: (HasCallStack) => Located (Exp 'TState)
on = fromState On

-- | The 'off' state.
off :: (HasCallStack) => Located (Exp TState)
off = fromState Off

-- | Declare that a given entity should be in a given state.
shouldBe :: (HasCallStack) => Located (Exp 'TEntityLight) -> Located (Exp 'TState) -> Located (Exp 'TAction)
shouldBe entity state = ESetState entity state :@ captureSrcSpan

-- | Gets the current toggle state of the given entity.
toggledStateOf :: (HasCallStack) => Located (Exp 'TEntityLight) -> Located (Exp 'TState)
toggledStateOf entity = EGetState entity :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Check for equality.
is ::
  (HasCallStack, SingI t, Proved Equatable t) =>
  Located (Exp t) ->
  Located (Exp t) ->
  Located (Exp 'TBool)
is s1 s2 = EEqual s1 s2 :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Check whether the first expression is strictly greater than the second.
isGreaterThan ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp t) ->
  Located (Exp t) ->
  Located (Exp 'TBool)
isGreaterThan e1 e2 = ECompare GreaterThan e1 e2 :@ captureSrcSpan

-- | Check whether the first expression is greater than or equal to the second.
isGreaterOrEqualTo ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp t) ->
  Located (Exp t) ->
  Located (Exp 'TBool)
isGreaterOrEqualTo e1 e2 = ECompare GreaterOrEqual e1 e2 :@ captureSrcSpan

-- | Check whether the first expression is strictly less than to the second.
isLessThan ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp t) ->
  Located (Exp t) ->
  Located (Exp 'TBool)
isLessThan e1 e2 = ECompare LessThan e1 e2 :@ captureSrcSpan

-- | Check whether the first expression is less than or equal to the second.
isLessOrEqualTo ::
  ( HasCallStack,
    SingI t,
    Proved Comparable t
  ) =>
  Located (Exp t) ->
  Located (Exp t) ->
  Located (Exp 'TBool)
isLessOrEqualTo e1 e2 = ECompare LessThanOrEqual e1 e2 :@ captureSrcSpan

--------------------------------------------------------------------------------

-- | Gets the current world time.
currentTime :: (HasCallStack) => Located (Exp 'TTime)
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
  Located (Exp 'TTime)
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
    Located (Located (Exp 'TBool)) ->
    IfBuilder IfCond
  IfThenB ::
    Located (Located (Exp 'TBool)) ->
    Located (Located (Exp 'TAction)) ->
    IfBuilder IfThen
  IfThenElseB ::
    Located (Located (Exp 'TBool)) ->
    Located (Located (Exp 'TAction)) ->
    Located (Located (Exp 'TAction)) ->
    IfBuilder IfThenElse

-- | Make a policy conditional on something.
if_ :: (HasCallStack) => Located (Exp 'TBool) -> IfBuilder IfCond
if_ cond = IfCondB (cond :@ captureSrcSpan)

-- | Define what should happen if the condition holds.
then_ :: (HasCallStack) => IfBuilder IfCond -> Located (Exp 'TAction) -> IfBuilder IfThen
then_ (IfCondB condExp) thenExp = IfThenB condExp (thenExp :@ captureSrcSpan)

-- | Define what should happen if the condition fails.
else_ :: (HasCallStack) => IfBuilder IfThen -> Located (Exp 'TAction) -> IfBuilder IfThenElse
else_ (IfThenB cond thenExp) elseExp = IfThenElseB cond thenExp (elseExp :@ captureSrcSpan)

-- | Defines a stage at which the if-builder can construct a valid if expression.
class BuildableIf stage where
  buildIf :: (HasCallStack) => IfBuilder stage -> Located (Exp 'TAction)

-- | Fully qualified if-then-else expressions are valid.
instance BuildableIf IfThenElse where
  buildIf (IfThenElseB condExp thenExp elseExp) =
    EIf condExp thenExp elseExp :@ captureSrcSpan

-- | If-then expressions can be valid as well (the else is just a null action).
instance BuildableIf IfThen where
  buildIf (IfThenB condExp thenExp) =
    EIf condExp thenExp (EDoNothing :@ captureSrcSpan :@ captureSrcSpan) :@ captureSrcSpan

--------------------------------------------------------------------------------
