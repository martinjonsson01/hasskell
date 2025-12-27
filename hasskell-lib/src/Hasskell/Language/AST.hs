{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}
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
    IntoEntity (..),
    -- Combinators
    fromState,
    on,
    off,
    shouldBe,
    toggledStateOf,
    if_,
    then_,
    else_,
    is,
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
import GHC.Stack
import Hasskell.HomeAssistant.API
import Hasskell.Language.CallStack
import Hasskell.Language.World
import Prelude.Singletons

$( singletons
     [d|
       data T = TEntity | TAction | TBool | TState

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
    ELitEntity eId -> [eId]
    ELitState _ -> []
    EGetState e -> referencedEntitiesIn e
    ESetState expr _ -> referencedEntitiesIn expr
    EDoNothing -> []
    EEqual e1 e2 -> referencedEntitiesIn e1 ++ referencedEntitiesIn e2
    EIf eCond eThen eElse ->
      referencedEntitiesIn eCond
        ++ referencedEntitiesIn eThen
        ++ referencedEntitiesIn eElse

instance HasLocations (Exp t) where
  extractLocations = \case
    ELitEntity _ -> []
    ELitState _ -> []
    EGetState (_ :@ loc) -> [loc]
    ESetState expr _ -> extractLocations expr
    EDoNothing -> []
    EEqual e1 e2 -> extractLocations e1 ++ extractLocations e2
    EIf eCond eThen eElse ->
      extractLocations eCond
        ++ extractLocations eThen
        ++ extractLocations eElse

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

data Exp :: T -> Type where
  -- Literals
  ELitEntity :: EntityId -> Exp 'TEntity
  ELitState :: ToggleState -> Exp 'TState
  -- Entity properties
  EGetState :: Located (Exp 'TEntity) -> Exp 'TState
  -- Actions
  ESetState :: Located (Exp 'TEntity) -> Located (Exp 'TState) -> Exp 'TAction
  EDoNothing :: Exp 'TAction
  -- Boolean logic
  EEqual :: Located (Exp 'TState) -> Located (Exp 'TState) -> Exp 'TBool
  EIf :: -- Nested locs to record both the if-then-else syntax and its operands.
    Located (Located (Exp 'TBool)) ->
    Located (Located (Exp 'TAction)) ->
    Located (Located (Exp 'TAction)) ->
    Exp 'TAction

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

deriving instance Ord (Exp t)

--------------------------------------------------------------------------------
-- Syntax

-- | Things that uniquely reference an entity.
class IntoEntity a where
  toEntity :: (HasCallStack) => a -> Located (Exp 'TEntity)

-- | A named entity.
instance IntoEntity Text where
  toEntity = (:@ captureSrcSpan) . ELitEntity . EntityId

-- | An identified entity.
instance IntoEntity EntityId where
  toEntity = (:@ captureSrcSpan) . ELitEntity

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
shouldBe :: (HasCallStack, IntoEntity e) => e -> Located (Exp 'TState) -> Located (Exp 'TAction)
shouldBe entity state = ESetState (toEntity entity) state :@ captureSrcSpan

-- | Gets the current toggle state of the given entity.
toggledStateOf :: (HasCallStack, IntoEntity e) => e -> Located (Exp 'TState)
toggledStateOf entity = EGetState (toEntity entity) :@ captureSrcSpan

-- | Check for equality.
is :: (HasCallStack) => Located (Exp 'TState) -> Located (Exp 'TState) -> Located (Exp 'TBool)
is s1 s2 = EEqual s1 s2 :@ captureSrcSpan

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
