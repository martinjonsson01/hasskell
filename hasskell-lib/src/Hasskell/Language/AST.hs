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
    shouldBe,
    toggledStateOf,
    eIf,
    ifElse,
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

-- | Declare a desired state.
policy :: Text -> Located (Exp 'TAction) -> Specification
policy name expr = Specification . singleton $ Policy name expr

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
    ESetState expr _ -> referencedEntitiesIn expr

instance HasLocations (Exp t) where
  extractLocations = \case
    ELitEntity _ -> []
    ESetState expr _ -> extractLocations expr

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
  EIf :: Located (Exp 'TBool) -> Located (Exp 'TAction) -> Located (Exp 'TAction) -> Exp 'TAction

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

deriving instance Ord (Exp t)

-- | Things that uniquely reference an entity.
class IntoEntity a where
  toEntity :: (HasCallStack) => a -> Located (Exp 'TEntity)

-- | A named entity.
instance IntoEntity Text where
  toEntity = (:@ captureSrcSpan) . ELitEntity . EntityId

-- | An identified entity.
instance IntoEntity EntityId where
  toEntity = (:@ captureSrcSpan) . ELitEntity

-- | Things that describe a specific state.
class IntoState a where
  toState :: (HasCallStack) => a -> Located (Exp 'TState)

instance IntoState ToggleState where
  toState = (:@ captureSrcSpan) . ELitState

instance IntoState (Located (Exp 'TState)) where
  toState = id

-- | Declare that a given entity should be in a given state.
shouldBe :: (HasCallStack, IntoEntity e, IntoState s) => e -> s -> Located (Exp 'TAction)
shouldBe entity state = ESetState (toEntity entity) (toState state) :@ captureSrcSpan

-- | Gets the current toggle state of the given entity.
toggledStateOf :: (HasCallStack, IntoEntity e) => e -> Located (Exp 'TState)
toggledStateOf entity = EGetState (toEntity entity) :@ captureSrcSpan

-- | Check for equality.
is :: (HasCallStack, IntoState s1, IntoState s2) => s1 -> s2 -> Located (Exp 'TBool)
is s1 s2 = EEqual (toState s1) (toState s2) :@ captureSrcSpan

-- | Make a policy conditional on something.
eIf :: (HasCallStack) => Located (Exp 'TBool) -> Located (Exp 'TAction) -> Located (Exp 'TAction)
eIf cond action = ifElse cond action (EDoNothing :@ captureSrcSpan)

-- | Choose an action depending on a condition.
ifElse :: (HasCallStack) => Located (Exp 'TBool) -> Located (Exp 'TAction) -> Located (Exp 'TAction) -> Located (Exp 'TAction)
ifElse cond thenExp elseExp = EIf cond thenExp elseExp :@ captureSrcSpan
