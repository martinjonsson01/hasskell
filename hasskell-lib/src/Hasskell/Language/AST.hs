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
       data T = TEntity | TVoid

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

data Policy = Policy {name :: Text, expression :: Located (Exp 'TVoid)}
  deriving (Eq, Ord, Show)

-- | Declare a desired state.
policy :: Text -> Located (Exp 'TVoid) -> Specification
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
    EEntity eId -> [eId]
    EShouldBe expr _ -> referencedEntitiesIn expr

instance HasLocations (Exp t) where
  extractLocations = \case
    EEntity _ -> []
    EShouldBe expr _ -> extractLocations expr

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
  EEntity :: EntityId -> Exp 'TEntity
  EShouldBe :: Located (Exp 'TEntity) -> ToggleState -> Exp 'TVoid

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

deriving instance Ord (Exp t)

-- | Things that uniquely reference an entity.
class IntoEntity a where
  toEntity :: (HasCallStack) => a -> Located (Exp 'TEntity)

-- | A named entity.
instance IntoEntity Text where
  toEntity = (:@ captureSrcSpan) . EEntity . EntityId

-- | An identified entity.
instance IntoEntity EntityId where
  toEntity = (:@ captureSrcSpan) . EEntity

-- | Declare that a given entity should be in a given state.
shouldBe :: (HasCallStack) => Located (Exp 'TEntity) -> ToggleState -> Located (Exp 'TVoid)
shouldBe entity state = EShouldBe entity state :@ captureSrcSpan
