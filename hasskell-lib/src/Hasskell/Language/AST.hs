{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hasskell.Language.AST
  ( Specification (..),
    Policy (..),
    policy,
    HasReferencedEntities (..),
    SomeExp (..),
    T (..),
    Exp (..),
    IntoEntity (..),
    isOn,
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
import Hasskell.Language.Diagnostic
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

data Policy = Policy {name :: Text, expression :: Exp 'TVoid}
  deriving (Eq, Ord, Show)

-- | Declare a desired state.
policy :: Text -> Exp 'TVoid -> Specification
policy name expr = Specification . singleton $ Policy name expr

class HasReferencedEntities a where
  referencedEntitiesIn :: a -> [EntityId]

instance HasReferencedEntities Specification where
  referencedEntitiesIn Specification {specPolicies} = concatMap referencedEntitiesIn specPolicies

instance HasReferencedEntities Policy where
  referencedEntitiesIn Policy {expression} = referencedEntitiesIn expression

instance HasReferencedEntities (Exp t) where
  referencedEntitiesIn = \case
    EEntity _ eId -> [eId]
    EIsOn _ expr -> referencedEntitiesIn expr

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
  EEntity :: Positions -> EntityId -> Exp 'TEntity
  EIsOn :: Positions -> Exp 'TEntity -> Exp 'TVoid

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

deriving instance Ord (Exp t)

-- | Things that uniquely reference an entity.
class IntoEntity a where
  toEntity :: (HasCallStack) => a -> Exp 'TEntity

-- | A named entity.
instance IntoEntity Text where
  toEntity = EEntity captureSrcSpan . EntityId

-- | An identified entity.
instance IntoEntity EntityId where
  toEntity = EEntity captureSrcSpan

-- | Turn on a given entity.
isOn :: (HasCallStack) => Exp 'TEntity -> Exp 'TVoid
isOn = EIsOn captureSrcSpan
