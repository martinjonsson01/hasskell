{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hasskell.Language.AST
  ( Specification (..),
    policies,
    Policy (..),
    policy,
    SomeExp (..),
    T (..),
    Exp (..),
    IntoEntity (..),
    isOn,
  )
where

import Data.Kind (Type)
import Data.Singletons.TH
import Data.Text (Text)
import Hasskell.HomeAssistant.API

data T = TDevice | TEntity | TVoid
  deriving (Show, Eq)

genSingletons [''T]

-- | A specification defining rules for desired world states.
data Specification = Specification
  { specPolicies :: [Policy]
  }
  deriving (Show)

-- | Create a specification consisting of several state policies.
policies :: [Policy] -> Specification
policies = Specification

data Policy = Policy {name :: Text, expression :: SomeExp}
  deriving (Show)

-- | Declare a desired state.
policy :: (SingI t) => Text -> Exp t -> Policy
policy name expr = Policy name (SomeExp expr)

data SomeExp :: Type where
  SomeExp :: (SingI t) => Exp t -> SomeExp

instance Show (SomeExp) where
  show (SomeExp e) = show e

data Exp :: T -> Type where
  EDevice :: Text -> Exp 'TDevice
  EEntity :: EntityId -> Exp 'TEntity
  EIsOn :: Exp 'TEntity -> Exp 'TVoid

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

-- | Things that uniquely reference an entity.
class IntoEntity a where
  toEntity :: a -> Exp 'TEntity

-- | A named entity.
instance IntoEntity Text where
  toEntity = EEntity . EntityId

-- | An identified entity.
instance IntoEntity EntityId where
  toEntity = EEntity

-- | Turn on a given entity.
isOn :: Exp 'TEntity -> Exp 'TVoid
isOn = EIsOn
