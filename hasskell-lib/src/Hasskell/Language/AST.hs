{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hasskell.Language.AST
  ( Specification,
    policies,
    Policy,
    policy,
    SomeExp,
    T (..),
    Exp,
    entity,
    turnOn,
  )
where

import Data.Kind (Type)
import Data.Singletons.TH
import Data.Text (Text)

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

data Policy = Declare {name :: Text, expression :: SomeExp}
  deriving (Show)

-- | Declare a desired state.
policy :: (SingI t) => Text -> Exp t -> Policy
policy name expr = Declare name (SomeExp expr)

data SomeExp :: Type where
  SomeExp :: (SingI t) => Exp t -> SomeExp

instance Show (SomeExp) where
  show (SomeExp e) = show e

data Exp :: T -> Type where
  EDevice :: Text -> Exp 'TDevice
  EEntity :: Text -> Exp 'TEntity
  ETurnOn :: Exp 'TEntity -> Exp 'TVoid

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

-- | A named entity.
entity :: Text -> Exp 'TEntity
entity = EEntity

-- | Turn on a given entity.
turnOn :: Exp 'TEntity -> Exp 'TVoid
turnOn = ETurnOn
