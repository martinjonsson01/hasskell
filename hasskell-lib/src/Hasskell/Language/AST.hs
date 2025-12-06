{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hasskell.Language.AST
  ( Specification (..),
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
import Data.List (singleton)
import Data.Singletons.TH
import Data.Text (Text)
import GHC.Stack
import Hasskell.HomeAssistant.API
import Error.Diagnose (Position (..))

data T = TDevice | TEntity | TVoid
  deriving (Show, Eq)

genSingletons [''T]

-- | A specification defining rules for desired world states.
data Specification = Specification
  { specPolicies :: [Policy]
  }
  deriving (Show)

instance Semigroup Specification where
  (Specification policiesA) <> (Specification policiesB) = Specification $ policiesA <> policiesB

instance Monoid Specification where
  mempty = Specification []

data Policy = Policy {name :: Text, expression :: SomeExp}
  deriving (Show)

-- | Declare a desired state.
policy :: (SingI t) => Text -> Exp t -> Specification
policy name expr = Specification . singleton $ Policy name (SomeExp expr)

data SomeExp :: Type where
  SomeExp :: (SingI t) => Exp t -> SomeExp

instance Show (SomeExp) where
  show (SomeExp e) = show e

data Exp :: T -> Type where
  EDevice :: Text -> Exp 'TDevice
  EEntity :: Position -> EntityId -> Exp 'TEntity
  EIsOn :: Position -> Exp 'TEntity -> Exp 'TVoid

deriving instance Show (Exp t)

deriving instance Eq (Exp t)

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

--------------------------------------------------------------------------------

captureSrcSpan :: (HasCallStack) => Position
captureSrcSpan = case reverse (getCallStack callStack) of
  ( _,
    SrcLoc
      { srcLocFile,
        srcLocStartLine,
        srcLocStartCol,
        srcLocEndLine,
        srcLocEndCol
      }
    )
    : _ -> Position (srcLocStartLine, srcLocStartCol) (srcLocEndLine, srcLocEndCol) srcLocFile
  _ -> Position (0, 0) (0,0) "<unknown>"
