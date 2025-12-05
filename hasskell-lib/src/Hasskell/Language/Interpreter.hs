module Hasskell.Language.Interpreter
  ( interpret,
    DesiredWorld (..),
  )
where

import Hasskell.Language.AST (Specification)
import Hasskell.Language.World (ObservedWorld, World)

-- | Describes the state of an ideal world.
newtype DesiredWorld = MkDesired World
  deriving (Eq, Show)

-- | Derives a desired world state from an observed one,
-- given a certain specification.
interpret :: Specification -> ObservedWorld -> DesiredWorld
interpret = undefined
