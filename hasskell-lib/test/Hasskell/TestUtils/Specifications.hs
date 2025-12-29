module Hasskell.TestUtils.Specifications
  ( lightAlwaysOn,
    lightAlways,
  )
where

import Data.Singletons
import GHC.Stack
import Hasskell
import Hasskell.Language.AST

lightAlwaysOn ::
  ( HasCallStack,
    SingI t,
    Proved Toggleable t
  ) =>
  Located (Exp t) ->
  Specification
lightAlwaysOn = lightAlways On

lightAlways ::
  ( HasCallStack,
    SingI t,
    Proved Toggleable t
  ) =>
  ToggleState ->
  Located (Exp t) ->
  Specification
lightAlways state entity =
  policy
    ("light is always " <> if state == On then "on" else "off")
    (entity `shouldBe` fromState state)
