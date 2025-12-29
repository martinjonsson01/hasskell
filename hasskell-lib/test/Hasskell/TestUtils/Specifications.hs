module Hasskell.TestUtils.Specifications
  ( lightAlwaysOn,
    lightAlways,
  )
where

import GHC.Stack
import Hasskell
import Hasskell.Language.AST

lightAlwaysOn :: (HasCallStack) => Located (Exp 'TEntityLight) -> Specification
lightAlwaysOn = lightAlways On

lightAlways :: (HasCallStack) => ToggleState -> Located (Exp 'TEntityLight) -> Specification
lightAlways state entity =
  policy
    ("light is always " <> if state == On then "on" else "off")
    (entity `shouldBe` fromState state)
