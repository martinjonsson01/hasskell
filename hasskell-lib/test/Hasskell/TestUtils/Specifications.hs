module Hasskell.TestUtils.Specifications
  ( lightAlwaysOn,
    lightAlways,
    realEntityAlwaysOn,
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
  Located (Exp Raw t) ->
  RawSpecification
lightAlwaysOn = lightAlways On

lightAlways ::
  ( HasCallStack,
    SingI t,
    Proved Toggleable t
  ) =>
  ToggleState ->
  Located (Exp Raw t) ->
  RawSpecification
lightAlways state entity =
  policy
    ("light is always " <> if state == On then "on" else "off")
    (entity `shouldBe` fromState state)

realEntityAlwaysOn :: RawSpecification
realEntityAlwaysOn = lightAlwaysOn (inputBoolean "input_boolean.test")
