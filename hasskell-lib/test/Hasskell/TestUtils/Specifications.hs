module Hasskell.TestUtils.Specifications
  ( lightAlwaysOn,
    lightAlways,
  )
where

import GHC.Stack
import Hasskell

lightAlwaysOn :: (HasCallStack, IntoEntity a) => a -> Specification
lightAlwaysOn = lightAlways On

lightAlways :: (HasCallStack, IntoEntity a) => ToggleState -> a -> Specification
lightAlways state light =
  policy
    ("light is always " <> if state == On then "on" else "off")
    (light `shouldBe` state)
