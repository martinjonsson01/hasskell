module Hasskell.TestUtils.Specifications
  ( lightAlwaysOn,
  )
where

import GHC.Stack
import Hasskell

lightAlwaysOn :: (HasCallStack, IntoEntity a) => a -> Specification
lightAlwaysOn light = policy "light is always on" (isOn $ toEntity light)
