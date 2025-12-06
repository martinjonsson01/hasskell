module Hasskell.TestUtils.Specifications
  ( lightAlwaysOn,
  )
where

import Hasskell

lightAlwaysOn :: (IntoEntity a) => a -> Specification
lightAlwaysOn light = policy "light is always on" (isOn $ toEntity light)
