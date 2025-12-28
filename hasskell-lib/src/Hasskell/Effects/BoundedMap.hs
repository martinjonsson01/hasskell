module Hasskell.Effects.BoundedMap
  ( BoundedMap,
    newBoundedMap,
    insert,
    remove,
  )
where

import Control.Monad
import Data.Hashable
import Effectful
import Effectful.Concurrent.STM
import StmContainers.Map qualified as SM

data BoundedMap k v = BoundedMap
  { innerMap :: SM.Map k v,
    maxCapacity :: Int,
    currentCapacity :: TVar Int
  }

-- | Creates a new empty map with the given maximum capacity.
newBoundedMap :: (Concurrent :> es) => Int -> Eff es (BoundedMap k v)
newBoundedMap capacity =
  atomically $
    BoundedMap <$> SM.new <*> pure capacity <*> newTVar 0

-- | Inserts a value into the map.
-- NOTE: will retry if the map is at max capacity.
insert :: (Hashable k, Concurrent :> es) => k -> v -> BoundedMap k v -> Eff es ()
insert key value BoundedMap {innerMap, maxCapacity, currentCapacity} = atomically $ do
  capacity <- readTVar currentCapacity
  when (capacity >= maxCapacity) retry
  modifyTVar currentCapacity (+ 1)
  SM.insert value key innerMap

-- | Removes the specified mapping, retrying until if it is not currently present.
remove :: (Hashable k, Concurrent :> es) => k -> BoundedMap k v -> Eff es v
remove key BoundedMap {innerMap, currentCapacity} = atomically $ do
  SM.lookup key innerMap >>= \case
    Just value -> do
      SM.delete key innerMap
      modifyTVar currentCapacity (subtract 1)
      pure value
    Nothing -> retry
