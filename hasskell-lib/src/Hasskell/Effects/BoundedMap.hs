module Hasskell.Effects.BoundedMap
  ( BoundedMap,
    newBoundedMap,
    insert,
    remove,
  )
where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Effectful
import Effectful.Concurrent.STM

data BoundedMap k v = BoundedMap
  { innerMap :: TVar (HashMap k v),
    maxCapacity :: Int,
    currentCapacity :: TVar Int
  }
  deriving (Eq)

-- | Creates a new empty map with the given maximum capacity.
newBoundedMap :: (Hashable k, Concurrent :> es) => Int -> Eff es (BoundedMap k v)
newBoundedMap capacity =
  atomically $
    BoundedMap <$> newTVar mempty <*> pure capacity <*> newTVar 0

-- | Inserts a value into the map.
-- NOTE: will retry if the map is at max capacity.
insert :: (Hashable k, Concurrent :> es) => k -> v -> BoundedMap k v -> Eff es ()
insert key value BoundedMap {innerMap, maxCapacity, currentCapacity} = atomically $ do
  capacity <- readTVar currentCapacity
  when (capacity >= maxCapacity) retry
  modifyTVar currentCapacity (+ 1)
  modifyTVar innerMap (HM.insert key value)

-- | Removes the specified mapping, retrying until if it is not currently present.
remove :: (Hashable k, Concurrent :> es) => k -> BoundedMap k v -> Eff es v
remove key BoundedMap {innerMap, currentCapacity} = atomically $ do
  hashMap <- readTVar innerMap
  case hashMap HM.!? key of
    Just value -> do
      modifyTVar innerMap (HM.delete key)
      modifyTVar currentCapacity (subtract 1)
      pure value
    Nothing -> retry
