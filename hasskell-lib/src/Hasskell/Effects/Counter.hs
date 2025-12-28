{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.Counter
  ( -- Counter
    CorrelationId (..),
    Counter,
    newId,
    runCounter,
  )
where

import Data.Hashable
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.TH

-- | Unique ID associated with a sent message.
newtype CorrelationId = CorrelationId Int
  deriving newtype (Hashable, Eq, Show, Num)

data Counter :: Effect where
  NewId :: Counter m CorrelationId

makeEffect ''Counter

runCounter :: (Concurrent :> es) => Eff (Counter : es) a -> Eff es a
runCounter counter = do
  var <- newTVarIO (1 :: CorrelationId) -- Has to start at 1, 0 results in "Message incorrectly formatted."
  interpretWith_ counter $ \case
    NewId -> atomically $ do
      corrId <- readTVar var
      modifyTVar var (+ 1)
      pure corrId
