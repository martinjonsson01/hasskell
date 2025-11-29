{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.Counter
  ( -- Counter
    CorrelationId (..),
    CorrelationIdSource,
    newId,
    runCounter,
  )
where

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.TH

-- | Unique ID associated with a sent message.
newtype CorrelationId = CorrelationId Int
  deriving newtype (Eq, Show, Num)

data CorrelationIdSource :: Effect where
  NewId :: CorrelationIdSource m CorrelationId

makeEffect ''CorrelationIdSource

runCounter :: (Concurrent :> es) => Eff (CorrelationIdSource : es) a -> Eff es a
runCounter counter = do
  var <- newTVarIO (1 :: CorrelationId) -- Has to start at 1, 0 results in "Message incorrectly formatted."
  interpretWith_ counter $ \case
    NewId -> atomically $ do
      corrId <- readTVar var
      modifyTVar var (+ 1)
      pure corrId
