{-# LANGUAGE TemplateHaskell #-}

module Hasskell.Effects.Time
  ( -- Time
    Time,
    getCurrentUTCTime,
    getCurrentTimeOfDay,
    runTimeIO,
  )
where

import Data.Time
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

data Time :: Effect where
  GetCurrentUTCTime :: Time m UTCTime
  GetCurrentTimeOfDay :: Time m TimeOfDay

makeEffect ''Time

runTimeIO ::
  (IOE :> es) =>
  Eff (Time : es) a ->
  Eff es a
runTimeIO = interpret_ $ \case
  GetCurrentUTCTime -> liftIO getCurrentTime
  GetCurrentTimeOfDay -> liftIO $ do
    time <- getCurrentTime
    timeZone <- getTimeZone time
    let utcTimeOfDay = timeToTimeOfDay (utctDayTime time)
        (_dayOffset, timeOfDay) = localToUTCTimeOfDay timeZone utcTimeOfDay
    pure timeOfDay
