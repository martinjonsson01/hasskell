module Hasskell.TestUtils.Gen
  ( withEntityOn,
    genWorldWithToggled,
    genObservedWorld,
    genWorld,
    genTime,
    genToggleable,
    genEntityId,
    genToggleState,
  )
where

import Data.Time
import Data.Time.Clock.POSIX
import Hasskell.HomeAssistant.API
import Hasskell.Language.Interpreter
import Hasskell.Language.World
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

withEntityOn :: ObservedWorld -> EntityId -> DesiredWorld
withEntityOn (MkObserved _ world) entityId =
  MkDesired $ world {worldToggleables = map turnOnEntity (worldToggleables world)}
  where
    turnOnEntity entity | toggleableId entity == entityId = entity {toggleableState = On}
    turnOnEntity entity | otherwise = entity

genWorldWithToggled :: ToggleState -> Gen (EntityId, ObservedWorld)
genWorldWithToggled state = do
  MkObserved time world <- genObservedWorld
  entityId <- genEntityId
  index <- Gen.int (Range.constant 0 (length (worldToggleables world)))
  let toggleable = Toggleable entityId state
      newToggleables = insertAt index toggleable (worldToggleables world)
      observed = MkObserved time world {worldToggleables = newToggleables}
  pure $ (entityId, observed)
  where
    insertAt :: Int -> a -> [a] -> [a]
    insertAt i x xs =
      let (front, back) = splitAt i xs
       in front ++ (x : back)

genObservedWorld :: Gen ObservedWorld
genObservedWorld = MkObserved <$> genTime <*> genWorld

genWorld :: Gen World
genWorld = MkWorld <$> Gen.list (Range.linear 0 10) genToggleable

genTime :: Gen UTCTime
genTime =
  posixSecondsToUTCTime . fromInteger
    <$> Gen.integral (Range.constant 0 (60 * 60 * 24 * 365 * 200)) -- up to year 2169

genToggleable :: Gen Toggleable
genToggleable = Toggleable <$> genEntityId <*> genToggleState

genEntityId :: Gen EntityId
genEntityId = EntityId <$> Gen.text (Range.constant 1 10) Gen.alphaNum

genToggleState :: Gen ToggleState
genToggleState = Gen.choice [pure On, pure Off]
