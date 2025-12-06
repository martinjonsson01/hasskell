module Hasskell.TestUtils.Gen
  ( genWorldWithToggled,
    genWorldWithoutEntity,
    genObservedWorld,
    genWorld,
    genTime,
    genToggleable,
    genEntityId,
    genUniqueEntityId,
    genToggleState,
  )
where

import Data.Time
import Data.Time.Clock.POSIX
import Hasskell.HomeAssistant.API
import Hasskell.Language.World
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

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

genWorldWithoutEntity :: Gen (EntityId, ObservedWorld)
genWorldWithoutEntity = do
  world <- genObservedWorld
  entity <- genUniqueEntityId world
  pure (entity, world)

genWorld :: Gen World
genWorld = MkWorld <$> Gen.list (Range.linear 0 10) genToggleable

genTime :: Gen UTCTime
genTime =
  posixSecondsToUTCTime . fromInteger
    <$> Gen.integral (Range.constant 0 (60 * 60 * 24 * 365 * 200)) -- up to year 2169

genUniqueEntityId :: ObservedWorld -> Gen EntityId
genUniqueEntityId (MkObserved _ MkWorld {worldToggleables}) =
  let existingIds = map toggleableId worldToggleables
   in Gen.filter (not . (`elem` existingIds)) genEntityId

genToggleable :: Gen Toggleable
genToggleable = Toggleable <$> genEntityId <*> genToggleState

genEntityId :: Gen EntityId
genEntityId = EntityId <$> Gen.text (Range.constant 1 10) Gen.alphaNum

genToggleState :: Gen ToggleState
genToggleState = Gen.choice [pure On, pure Off]
