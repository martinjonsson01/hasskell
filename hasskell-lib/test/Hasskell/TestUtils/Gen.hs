module Hasskell.TestUtils.Gen
  ( -- | World generators
    genWorldWithToggled,
    genWorldWithoutEntity,
    genWorldWithKnownEntities,
    genWorldWithToggleds,
    genObservedWorld,
    genWorld,
    genTime,
    genToggleable,
    genEntityId,
    genUniqueEntityId,
    genToggleState,
    -- | Specification generators
    genSpecWithPolicy,
    genPolicy,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.World
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

----------------------------------------------------------------------

genWithInserted :: [a] -> [a] -> Gen [a]
genWithInserted toInsert items = do
  index <- Gen.int (Range.constant 0 (length items))
  let (front, back) = splitAt index items
   in pure $ front ++ toInsert ++ back

----------------------------------------------------------------------

genWorldWithToggled :: ToggleState -> Gen (EntityId, ObservedWorld)
genWorldWithToggled state = do
  entityId <- genEntityId
  world <- genWorldWithToggleds [(entityId, state)]
  pure (entityId, world)

genWorldWithToggleds :: [(EntityId, ToggleState)] -> Gen ObservedWorld
genWorldWithToggleds entitiesAndStates = do
  MkObserved time world <- genObservedWorld
  let toggleables = map (uncurry Toggleable) entitiesAndStates
  newToggleables <- genWithInserted toggleables (worldToggleables world)
  pure $ MkObserved time world {worldToggleables = newToggleables}

genWorldWithKnownEntities :: [EntityId] -> Gen ObservedWorld
genWorldWithKnownEntities known = do
  MkObserved time world <- genObservedWorld
  knownEntities <- mapM genEntityWithId known
  pure $ MkObserved time world {worldToggleables = knownEntities <> (worldToggleables world)}

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
genToggleable = genEntityId >>= genEntityWithId

genEntityWithId :: EntityId -> Gen Toggleable
genEntityWithId entity = Toggleable <$> pure entity <*> genToggleState

genEntityId :: Gen EntityId
genEntityId = EntityId <$> Gen.text (Range.constant 1 10) Gen.alphaNum

genToggleState :: Gen ToggleState
genToggleState = Gen.choice [pure On, pure Off]

----------------------------------------------------------------------

genSpecWithPolicy :: ObservedWorld -> Specification -> Gen Specification
genSpecWithPolicy (MkObserved _ world) includePolicy = do
  let entitiesToExclude = referencedEntitiesIn includePolicy
  case NE.nonEmpty (filter (not . (`elem` entitiesToExclude)) . map toggleableId $ worldToggleables world) of
    Just knownEntities -> do
      policies <- Gen.list (Range.constant 0 10) (genPolicy knownEntities)
      mconcat <$> genWithInserted [includePolicy] policies
    Nothing -> pure mempty

genPolicy :: NonEmpty EntityId -> Gen Specification
genPolicy knownEntities = policy <$> genPolicyName <*> genVoidExp knownEntities

genPolicyName :: Gen Text
genPolicyName = Gen.text (Range.constant 1 10) Gen.alphaNum

genVoidExp :: NonEmpty EntityId -> Gen (Exp 'TVoid)
genVoidExp knownEntities = EIsOn <$> genLocation <*> genEntityExp knownEntities

genEntityExp :: NonEmpty EntityId -> Gen (Exp 'TEntity)
genEntityExp knownEntities = EEntity <$> genLocation <*> genKnownEntityId knownEntities

genKnownEntityId :: NonEmpty EntityId -> Gen EntityId
genKnownEntityId knownEntities = do
  index <- Gen.int (Range.constant 0 (length knownEntities - 1))
  pure $ knownEntities NE.!! index

genLocation :: Gen Location
genLocation = Location <$> genPosition <*> pure mempty

genPosition :: Gen Position
genPosition = pure $ Position {begin = (-1, -1), end = (-1, -1), file = "Generated"}
