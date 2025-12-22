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

import Data.HashMap.Strict qualified as HMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
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
  let toggleables = HMap.fromList (map (uncurry (,)) entitiesAndStates)
      newToggleables = toggleables `HMap.union` (worldToggleables world)
  pure $ MkObserved time world {worldToggleables = newToggleables}

genWorldWithKnownEntities :: [EntityId] -> Gen ObservedWorld
genWorldWithKnownEntities known = do
  MkObserved time world <- genObservedWorld
  knownEntities <- HMap.fromList <$> mapM genEntityWithId known
  pure $ MkObserved time world {worldToggleables = knownEntities <> (worldToggleables world)}

genObservedWorld :: Gen ObservedWorld
genObservedWorld = MkObserved <$> genTime <*> genWorld

genWorldWithoutEntity :: Gen (EntityId, ObservedWorld)
genWorldWithoutEntity = do
  world <- genObservedWorld
  entity <- genUniqueEntityId world
  pure (entity, world)

genWorld :: Gen World
genWorld = MkWorld . HMap.fromList <$> Gen.list (Range.linear 0 10) genToggleable

genTime :: Gen UTCTime
genTime =
  posixSecondsToUTCTime . fromInteger
    <$> Gen.integral (Range.constant 0 (60 * 60 * 24 * 365 * 200)) -- up to year 2169

genUniqueEntityId :: ObservedWorld -> Gen EntityId
genUniqueEntityId (MkObserved _ MkWorld {worldToggleables}) =
  let idExists eId = isJust $ HMap.lookup eId worldToggleables
   in Gen.filter (not . idExists) genEntityId

genToggleable :: Gen (EntityId, ToggleState)
genToggleable = genEntityId >>= genEntityWithId

genEntityWithId :: EntityId -> Gen (EntityId, ToggleState)
genEntityWithId entity = (,) <$> pure entity <*> genToggleState

genEntityId :: Gen EntityId
genEntityId = EntityId <$> Gen.text (Range.constant 1 10) Gen.alphaNum

genToggleState :: Gen ToggleState
genToggleState = Gen.choice [pure On, pure Off]

----------------------------------------------------------------------

genSpecWithPolicy :: ObservedWorld -> Specification -> Gen Specification
genSpecWithPolicy (MkObserved _ world) includePolicy = do
  let entitiesToExclude = HMap.fromList $ map (,Off) (referencedEntitiesIn includePolicy)
      knownEntitiesMap = worldToggleables world `HMap.difference` entitiesToExclude
  case NE.nonEmpty (HMap.keys knownEntitiesMap) of
    Just knownEntities -> do
      policies <- Gen.list (Range.constant 0 10) (genPolicy knownEntities)
      mconcat <$> genWithInserted [includePolicy] policies
    Nothing -> pure mempty

genPolicy :: NonEmpty EntityId -> Gen Specification
genPolicy knownEntities = policy <$> genPolicyName <*> genVoidExp knownEntities

genPolicyName :: Gen Text
genPolicyName = Gen.text (Range.constant 1 10) Gen.alphaNum

genStateExp :: Gen (Located (Exp 'TState))
genStateExp = do
  loc <- genLocation
  state <- genToggleState
  pure (ELitState state :@ loc)

genVoidExp :: NonEmpty EntityId -> Gen (Located (Exp 'TAction))
genVoidExp knownEntities = do
  loc <- genLocation
  expr <- genEntityExp knownEntities
  state <- genStateExp
  pure (ESetState expr state :@ loc)

genEntityExp :: NonEmpty EntityId -> Gen (Located (Exp 'TEntity))
genEntityExp knownEntities = do
  loc <- genLocation
  eId <- genKnownEntityId knownEntities
  pure (ELitEntity eId :@ loc)

genKnownEntityId :: NonEmpty EntityId -> Gen EntityId
genKnownEntityId knownEntities = do
  index <- Gen.int (Range.constant 0 (length knownEntities - 1))
  pure $ knownEntities NE.!! index

genLocation :: Gen Location
genLocation = Location <$> genPosition <*> pure mempty

genPosition :: Gen Position
genPosition = pure $ Position {begin = (-1, -1), end = (-1, -1), file = "Generated"}
