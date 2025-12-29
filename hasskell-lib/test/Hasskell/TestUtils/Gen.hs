module Hasskell.TestUtils.Gen
  ( SomeToggleable (..),
    -- | World generators
    genWorldWithToggled,
    genWorldWithToggledAndTime,
    genWorldWithoutEntity,
    genWorldWithKnownEntities,
    genWorldWithToggleds,
    genWorldWithToggleds',
    genObservedWorld,
    genWorld,
    genTime,
    genToggleable,
    genEntityId,
    genUniqueEntity,
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
import Data.Singletons
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Tuple.HT
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

-- | An arbitrary entity that can be toggled.
data SomeToggleable where
  SomeToggleable ::
    (SingI t, HasEntityId (Exp t), Proved Toggleable t) =>
    Located (Exp t) ->
    SomeToggleable

deriving instance (Show SomeToggleable)

instance HasEntityId SomeToggleable where
  idOf (SomeToggleable e) = idOf e

genWorldWithToggled :: ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggled state = do
  entityId <- genEntity
  world <- genWorldWithToggleds' [(entityId, state)]
  pure (entityId, world)

genWorldWithToggledAndTime :: ToggleState -> (Int, Int) -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggledAndTime state (hours, mins) = do
  entity <- genEntity
  let timeOfDay = TimeOfDay hours mins 0
  world <- genTimedWorldWithToggleds timeOfDay [(entity, state)]
  pure (entity, world)

genWorldWithToggleds ::
  ( SingI t,
    HasEntityId (Exp t),
    Proved Toggleable t
  ) =>
  [(Located (Exp t), ToggleState)] ->
  Gen ObservedWorld
genWorldWithToggleds = genWorldWithToggleds' . map (mapFst SomeToggleable)

genWorldWithToggleds' :: [(SomeToggleable, ToggleState)] -> Gen ObservedWorld
genWorldWithToggleds' entitiesAndStates = do
  timeOfDay <- genTimeOfDay
  genTimedWorldWithToggleds timeOfDay entitiesAndStates

genTimedWorldWithToggleds :: TimeOfDay -> [(SomeToggleable, ToggleState)] -> Gen ObservedWorld
genTimedWorldWithToggleds timeOfDay entitiesAndStates = do
  world <- genWorld
  let extractId = mapFst idOf
      toggleables = HMap.fromList (map extractId $ map (uncurry (,)) entitiesAndStates)
      newToggleables = toggleables `HMap.union` (worldToggleables world)
  pure $ MkObserved timeOfDay world {worldToggleables = newToggleables}

genWorldWithKnownEntities :: (HasEntityId (Exp t)) => [Located (Exp t)] -> Gen ObservedWorld
genWorldWithKnownEntities known = do
  MkObserved timeOfDay world <- genObservedWorld
  let pairGen eId = (eId,) <$> genToggleState
  knownEntityMap <- HMap.fromList <$> mapM pairGen (map idOf known)
  pure $ MkObserved timeOfDay world {worldToggleables = knownEntityMap <> (worldToggleables world)}

genObservedWorld :: Gen ObservedWorld
genObservedWorld = MkObserved <$> genTimeOfDay <*> genWorld

genWorldWithoutEntity :: Gen (SomeToggleable, ObservedWorld)
genWorldWithoutEntity = do
  world <- genObservedWorld
  entity <- genUniqueEntity world
  pure (entity, world)

genWorld :: Gen World
genWorld = MkWorld . HMap.fromList <$> Gen.list (Range.linear 0 10) genToggleable

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = genTime >>= pure . timeToTimeOfDay . utctDayTime

genTime :: Gen UTCTime
genTime =
  posixSecondsToUTCTime . fromInteger
    <$> Gen.integral (Range.constant 0 (60 * 60 * 24 * 365 * 200)) -- up to year 2169

genUniqueEntity :: ObservedWorld -> Gen SomeToggleable
genUniqueEntity (MkObserved _ MkWorld {worldToggleables}) =
  let idExists eId = isJust $ HMap.lookup eId worldToggleables
   in Gen.filter (not . idExists . idOf) genEntity

genToggleable :: Gen (EntityId, ToggleState)
genToggleable = (,) <$> genEntityId <*> genToggleState

genEntityId :: Gen EntityId
genEntityId = makeEntityIdUnsafe <$> Gen.text (Range.constant 1 10) Gen.alphaNum

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
  SomeToggleable expr <- genKnownEntity knownEntities
  state <- genStateExp
  pure (ESetState expr state :@ loc)

genKnownEntity :: NonEmpty EntityId -> Gen SomeToggleable
genKnownEntity knownEntities = do
  eId <- genKnownEntityId knownEntities
  genEntityWithId eId

genEntityWithId :: EntityId -> Gen SomeToggleable
genEntityWithId eId = SomeToggleable . (ELitEntityLight eId :@) <$> genLocation

genEntity :: Gen SomeToggleable
genEntity = genEntityId >>= genEntityWithId

genKnownEntityId :: NonEmpty EntityId -> Gen EntityId
genKnownEntityId knownEntities = do
  index <- Gen.int (Range.constant 0 (length knownEntities - 1))
  pure $ knownEntities NE.!! index

genLocation :: Gen Location
genLocation = Location <$> genPosition <*> pure mempty

genPosition :: Gen Position
genPosition = pure $ Position {begin = (-1, -1), end = (-1, -1), file = "Generated"}
