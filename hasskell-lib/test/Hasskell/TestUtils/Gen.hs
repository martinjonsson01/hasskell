module Hasskell.TestUtils.Gen
  ( SomeToggleable (..),
    -- | World generators
    genWorldWithToggled,
    genWorldWithToggledAndTime,
    genWorldWithEntity,
    genWorldWithoutEntity,
    genWorldWithKnownEntities,
    genWorldWithToggleds,
    genWorldWithToggleds',
    genObservedWorld,
    genWorld,
    genTime,
    -- | Entity generators
    genEntityId,
    genToggleState,
    genToggleable,
    genUniqueEntity,
    genWorldWithToggledLight,
    genWorldWithToggledInputBoolean,
    -- | Specification generators
    genSpecWithPolicy,
    genPolicy,
  )
where

import Data.HashMap.Strict qualified as HMap
import Data.List qualified as List
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

discardMiddle :: (a, b, c) -> (a, c)
discardMiddle (a, _, c) = (a, c)

genWorldWithToggledLight :: ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggledLight = (discardMiddle <$>) . genWorldWithToggled' genLight'

genWorldWithToggledInputBoolean :: ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggledInputBoolean = (discardMiddle <$>) . genWorldWithToggled' genInputBoolean'

genWorldWithToggled :: ToggleState -> Gen (SomeToggleable, HASSDomain, ObservedWorld)
genWorldWithToggled = genWorldWithToggled' genEntity'

genWorldWithToggled' :: Gen (SomeToggleable, HASSDomain) -> ToggleState -> Gen (SomeToggleable, HASSDomain, ObservedWorld)
genWorldWithToggled' entityGen state = do
  (entityId, domain) <- entityGen
  world <- genWorldWithToggleds' [(entityId, state)]
  pure (entityId, domain, world)

genWorldWithToggledAndTime :: ToggleState -> (Int, Int) -> Gen (SomeToggleable, HASSDomain, ObservedWorld)
genWorldWithToggledAndTime state (hours, mins) = do
  (entity, domain) <- genEntity'
  let timeOfDay = TimeOfDay hours mins 0
  world <- genTimedWorldWithToggleds timeOfDay [(entity, state)]
  pure (entity, domain, world)

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

genWorldWithEntity :: (HasEntityId (Exp t)) => Located (Exp t) -> Gen ObservedWorld
genWorldWithEntity = genWorldWithKnownEntities . List.singleton

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

genEntityWithId' :: EntityId -> Gen (SomeToggleable, HASSDomain)
genEntityWithId' eId =
  Gen.choice
    [ genLightWithId' eId,
      genInputBooleanWithId' eId
    ]

genEntityWithId :: EntityId -> Gen SomeToggleable
genEntityWithId eId = Gen.choice [genLightWithId eId, genInputBooleanWithId eId]

genLightWithId' :: EntityId -> Gen (SomeToggleable, HASSDomain)
genLightWithId' eId =
  (,domainLight)
    <$> SomeToggleable . (ELitEntityLight eId :@)
    <$> genLocation

genLightWithId :: EntityId -> Gen SomeToggleable
genLightWithId = (fst <$>) . genLightWithId'

genInputBooleanWithId :: EntityId -> Gen SomeToggleable
genInputBooleanWithId = (fst <$>) . genInputBooleanWithId'

genInputBooleanWithId' :: EntityId -> Gen (SomeToggleable, HASSDomain)
genInputBooleanWithId' eId =
  (,domainInputBoolean)
    <$> SomeToggleable . (ELitEntityInputBoolean eId :@)
    <$> genLocation

genEntity' :: Gen (SomeToggleable, HASSDomain)
genEntity' = genEntityId >>= genEntityWithId'

genEntity :: Gen SomeToggleable
genEntity = fst <$> genEntity'

genLight' :: Gen (SomeToggleable, HASSDomain)
genLight' = genEntityId >>= genLightWithId'

genInputBoolean' :: Gen (SomeToggleable, HASSDomain)
genInputBoolean' = genEntityId >>= genInputBooleanWithId'

genKnownEntityId :: NonEmpty EntityId -> Gen EntityId
genKnownEntityId knownEntities = do
  index <- Gen.int (Range.constant 0 (length knownEntities - 1))
  pure $ knownEntities NE.!! index

genLocation :: Gen Location
genLocation = Location <$> genPosition <*> pure mempty

genPosition :: Gen Position
genPosition = pure $ Position {begin = (-1, -1), end = (-1, -1), file = "Generated"}
