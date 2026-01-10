module Hasskell.TestUtils.Gen
  ( SomeToggleable (..),
    -- | World generators
    genWorldWithToggled,
    genWorldWithToggledAndTime,
    genWorldWithEntity,
    genWorldWithoutEntity,
    genWorldWithoutThisEntity,
    genWorldWithKnownEntities,
    genWorldWithToggleds,
    genObservedWorld,
    genWorld,
    genTime,
    -- | Entity generators
    genEntities,
    genEntityId,
    genEntityWithState,
    genEntity,
    genToggleState,
    genToggleable,
    genUniqueEntity,
    genWorldWithToggledLight,
    genWorldWithToggledInputBoolean,
    -- Entity helpers
    observedLight,
    observedInputBoolean,
    -- | Specification generators
    genSpecWithPolicy,
    genPolicy,
    -- | API response generators
    genStateChange,
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set qualified as S
import Data.Singletons
import Data.Text (Text)
import Data.Text qualified as T
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
import Prettyprinter (pretty)

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
    (SingI t, HasEntityId (Exp Raw t), Proved Toggleable t) =>
    { someEntityExpr :: Located (Exp Raw t),
      someObservedEntity :: ObservedEntity
    } ->
    SomeToggleable

deriving instance (Show SomeToggleable)

instance HasEntityId SomeToggleable where
  idOf (SomeToggleable e _) = idOf e

instance HasKnownEntityId SomeToggleable where
  knownIdOf = makeKnownEntityIdUnsafe . idOf

observedLight :: (HasEntityId e) => e -> ToggleState -> ObservedEntity
observedLight e state =
  ObservedEntity
    (makeKnownEntityIdUnsafe (idOf e))
    (HS.singleton domainLight)
    state

observedInputBoolean :: (HasEntityId e) => e -> ToggleState -> ObservedEntity
observedInputBoolean e state =
  ObservedEntity
    (makeKnownEntityIdUnsafe (idOf e))
    (HS.singleton domainInputBoolean)
    state

genWorldWithToggledLight :: ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggledLight = genWorldWithToggled' genLightWithState

genWorldWithToggledInputBoolean :: ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggledInputBoolean = genWorldWithToggled' genInputBooleanWithState

genWorldWithToggled :: ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggled = genWorldWithToggled' genEntityWithState

genWorldWithToggled' :: (ToggleState -> Gen SomeToggleable) -> ToggleState -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggled' entityGen state = do
  entity <- entityGen state
  world <- genWorldWithToggleds [someObservedEntity entity]
  pure (entity, world)

genWorldWithToggledAndTime :: ToggleState -> (Int, Int) -> Gen (SomeToggleable, ObservedWorld)
genWorldWithToggledAndTime state (hours, mins) = do
  entity <- genEntityWithState state
  let timeOfDay = TimeOfDay hours mins 0
  world <- genTimedWorldWithToggleds timeOfDay [someObservedEntity entity]
  pure (entity, world)

genWorldWithToggleds :: [ObservedEntity] -> Gen ObservedWorld
genWorldWithToggleds entitiesAndStates = do
  timeOfDay <- genTimeOfDay
  genTimedWorldWithToggleds timeOfDay entitiesAndStates

genTimedWorldWithToggleds :: TimeOfDay -> [ObservedEntity] -> Gen ObservedWorld
genTimedWorldWithToggleds timeOfDay entitiesAndStates = do
  world <- genWorld
  let toggleables = HMap.fromList (map toPairedWithId entitiesAndStates)
      newToggleables = toggleables `HMap.union` (worldEntities world)
  pure $ MkObserved timeOfDay world {worldEntities = newToggleables}

genWorldWithEntity :: (HasEntityId (Exp Raw t)) => Located (Exp Raw t) -> Gen ObservedWorld
genWorldWithEntity = genWorldWithKnownEntities . List.singleton

genWorldWithKnownEntities :: (HasEntityId (Exp Raw t)) => [Located (Exp Raw t)] -> Gen ObservedWorld
genWorldWithKnownEntities known = do
  MkObserved timeOfDay world <- genObservedWorld
  let pairGen eId = (eId,) <$> genObservedEntityWithId eId
  knownEntityMap <- HMap.fromList <$> mapM pairGen (map (makeKnownEntityIdUnsafe . idOf) known)
  pure $ MkObserved timeOfDay world {worldEntities = knownEntityMap <> (worldEntities world)}

genObservedWorld :: Gen ObservedWorld
genObservedWorld = MkObserved <$> genTimeOfDay <*> genWorld

genWorldWithoutThisEntity :: (Proved IsEntity t) => Located (Exp Raw t) -> Gen ObservedWorld
genWorldWithoutThisEntity entity = do
  world <- genObservedWorld
  let observed = observedWorld world
      observedEntities = worldEntities observed
      knownEntityId = makeKnownEntityIdUnsafe (idOf entity)
      worldWithoutEntity =
        world
          { observedWorld =
              observed {worldEntities = HMap.delete knownEntityId observedEntities}
          }
  pure worldWithoutEntity

genWorldWithoutEntity :: Gen (SomeToggleable, ObservedWorld)
genWorldWithoutEntity = do
  world <- genObservedWorld
  entity <- genUniqueEntity world
  pure (entity, world)

genWorld :: Gen World
genWorld = MkWorld <$> genEntities

genEntities :: Gen (HashMap KnownEntityId ObservedEntity)
genEntities =
  HMap.fromList
    <$> Gen.list
      (Range.linear 0 10)
      (toPairedWithId <$> genObservedEntity)

toPairedWithId :: ObservedEntity -> (KnownEntityId, ObservedEntity)
toPairedWithId entity = (knownIdOf entity, entity)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = genTime >>= pure . timeToTimeOfDay . utctDayTime

genTime :: Gen UTCTime
genTime =
  posixSecondsToUTCTime . fromInteger
    <$> Gen.integral (Range.constant 0 (60 * 60 * 24 * 365 * 200)) -- up to year 2169

genUniqueEntity :: ObservedWorld -> Gen SomeToggleable
genUniqueEntity (MkObserved _ MkWorld {worldEntities}) =
  let idExists eId = isJust $ HMap.lookup (makeKnownEntityIdUnsafe eId) worldEntities
   in Gen.filter (not . idExists . idOf) genEntity

genObservedEntity :: Gen ObservedEntity
genObservedEntity = genKnownEntityId >>= genObservedEntityWithId

genObservedEntityWithId :: KnownEntityId -> Gen ObservedEntity
genObservedEntityWithId eId =
  ObservedEntity eId
    <$> genDomains
    <*> genToggleState

genDomains :: Gen (HashSet HASSDomain)
genDomains = HS.fromList <$> (Gen.list (Range.constant 0 3) genDomain)

genDomain :: Gen HASSDomain
genDomain = Gen.choice [pure domainLight, pure domainInputBoolean]

genToggleable :: Gen (EntityId, ToggleState)
genToggleable = (,) <$> genEntityId <*> genToggleState

genEntityId :: Gen EntityId
genEntityId = EntityId <$> Gen.text (Range.constant 1 10) Gen.alphaNum

genKnownEntityId :: Gen KnownEntityId
genKnownEntityId = makeKnownEntityIdUnsafe <$> genEntityId

genToggleState :: Gen ToggleState
genToggleState = Gen.choice [pure On, pure Off]

----------------------------------------------------------------------

genSpecWithPolicy :: ObservedWorld -> RawSpecification -> Gen RawSpecification
genSpecWithPolicy (MkObserved _ world) includePolicy = do
  let entitiesToExclude =
        HMap.fromList $
          S.toList $
            S.map (\eId -> (makeKnownEntityIdUnsafe eId, Off)) (referencedEntitiesIn includePolicy)
      knownEntitiesMap = worldEntities world `HMap.difference` entitiesToExclude
  case NE.nonEmpty (HMap.keys knownEntitiesMap) of
    Just knownEntities -> do
      policies <- Gen.list (Range.constant 0 10) (genPolicy (NE.map unwrapKnownEntityId knownEntities))
      mconcat <$> genWithInserted [includePolicy] policies
    Nothing -> pure mempty

genPolicy :: NonEmpty EntityId -> Gen RawSpecification
genPolicy knownEntities = policy <$> genPolicyName <*> genActionExp knownEntities

genPolicyName :: Gen Text
genPolicyName = Gen.text (Range.constant 1 10) Gen.alphaNum

genStateExp :: Gen (Located (Exp Raw 'TState))
genStateExp = do
  loc <- genLocation
  state <- genToggleState
  pure (ELitState state :@ loc)

genActionExp :: NonEmpty EntityId -> Gen (Located (Exp Raw 'TAction))
genActionExp knownEntities = do
  loc <- genLocation
  SomeToggleable expr _ <- genKnownEntity knownEntities
  state <- genStateExp
  pure (ESetState expr state :@ loc)

genKnownEntity :: NonEmpty EntityId -> Gen SomeToggleable
genKnownEntity knownEntities = do
  eId <- genEntityIdOneOf knownEntities
  genEntityWithId eId

genLight' :: EntityId -> ToggleState -> Gen SomeToggleable
genLight' eId state = do
  loc <- genLocation
  let expr = ELitEntityLight eId :@ loc
  pure
    ( SomeToggleable
        expr
        (observedLight expr state)
    )

genInputBoolean' :: EntityId -> ToggleState -> Gen SomeToggleable
genInputBoolean' eId state = do
  loc <- genLocation
  let expr = ELitEntityInputBoolean eId :@ loc
  pure
    ( SomeToggleable
        expr
        (observedInputBoolean expr state)
    )

genEntity' :: EntityId -> ToggleState -> Gen SomeToggleable
genEntity' eId state = Gen.choice [genLight' eId state, genInputBoolean' eId state]

genEntity :: Gen SomeToggleable
genEntity = genEntityId >>= genEntityWithId

genEntityWithId :: EntityId -> Gen SomeToggleable
genEntityWithId eId = do
  state <- genToggleState
  genEntity' eId state

genEntityWithState :: ToggleState -> Gen SomeToggleable
genEntityWithState state = do
  eId <- genEntityId
  genEntity' eId state

genLightWithState :: ToggleState -> Gen SomeToggleable
genLightWithState state = do
  eId <- genEntityId
  genLight' eId state

genInputBooleanWithState :: ToggleState -> Gen SomeToggleable
genInputBooleanWithState state = do
  eId <- genEntityId
  genInputBoolean' eId state

genEntityIdOneOf :: NonEmpty EntityId -> Gen EntityId
genEntityIdOneOf knownEntities = do
  index <- Gen.int (Range.constant 0 (length knownEntities - 1))
  pure $ knownEntities NE.!! index

genLocation :: Gen Location
genLocation = Location <$> genPosition <*> pure mempty

genPosition :: Gen Position
genPosition = pure $ Position {begin = (-1, -1), end = (-1, -1), file = "Generated"}

--------------------------------------------------------------------------------

genStateChange :: KnownEntityId -> ToggleState -> ToggleState -> Gen HASSChange
genStateChange eId from to = Change <$> genVariables eId from to

genVariables :: KnownEntityId -> ToggleState -> ToggleState -> Gen HASSVariables
genVariables eId from to = Variables <$> genTriggered eId from to

genTriggered :: KnownEntityId -> ToggleState -> ToggleState -> Gen HASSTriggered
genTriggered eId from to =
  Triggered
    mempty
    mempty
    mempty
    mempty
    eId
    mempty
    mempty
    mempty
    <$> genState eId from
    <*> genState eId to

genState :: KnownEntityId -> ToggleState -> Gen HASSState
genState eId state =
  State
    eId
    (T.show $ pretty state)
    mempty
    <$> genTime
    <*> genTime
    <*> genTime
    <*> genContext

genContext :: Gen HASSContext
genContext = pure $ HASSContext mempty mempty mempty
