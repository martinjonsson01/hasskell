module Hasskell.Language.Reconciler
  ( -- Plans
    ReconciliationPlan (..),
    ReconciliationStep (..),
    isPlanEmpty,
    reconcile,
    -- Reports
    ReconciliationReport,
    hasWarnings,
    renderReport,
  )
where

import Control.Monad
import Data.Either
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.World
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Terminal

-- | Non-fatal details about how the reconciliation went.
data ReconciliationReport = MkReconciliationReport [ReconciliationDiagnostic]

-- | Info about a reconciliation occurrence.
data ReconciliationDiagnostic = Diagnostic
  { diagnosticSpan :: Position,
    diagnosticReport :: Report Text
  }

-- | Converts the given report from a data representation into a pretty
-- user-presentable representation.
renderReport :: (MonadIO m) => ReconciliationReport -> m Text
renderReport (MkReconciliationReport diagnostics) = do
  let reports = map diagnosticReport diagnostics
      incompleteDiagnostic = foldl' addReport mempty reports
      files = List.nub $ map (file . diagnosticSpan) diagnostics

  fullDiagnostic <- foldM loadAndAddFile incompleteDiagnostic files

  let doc = prettyDiagnostic WithUnicode (TabSize 2) fullDiagnostic
      coloredDoc = reAnnotate defaultStyle doc
  pure $ Terminal.renderStrict $ layoutPretty defaultLayoutOptions coloredDoc
  where
    loadAndAddFile diagnostic path = liftIO $ addFile diagnostic path <$> readFile path

hasWarnings :: ReconciliationReport -> Bool
hasWarnings (MkReconciliationReport reports) = length (reports) > 0

warnUnknownEntity :: Position -> EntityId -> ReconciliationDiagnostic
warnUnknownEntity srcSpan (EntityId entityId) =
  Diagnostic
    srcSpan
    ( Warn
        Nothing
        "Unknown entity"
        [(srcSpan, message)]
        []
    )
  where
    title = "Unknown entity"
    message = This $ T.unwords [title, T.show entityId]

--------------------------------------------------------------------------------

-- | A plan describing how to transform one world into another.
data ReconciliationPlan = MkReconciliationPlan [ReconciliationStep]
  deriving (Eq, Ord, Show)

isPlanEmpty :: ReconciliationPlan -> Bool
isPlanEmpty (MkReconciliationPlan steps) = null steps

-- | A step that can be taken to alter the world.
data ReconciliationStep = TurnOnEntity EntityId
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Computes the steps necessary to transform
-- the observed world state into the specified world state.
reconcile :: ObservedWorld -> Specification -> (ReconciliationPlan, ReconciliationReport)
reconcile observed spec =
  let (unknowns, toTurnOn) = extractAllEntitiesToTurnOn spec observed
      steps = turnOnEntities toTurnOn observed
   in ( MkReconciliationPlan steps,
        MkReconciliationReport unknowns
      )

turnOnEntities :: [EntityId] -> ObservedWorld -> [ReconciliationStep]
turnOnEntities toTurnOn (MkObserved _ world) =
  mapMaybe turnOn (worldToggleables world)
  where
    turnOn entity
      | toggleableId entity `elem` toTurnOn = pure $ TurnOnEntity (toggleableId entity)
      | otherwise = Nothing

extractAllEntitiesToTurnOn :: Specification -> ObservedWorld -> ([ReconciliationDiagnostic], [EntityId])
extractAllEntitiesToTurnOn Specification {specPolicies} (MkObserved _ world) =
  partitionEithers $ concatMap (extractEntitiesToTurnOn entityMap) specPolicies
  where
    entityMap = worldToggleableMap world

extractEntitiesToTurnOn ::
  Map EntityId Toggleable ->
  Policy ->
  [Either ReconciliationDiagnostic EntityId]
extractEntitiesToTurnOn entityMap (Policy _ (SomeExp (EIsOn _ (EEntity entitySpan entityId)))) =
  maybe
    [Left (warnUnknownEntity entitySpan entityId)]
    (\t -> [Right (toggleableId t) | toggleableState t /= On])
    (Map.lookup entityId entityMap)
extractEntitiesToTurnOn _ _ = []

worldToggleableMap :: World -> Map EntityId Toggleable
worldToggleableMap world =
  Map.fromList
    [ (toggleableId t, t)
    | t <- worldToggleables world
    ]
