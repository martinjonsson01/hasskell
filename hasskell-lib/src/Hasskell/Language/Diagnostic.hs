module Hasskell.Language.Diagnostic
  ( -- Reports
    ReconciliationReport,
    reportFromList,
    ReconciliationDiagnostic,
    renderReport,
    -- Capturing source locations
    captureSrcSpan,
    Location,
    -- Warnings
    hasWarnings,
    warnUnknownEntity,
  )
where

import Data.Foldable
import Data.Heap (MaxPrioHeap)
import Data.Heap qualified as Heap
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import Data.Text.Metrics
import Effectful
import Effectful.FileSystem qualified as File
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.CallStack
import Hasskell.Language.Report

-- | Non-fatal details about how the reconciliation went.
data ReconciliationReport = MkReconciliationReport [ReconciliationDiagnostic]

reportFromList :: [ReconciliationDiagnostic] -> ReconciliationReport
reportFromList = MkReconciliationReport

-- | Info about a reconciliation occurrence.
data ReconciliationDiagnostic = Diagnostic
  { diagnosticLocation :: Location,
    diagnosticReport :: Report Text
  }

-- | Converts the given report from a data representation into a pretty
-- user-presentable representation.
renderReport :: (MonadIO m) => ReconciliationReport -> m Text
renderReport = liftIO . runEff . File.runFileSystem . innerRenderReport

innerRenderReport :: (File.FileSystem :> es) => ReconciliationReport -> Eff es Text
innerRenderReport (MkReconciliationReport diagnostics) = do
  let referencedLocation = map diagnosticLocation diagnostics
  baseDiagnostic <- loadReferencedFiles referencedLocation

  let reports = map diagnosticReport diagnostics
      fullDiagnostic = foldl' addReport baseDiagnostic reports

  pure (renderInANSIColor fullDiagnostic)

hasWarnings :: ReconciliationReport -> Bool
hasWarnings (MkReconciliationReport reports) = length (reports) > 0

-- TODO: don't create a real report here, just store the data so
-- that we can create a real one later on (where we're free to rewrite
-- the file paths as we like)
warnUnknownEntity :: Location -> EntityId -> [EntityId] -> ReconciliationDiagnostic
warnUnknownEntity positions (EntityId entityId) knownEntities =
  Diagnostic
    positions
    ( Warn
        Nothing
        "Unknown entity referenced"
        (mainMarker : contextMarkers <> suggestionMarker)
        ["The entity ID may be misspelled."]
    )
  where
    message = This $ mconcat ["Unknown entity ID `", entityId, "`"]
    mainMarker = (positionsPrimary positions, message)
    suggestionMarker =
      maybeToList $
        (positionsPrimary positions,)
          . Where
          . ("did you mean `" <>)
          . (<> "`?")
          <$> closestMatch
      where
        unwrapEntity (EntityId entityInner) = entityInner
        closestMatch = findClosestMatch entityId (map unwrapEntity knownEntities)
    contextMarkers = map (,Where "via") (positionsSecondary positions)

findClosestMatch :: Text -> [Text] -> Maybe Text
findClosestMatch text candidates =
  snd <$> find ((> 0.85) . fst) (Heap.viewHead (scoreSimilarities text candidates))

scoreSimilarities :: Text -> [Text] -> MaxPrioHeap (Ratio Int) Text
scoreSimilarities text candidates =
  foldl'
    (flip Heap.insert)
    mempty
    (map (computeSimilarity text) candidates)

computeSimilarity :: Text -> Text -> (Ratio Int, Text)
computeSimilarity reference candidate = (jaroWinkler reference candidate, candidate)
