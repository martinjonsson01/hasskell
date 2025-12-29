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
    -- Errors
    errorTypeMismatch,
  )
where

import Data.Foldable
import Data.Heap (MaxPrioHeap)
import Data.Heap qualified as Heap
import Data.List qualified as List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Metrics
import Effectful
import Effectful.FileSystem qualified as File
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.Report
import Prettyprinter

-- | Non-fatal details about how the reconciliation went.
newtype ReconciliationReport = MkReconciliationReport [ReconciliationDiagnostic] -- TODO: make into set
  deriving (Semigroup, Monoid) via [ReconciliationDiagnostic]

reportFromList :: [ReconciliationDiagnostic] -> ReconciliationReport
reportFromList = MkReconciliationReport

-- | Info about a reconciliation occurrence.
data ReconciliationDiagnostic = Diagnostic
  { diagnosticLocation :: Location,
    diagnosticReport :: Report Text
  }

-- | Converts the given report from a data representation into a pretty
-- user-presentable representation.
renderReport :: (MonadIO m) => ReportStyle -> ReconciliationReport -> m Text
renderReport style = liftIO . runEff . File.runFileSystem . innerRenderReport style

innerRenderReport :: (File.FileSystem :> es) => ReportStyle -> ReconciliationReport -> Eff es Text
innerRenderReport style (MkReconciliationReport diagnostics) = do
  let referencedLocation = map diagnosticLocation diagnostics
  baseDiagnostic <- loadReferencedFiles referencedLocation

  let reports = map diagnosticReport diagnostics
      fullDiagnostic = foldl' addReport baseDiagnostic reports

  pure (layoutDoc style $ annotateDoc style fullDiagnostic)

hasWarnings :: ReconciliationReport -> Bool
hasWarnings (MkReconciliationReport reports) = length (reports) > 0

-- TODO: don't create a real report here, just store the data so
-- that we can create a real one later on (where we're free to rewrite
-- the file paths as we like)
warnUnknownEntity :: [EntityId] -> Located EntityId -> ReconciliationReport
warnUnknownEntity knownEntities (entityId :@ positions) =
  MkReconciliationReport $
    List.singleton $
      Diagnostic
        positions
        ( Warn
            Nothing
            "Unknown entity referenced"
            (mainMarker : contextMarkers <> suggestionMarker)
            ["The entity ID may be misspelled."]
        )
  where
    message = This $ T.show $ "Unknown entity ID" <+> pretty entityId
    mainMarker = (positionsPrimary positions, message)
    suggestionMarker =
      maybeToList $
        (positionsPrimary positions,)
          . Where
          . T.show
          . ("did you mean `" <>)
          . (<> "`?")
          <$> closestMatch
      where
        closestMatch = findClosestMatch (unwrapEntityId entityId) (map unwrapEntityId knownEntities)
    contextMarkers = map (,Where "via") (positionsSecondary positions)

errorTypeMismatch :: T -> Located T -> ReconciliationReport
errorTypeMismatch expectedT (actualT :@ loc) =
  MkReconciliationReport $
    List.singleton $
      Diagnostic
        loc
        ( Err
            Nothing
            "Type mismatch"
            (mainMarker : contextMarkers)
            []
        )
  where
    message =
      This $
        mconcat
          [ "Expected `",
            T.show expectedT,
            "` but got `",
            T.show actualT,
            "`"
          ]
    mainMarker = (positionsPrimary loc, message)
    contextMarkers = map (,Where "via") (positionsSecondary loc)

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
