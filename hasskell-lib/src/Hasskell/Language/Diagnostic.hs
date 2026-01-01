module Hasskell.Language.Diagnostic
  ( -- Reports
    VerificationReport,
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
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable
import Data.Heap (MaxPrioHeap)
import Data.Heap qualified as Heap
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Metrics
import Effectful
import Effectful.FileSystem qualified as File
import Error.Diagnose
import GHC.Generics
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.Report
import Prettyprinter

-- | Non-fatal details about how the reconciliation went.
newtype VerificationReport = MkVerificationReport (HashSet ReconciliationDiagnostic)
  deriving newtype (Semigroup, Monoid)

hasWarnings :: VerificationReport -> Bool
hasWarnings (MkVerificationReport reports) = length (reports) > 0

reportFromList :: [ReconciliationDiagnostic] -> VerificationReport
reportFromList = MkVerificationReport . HS.fromList

-- | Info about a reconciliation occurrence.
data ReconciliationDiagnostic = WarnUnknownEntity [KnownEntityId] (Located EntityId)
  deriving (Eq, Ord, Show, Generic, Hashable)

warnUnknownEntity :: [KnownEntityId] -> Located EntityId -> VerificationReport
warnUnknownEntity known = MkVerificationReport . HS.singleton . WarnUnknownEntity known

instance HasLocations VerificationReport where
  extractLocations (MkVerificationReport diagnostics) = foldMap extractLocations diagnostics

instance HasLocations ReconciliationDiagnostic where
  extractLocations = \case
    WarnUnknownEntity _ (_ :@ loc) -> HS.singleton loc

-- | Converts the given report from a data representation into a pretty
-- user-presentable representation.
renderReport :: (MonadIO m) => ReportStyle -> VerificationReport -> m Text
renderReport style = liftIO . runEff . File.runFileSystem . innerRenderReport style

innerRenderReport :: (File.FileSystem :> es) => ReportStyle -> VerificationReport -> Eff es Text
innerRenderReport style report@(MkVerificationReport diagnostics) = do
  baseDiagnostic <- loadReferencedFiles report

  let reports = map renderDiagnostic (toList diagnostics)
      fullDiagnostic = foldl' addReport baseDiagnostic reports

  pure (layoutDoc style $ annotateDoc style fullDiagnostic)

renderDiagnostic :: ReconciliationDiagnostic -> Report Text
renderDiagnostic = \case
  WarnUnknownEntity knownEntities entityId -> renderUnknownEntity knownEntities entityId

renderUnknownEntity :: [KnownEntityId] -> Located EntityId -> Report Text
renderUnknownEntity knownEntities (entityId :@ positions) =
  Warn
    Nothing
    "Unknown entity referenced"
    (mainMarker : contextMarkers <> suggestionMarker)
    ["The entity ID may be misspelled."]
  where
    message = This $ T.show $ "Unknown entity ID" <+> pretty entityId
    mainPos = trimPos (positionsPrimary positions)
    mainMarker = (mainPos, message)
    suggestionMarker =
      maybeToList $
        (mainPos,)
          . Where
          . T.show
          . ("did you mean `" <>)
          . (<> "`?")
          <$> closestMatch
      where
        closestMatch = findClosestMatch (unwrapEntityId entityId) (map (unwrapEntityId . unwrapKnownEntityId) knownEntities)
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
