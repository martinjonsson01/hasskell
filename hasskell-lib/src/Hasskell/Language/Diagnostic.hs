module Hasskell.Language.Diagnostic
  ( -- Reports
    ReconciliationReport,
    reportFromList,
    ReconciliationDiagnostic,
    renderReport,
    -- Capturing source locations
    captureSrcSpan,
    Blame,
    -- Warnings
    hasWarnings,
    warnUnknownEntity,
  )
where

import Control.Monad
import Data.ByteString.UTF8 qualified as BS8
import Data.Foldable
import Data.Heap (MaxPrioHeap)
import Data.Heap qualified as Heap
import Data.List qualified as List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import Data.Text.Metrics
import Effectful
import Effectful.FileSystem qualified as File
import Effectful.FileSystem.IO.ByteString qualified as File8
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.CallStack
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Terminal

-- | Non-fatal details about how the reconciliation went.
data ReconciliationReport = MkReconciliationReport [ReconciliationDiagnostic]

reportFromList :: [ReconciliationDiagnostic] -> ReconciliationReport
reportFromList = MkReconciliationReport

-- | Info about a reconciliation occurrence.
data ReconciliationDiagnostic = Diagnostic
  { diagnosticBlame :: Blame,
    diagnosticReport :: Report Text
  }

-- | Converts the given report from a data representation into a pretty
-- user-presentable representation.
renderReport :: (MonadIO m) => ReconciliationReport -> m Text
renderReport = liftIO . runEff . File.runFileSystem . innerRenderReport

innerRenderReport :: (File.FileSystem :> es) => ReconciliationReport -> Eff es Text
innerRenderReport (MkReconciliationReport diagnostics) = do
  let reports = map diagnosticReport diagnostics
      incompleteDiagnostic = foldl' addReport mempty reports
      getFilesIn (Blame primary secondary) = (file primary) : map file secondary
      files = List.nub $ concatMap (getFilesIn . diagnosticBlame) diagnostics

  fullDiagnostic <- foldM loadAndAddFile incompleteDiagnostic files

  let doc = prettyDiagnostic WithUnicode (TabSize 2) fullDiagnostic
      coloredDoc = reAnnotate defaultStyle doc
  pure $ Terminal.renderStrict $ layoutPretty defaultLayoutOptions coloredDoc
  where
    loadAndAddFile :: (File.FileSystem :> es) => Diagnostic Text -> FilePath -> Eff es (Diagnostic Text)
    loadAndAddFile diagnostic path = do
      absolutePath <- File.makeAbsolute path
      exists <- File.doesFileExist absolutePath
      contents <-
        if exists
          then BS8.toString <$> File8.readFile absolutePath
          else pure "file does not exist"
      pure $ addFile diagnostic path contents

hasWarnings :: ReconciliationReport -> Bool
hasWarnings (MkReconciliationReport reports) = length (reports) > 0

-- TODO: don't create a real report here, just store the data so
-- that we can create a real one later on (where we're free to rewrite
-- the file paths as we like)
warnUnknownEntity :: Blame -> EntityId -> [EntityId] -> ReconciliationDiagnostic
warnUnknownEntity blame (EntityId entityId) knownEntities =
  Diagnostic
    blame
    ( Warn
        Nothing
        "Unknown entity referenced"
        (mainMarker : contextMarkers <> suggestionMarker)
        ["The entity ID may be misspelled."]
    )
  where
    message = This $ mconcat ["Unknown entity ID `", entityId, "`"]
    mainMarker = (blamePrimary blame, message)
    suggestionMarker =
      maybeToList $
        (blamePrimary blame,)
          . Where
          . ("did you mean `" <>)
          . (<> "`?")
          <$> closestMatch
      where
        unwrapEntity (EntityId entityInner) = entityInner
        closestMatch = findClosestMatch entityId (map unwrapEntity knownEntities)
    contextMarkers = map (,Where "via") (blameSecondary blame)

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
