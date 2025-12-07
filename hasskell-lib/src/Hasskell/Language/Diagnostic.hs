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
import Data.List qualified as List
import Data.Text (Text)
import Effectful
import Effectful.FileSystem qualified as File
import Effectful.FileSystem.IO.ByteString qualified as File8
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.CallStack
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Terminal
import System.FilePath

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
      exists <- File.doesFileExist path
      contents <-
        if exists
          then BS8.toString <$> File8.readFile path
          else pure "file does not exist"
      pure $ addFile diagnostic (simplifyFilePath path) contents

hasWarnings :: ReconciliationReport -> Bool
hasWarnings (MkReconciliationReport reports) = length (reports) > 0

simplifyBlamePaths :: Blame -> Blame
simplifyBlamePaths (Blame primary secondary) =
  Blame
    (simplifyPath primary)
    (map simplifyPath secondary)
  where
    simplifyPath position = position {file = simplifyFilePath (file position)}

simplifyFilePath :: FilePath -> FilePath
simplifyFilePath = takeFileName

warnUnknownEntity :: Blame -> EntityId -> ReconciliationDiagnostic
warnUnknownEntity blame (EntityId entityId) =
  Diagnostic
    blame
    ( Warn
        Nothing
        "Unknown entity referenced"
        (mainMarker : contextMarkers)
        ["The entity ID may be misspelled."]
    )
  where
    message = This $ mconcat ["Unknown entity ID `", entityId, "`"]
    cleanedBlame = simplifyBlamePaths blame
    mainMarker = (blamePrimary cleanedBlame, message)
    contextMarkers = map (,Where "via") (blameSecondary cleanedBlame)
