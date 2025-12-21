module Hasskell.Language.Report
  ( loadReferencedFiles,
    renderInANSIColor,
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
import Hasskell.Language.CallStack
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Terminal

-- | Loads all the files referenced in the given positions into an empty diagnostic.
loadReferencedFiles :: (File.FileSystem :> es) => [Positions] -> Eff es (Diagnostic msg)
loadReferencedFiles allPositions = do
  let getFilesIn (Positions primary secondary) = (file primary) : map file secondary
      files = List.nub $ concatMap getFilesIn allPositions

  foldM loadAndAddFile mempty files
  where
    loadAndAddFile :: (File.FileSystem :> es) => Diagnostic msg -> FilePath -> Eff es (Diagnostic msg)
    loadAndAddFile diagnostic path = do
      absolutePath <- File.makeAbsolute path
      exists <- File.doesFileExist absolutePath
      contents <-
        if exists
          then BS8.toString <$> File8.readFile absolutePath
          else pure "file does not exist"
      pure $ addFile diagnostic path contents

renderInANSIColor :: (Pretty msg) => Diagnostic msg -> Text
renderInANSIColor diagnostic =
  let doc = prettyDiagnostic WithUnicode (TabSize 2) diagnostic
      coloredDoc = reAnnotate defaultStyle doc
   in Terminal.renderStrict $ layoutPretty defaultLayoutOptions coloredDoc
