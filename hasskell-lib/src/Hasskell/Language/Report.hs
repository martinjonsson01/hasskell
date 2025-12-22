module Hasskell.Language.Report
  ( loadReferencedFiles,
    renderInANSIColor,
    renderColorless,
    toDocWithColor,
    toDocWithoutColor,
    layoutDoc,
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
import Prettyprinter.Render.Text qualified as PrettyText

-- | Loads all the files referenced in the given positions into an empty diagnostic.
loadReferencedFiles :: (File.FileSystem :> es) => [Location] -> Eff es (Diagnostic msg)
loadReferencedFiles allLocations = do
  let getFilesIn (Location primary secondary) = (file primary) : map file secondary
      files = List.nub $ concatMap getFilesIn allLocations

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

toDoc :: (Pretty msg) => Diagnostic msg -> Doc (Annotation ann)
toDoc = prettyDiagnostic WithUnicode (TabSize 2)

toDocWithoutColor :: (Pretty msg) => Diagnostic msg -> Doc ann
toDocWithoutColor = unAnnotate . toDoc

toDocWithColor :: (Pretty msg) => Diagnostic msg -> Doc Terminal.AnsiStyle
toDocWithColor = reAnnotate defaultStyle . toDoc

layoutDoc :: Doc Terminal.AnsiStyle -> Text
layoutDoc = Terminal.renderStrict . layoutPretty defaultLayoutOptions

renderInANSIColor :: (Pretty msg) => Diagnostic msg -> Text
renderInANSIColor = layoutDoc . toDocWithColor

renderColorless :: (Pretty msg) => Diagnostic msg -> Text
renderColorless = PrettyText.renderStrict . layoutPretty defaultLayoutOptions . toDoc
