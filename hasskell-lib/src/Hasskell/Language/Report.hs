module Hasskell.Language.Report
  ( loadReferencedFiles,
    trimPath,
    trimLoc,
    trimPos,
    annotateDoc,
    defaultStyle,
    unadornedStyle,
    layoutDoc,
    ReportStyle (..),
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
import System.FilePath

-- | Loads all the files referenced in the given positions into an empty diagnostic.
loadReferencedFiles :: (File.FileSystem :> es, HasLocations l) => l -> Eff es (Diagnostic msg)
loadReferencedFiles allLocations = do
  let getFilesIn (Location primary secondary) = (file primary) : map file secondary
      files = List.nub $ foldMap getFilesIn (extractLocations allLocations)

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
      pure $ addFile diagnostic (trimPath path) contents

-- | Trims away the irrelevant parts of the path, for displaying it.
trimLoc :: Location -> Location
trimLoc Location {positionsPrimary, positionsSecondary} =
  Location
    { positionsPrimary = trimPos positionsPrimary,
      positionsSecondary = map trimPos positionsSecondary
    }

-- | Trims away the irrelevant parts of the path, for displaying it.
trimPos :: Position -> Position
trimPos pos@Position {file} = pos {file = trimPath file}

-- | Trims away the irrelevant parts of the path, for displaying it.
trimPath :: FilePath -> FilePath
trimPath = takeFileName

toAnnotatedDoc :: (Pretty msg) => Diagnostic msg -> Doc (Annotation ann)
toAnnotatedDoc = prettyDiagnostic WithUnicode (TabSize 2)

data ReportStyle = Plain | Rich

layoutDoc :: ReportStyle -> Doc Terminal.AnsiStyle -> Text
layoutDoc Plain = PrettyText.renderStrict . layoutPretty defaultLayoutOptions
layoutDoc Rich = Terminal.renderStrict . layoutPretty defaultLayoutOptions

annotateDoc :: (Pretty msg) => ReportStyle -> Diagnostic msg -> Doc Terminal.AnsiStyle
annotateDoc Plain = reAnnotate unadornedStyle . toAnnotatedDoc
annotateDoc Rich = reAnnotate defaultStyle . toAnnotatedDoc
