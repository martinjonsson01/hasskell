module Hasskell.Language.Provenance
  ( Explanation (..),
    -- Facts
    differs,
    observed,
    desired,
    -- Pretty-printing
    renderExplanation,
  )
where

import Data.List qualified as List
import Data.Text (Text)
import Effectful
import Effectful.FileSystem qualified as File
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.Report
import Hasskell.Language.World
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Terminal

-- | A motivated reasoning behind why something is the case.
data Explanation = Explain
  { what :: Fact,
    why :: [Explanation]
  }
  deriving (Eq, Ord, Show)

instance HasLocations Explanation where
  extractLocations Explain {what = fact, why} =
    extractLocations fact ++ concatMap extractLocations why

-- | The answer to a "but why?" question.
data Fact
  = DeclaredFact (Located DeclaredFact)
  | WorldFact WorldFact
  deriving (Eq, Ord, Show)

instance HasLocations Fact where
  extractLocations = \case
    DeclaredFact (_ :@ loc) -> [loc]
    WorldFact _ -> []

-- | A fact stemming from the user's declarations.
data DeclaredFact
  = Diff EntityId ToggleState ToggleState
  | DesiredState EntityId ToggleState
  deriving (Eq, Ord, Show)

-- | There is a difference between two states.
differs :: Location -> EntityId -> ToggleState -> ToggleState -> Fact
differs loc eId s1 s2 = DeclaredFact (Diff eId s1 s2 :@ loc)

-- | There is a desired state for an entity.
desired :: Location -> EntityId -> ToggleState -> Fact
desired loc eId = DeclaredFact . (:@ loc) . DesiredState eId

-- | A fact stemming from the current state of the world.
data WorldFact = ObservedState EntityId ToggleState
  deriving (Eq, Ord, Show)

-- | A specific state has been observed.
observed :: EntityId -> ToggleState -> Fact
observed eId = WorldFact . ObservedState eId

-- | Converts the given explanation into a human-readable format.
renderExplanation :: (MonadIO m) => Explanation -> m Text
renderExplanation = liftIO . runEff . File.runFileSystem . innerRenderExplanation

innerRenderExplanation :: (File.FileSystem :> es) => Explanation -> Eff es Text
innerRenderExplanation explanation = do
  let positions = extractLocations explanation
  baseDiagnostic <- loadReferencedFiles positions
  pure (layoutDoc (renderExplanationTree baseDiagnostic explanation))

renderExplanationTree :: Diagnostic Fact -> Explanation -> Doc Terminal.AnsiStyle
renderExplanationTree baseDiagnostic Explain {what, why} =
  let prettyFact = renderFact baseDiagnostic what
      childExplanations = map (renderExplanationTree baseDiagnostic) why
      prefixSubExplanation = indent 4 . ("-> because" <+>)
      subExplanations = map prefixSubExplanation childExplanations
      explanations = List.intersperse mempty (prettyFact : subExplanations)
   in vsep explanations

renderFact :: Diagnostic Fact -> Fact -> Doc Terminal.AnsiStyle
renderFact baseDiagnostic fact = case fact of
  DeclaredFact (_ :@ loc) ->
    toDocWithColor $
      baseDiagnostic
        `addReport` Warn
          Nothing
          fact
          [(positionsPrimary loc, This fact)]
          []
  WorldFact _ -> pretty fact

instance Pretty Fact where
  pretty = \case
    DeclaredFact (fact :@ _) -> pretty fact
    WorldFact fact -> pretty fact

instance Pretty DeclaredFact where
  pretty = \case
    Diff eId s1 s2 ->
      "entity" <+> pretty eId <> ":" <+> pretty s1 <+> "/=" <+> pretty s2
    DesiredState eId state ->
      "entity" <+> pretty eId <+> "should be" <+> pretty state

instance Pretty WorldFact where
  pretty = \case
    ObservedState eId current ->
      "entity" <+> pretty eId <+> "is" <+> pretty current
