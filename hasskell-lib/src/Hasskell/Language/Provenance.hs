module Hasskell.Language.Provenance
  ( Explanation (..),
    Explained (..),
    explain,
    because,
    becauseMore,
    elaborate,
    stripExplanation,
    -- Facts
    differs,
    observed,
    desired,
    branched,
    equality,
    evaluated,
    literal,
    -- Pretty-printing
    prettifyExplanation,
    cleanExplanation,
  )
where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem qualified as File
import Error.Diagnose
import Hasskell.HomeAssistant.API
import Hasskell.Language.AST
import Hasskell.Language.CallStack
import Hasskell.Language.Report
import Hasskell.Language.World
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | A motivated reasoning behind why something is the case.
data Explanation = Explain
  { what :: Fact,
    why :: [Explanation]
  }
  deriving (Eq, Ord, Show)

-- | Something that has been given an explanation.
data Explained a = a :£ Explanation
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Things that are explanations.
class IntoExplanation a where
  toExplanation :: a -> Explanation

instance IntoExplanation Explanation where
  toExplanation = id

instance IntoExplanation Fact where
  toExplanation fact = Explain fact mempty

-- | Explain a fact.
explain :: Fact -> Explanation -> Explanation
explain fact = Explain fact . List.singleton

-- | Give an explanation to something.
because :: (IntoExplanation expl) => a -> expl -> Explained a
because a expl = a :£ (toExplanation expl)

-- | Add a sibling explanation.
becauseMore :: (IntoExplanation expl) => Explained a -> expl -> Explained a
becauseMore (a :£ expl) new = a :£ expl {why = toExplanation new : why expl}

-- | Give another level of explanation to something.
elaborate :: Explained a -> Fact -> Explained a
elaborate (a :£ old) newFact = a :£ Explain {what = newFact, why = [old]}

-- | Forget about the explanation.
stripExplanation :: Explained a -> a
stripExplanation (a :£ _) = a

instance HasLocations Explanation where
  extractLocations Explain {what = fact, why} =
    extractLocations fact ++ concatMap extractLocations why

-- | The answer to a "but why?" question.
data Fact
  = DeclaredFact (Located DeclaredFact)
  | SourcelessFact SourcelessFact
  deriving (Eq, Ord, Show)

instance HasLocations Fact where
  extractLocations = \case
    DeclaredFact (_ :@ loc) -> [loc]
    SourcelessFact _ -> []

-- | A fact stemming from the user's declarations.
data DeclaredFact
  = DesiredState EntityId ToggleState
  | BranchTaken
  | Equality Bool
  | Evaluated ToggleState
  | Literal
  deriving (Eq, Ord, Show)

-- | There is a difference between two states.
differs :: EntityId -> ToggleState -> ToggleState -> Fact
differs eId s1 s2 = SourcelessFact (Diff eId s1 s2)

-- | There is a desired state for an entity.
desired :: Location -> EntityId -> ToggleState -> Fact
desired loc eId = DeclaredFact . (:@ loc) . DesiredState eId

-- | A branch was taken.
branched :: Location -> Fact
branched = DeclaredFact . (BranchTaken :@)

-- | Values are equal.
equality :: Location -> Bool -> Fact
equality loc = DeclaredFact . (:@ loc) . Equality

-- | A given state was evaluated.
evaluated :: Location -> ToggleState -> Fact
evaluated loc = DeclaredFact . (:@ loc) . Evaluated

-- | The state was specified with a source code literal.
literal :: Location -> Fact
literal = DeclaredFact . (Literal :@)

-- | A fact that does not stem from the user's declarations.
data SourcelessFact
  = ObservedState EntityId ToggleState
  | Diff EntityId ToggleState ToggleState
  deriving (Eq, Ord, Show)

-- | A specific state has been observed.
observed :: EntityId -> ToggleState -> Fact
observed eId = SourcelessFact . ObservedState eId

-- | Converts the given explanation into a human-readable format.
prettifyExplanation :: (File.FileSystem :> es) => Explanation -> Eff es (Doc AnsiStyle)
prettifyExplanation explanation = do
  let positions = extractLocations explanation
  baseDiagnostic <- loadReferencedFiles positions
  pure (prettifyExplanationTree baseDiagnostic explanation)

-- Hacky way to remove the "warnings" that we can't get rid of from Diagnostics.
-- This is because we make use of Diagnostic warnings to pretty-print code references.
cleanExplanation :: Text -> Text
cleanExplanation =
  T.replace
    "\ESC[0;93;1m[warning]\ESC[0m: "
    ""
    . T.replace "[warning]: " ""

prettifyExplanationTree :: Diagnostic Fact -> Explanation -> Doc AnsiStyle
prettifyExplanationTree baseDiagnostic Explain {what, why} =
  let prettyFact = renderFact baseDiagnostic what
      childExplanations = map (prettifyExplanationTree baseDiagnostic) why
      prefixSubExplanation = indent 4 . ("├─▶ because" <+>)
      subExplanations = map prefixSubExplanation childExplanations
      explanations = List.intersperse (indent 4 "│") (prettyFact : subExplanations)
   in vsep explanations

renderFact :: Diagnostic Fact -> Fact -> Doc AnsiStyle
renderFact baseDiagnostic fact = case fact of
  DeclaredFact (_ :@ loc) ->
    toDocWithColor $
      baseDiagnostic
        `addReport` Warn
          Nothing
          fact
          [(positionsPrimary loc, This fact)]
          []
  SourcelessFact _ -> pretty fact

instance Pretty Fact where
  pretty = \case
    DeclaredFact (fact :@ _) -> pretty fact
    SourcelessFact fact -> pretty fact

instance Pretty DeclaredFact where
  pretty = \case
    DesiredState eId state ->
      "entity" <+> pretty eId <+> "should be" <+> pretty state
    BranchTaken -> "branch was taken"
    Equality equal -> pretty equal
    Evaluated state -> pretty state
    Literal -> "literal"

instance Pretty SourcelessFact where
  pretty = \case
    Diff _ s1 s2 -> "needs reconciling:" <+> pretty s1 <+> "/=" <+> pretty s2
    ObservedState eId current ->
      "entity" <+> pretty eId <+> "is" <+> pretty current
