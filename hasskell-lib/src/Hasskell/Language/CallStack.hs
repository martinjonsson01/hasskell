module Hasskell.Language.CallStack
  ( Positions (..),
    captureSrcSpan,
    captureSrcSpan',
    convertSrcLoc,
  )
where

import Data.Bifunctor (bimap)
import Data.List qualified as List
import Error.Diagnose
import GHC.Stack

-- | A main location, along with supplementary locations, in the user's source code.
data Positions = Positions
  { positionsPrimary :: Position,
    positionsSecondary :: [Position]
  }
  deriving (Eq, Ord, Show)

-- | Captures the current context, based on the current call stack.
captureSrcSpan :: (HasCallStack) => Positions
captureSrcSpan = captureSrcSpan' callStack

-- | Captures the current context, based on the given call stack.
captureSrcSpan' :: CallStack -> Positions
captureSrcSpan' cs =
  let (primary, secondary) =
        bimap
          convertSrcLoc
          (map convertSrcLoc)
          (selectPositions cs)
   in Positions primary secondary

convertSrcLoc :: SrcLoc -> Position
convertSrcLoc
  SrcLoc
    { srcLocFile,
      srcLocStartLine,
      srcLocStartCol,
      srcLocEndLine,
      srcLocEndCol
    } =
    Position
      (srcLocStartLine, srcLocStartCol)
      (srcLocEndLine, srcLocEndCol)
      srcLocFile

-- | Selects positions that lie in user code.
selectPositions :: CallStack -> (SrcLoc, [SrcLoc])
selectPositions cs =
  let frames = reverse (getCallStack cs) -- outermost first
      classified = zip frames (map (classify . snd) frames)
      userFrames = takeWhile ((== UserFrame) . snd) classified
   in case List.unsnoc userFrames of
        Just (rest, ((_, boundarySiteLoc), _)) -> (boundarySiteLoc, map (snd . fst) rest)
        Nothing -> (snd (last frames), []) -- fall back to just taking the top frame

-- | A classification of call stack frames.
data FrameClass = UserFrame | InternalFrame
  deriving (Eq, Ord, Show)

classify :: SrcLoc -> FrameClass
classify loc
  | isInternal loc = InternalFrame
  | otherwise = UserFrame

isInternal :: SrcLoc -> Bool
isInternal loc =
  (srcLocModule loc `elem` knownInternalModules)
    || ("hasskell-lib" `List.isPrefixOf` srcLocPackage loc)

knownInternalModules :: [String]
knownInternalModules = ["Hasskell.Language.AST"]
