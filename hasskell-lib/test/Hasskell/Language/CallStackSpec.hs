{-# LANGUAGE ImplicitParams #-}

module Hasskell.Language.CallStackSpec (spec) where

import Data.List qualified as List
import Data.Maybe
import GHC.Stack
import Hasskell.Language.CallStack
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Diagnostic call stack" $ do
    it "correctly selects primary frame" $ do
      let internalLoc0 = fakePosition "hasskell-lib" "Hasskell.Some.Module"
          libraryLoc0 = fakePosition "user-lib" "My.Library.Mod"
          userLoc = fakePosition "user-lib" "My.User.Mod"
          cs = fromCallSiteList $ map ("",) $ [internalLoc0, libraryLoc0, userLoc]
          Location primary _ = captureSrcSpan' cs
      primary `shouldBe` convertSrcLoc libraryLoc0

    it "correctly selects secondary frame" $ do
      let internalLoc0 = fakePosition "hasskell-lib" "Hasskell.Some.Module"
          libraryLoc0 = fakePosition "user-lib" "My.Library.Mod"
          userLoc = fakePosition "user-lib" "My.User.Mod"
          cs = fromCallSiteList $ map ("",) $ [internalLoc0, libraryLoc0, userLoc]
          Location _ secondarySites = captureSrcSpan' cs
      secondarySites `shouldBe` [convertSrcLoc userLoc]

currentLoc :: (HasCallStack) => SrcLoc
currentLoc = fromJust . (snd . fst <$>) . List.uncons $ drop 1 $ getCallStack callStack

fakePosition :: (HasCallStack) => String -> String -> SrcLoc
fakePosition packageName moduleName =
  currentLoc
    { srcLocPackage = packageName,
      srcLocModule = moduleName
    }
