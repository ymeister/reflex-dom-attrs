{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Reflex.Dom.Attrs
import Reflex.Spider.Internal (Global, SpiderTimeline)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

newtype TestM a = TestM {runTestM :: Identity a}
  deriving (Functor, Applicative, Monad)

type TestTimeline = SpiderTimeline Global

testLastStyleWins :: TestTree
testLastStyleWins =
  testCase "when mconcat combines Attrs that set a CSS property, the right-most conflicting property set wins" $ do
    let combinedAttr :: Attrs TestTimeline TestM
        combinedAttr = "style" ~:
          [ "color" ~:: "red"
          , "color" ~:: "green"
          , "color" ~:: "blue"
          ]
        finalColor = Map.lookup "color" (attrs_style combinedAttr)
    assertEqual "final property is the rightmost" finalColor $ Just "blue"
    assertEqual "final property is not the leftmost" (finalColor == Just "red") False

testSemigroup :: TestTree
testSemigroup = testGroup "Attrs t m Semigroup instance" [testLastStyleWins]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Reflex.Dom.Attrs"
      [testSemigroup]
