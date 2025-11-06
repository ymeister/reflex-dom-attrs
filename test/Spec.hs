{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Reflex.Dom.Attrs
import Reflex.Spider.Internal (Global, SpiderTimeline)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype TestM a = TestM {runTestM :: Identity a}
  deriving (Functor, Applicative, Monad)

type TestTimeline = SpiderTimeline Global

testLastStyleWins :: TestTree
testLastStyleWins =
  testCase "when mconcat combines Attrs that set a CSS property, the right-most conflicting property set wins" $
    let combinedAttr :: Attrs TestTimeline TestM
        combinedAttr = "style" ~:
          [ "color" ~:: "red"
          , "color" ~:: "green"
          , "color" ~:: "blue"
          ]
        finalColor = Map.lookup "color" (attrs_style combinedAttr)
    in finalColor @?= Just "blue"

testFirstStyleLoses :: TestTree
testFirstStyleLoses = 
  testCase "when mconcat combines Attrs that set a CSS property, the left-most conflicting property set loses" $
    let combinedAttr :: Attrs TestTimeline TestM
        combinedAttr = "style" ~:
          [ "color" ~:: "red"
          , "color" ~:: "green"
          , "color" ~:: "blue"
          ]
        finalColor = Map.lookup "color" (attrs_style combinedAttr)
    in (finalColor == Just "red") @?= False

testSemigroup :: TestTree
testSemigroup = testGroup "Attrs t m Semigroup instance" [testFirstStyleLoses, testLastStyleWins]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Reflex.Dom.Attrs"
      [testSemigroup]
