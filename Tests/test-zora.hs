{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- imports

import Test.Tasty
import Test.Tasty.HUnit

import qualified Zora.List as ZL

testTakeWhileKeepLast :: Assertion
testTakeWhileKeepLast = do
    ZL.take_while_keep_last (< 3) [1..] @?= [1,2,3]

tests :: TestTree
tests = testGroup "unit tests"
    [ testCase "Testing `take_while_keep_last`" testTakeWhileKeepLast ]

main :: IO ()
main = defaultMain tests