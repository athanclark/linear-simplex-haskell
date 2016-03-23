module Main where

import LinearSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [spec]
