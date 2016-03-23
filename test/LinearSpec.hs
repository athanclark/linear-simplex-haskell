module LinearSpec (spec) where

import Linear.Simplex

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Linear.Simplex"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

solutionIsOptimal :: Set.Set MainVarName -> Property
solutionIsOptimal names = not (not $ x) === x
