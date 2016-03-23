module LinearSpec (spec) where

import Linear.Grammar
import Linear.Context
import Linear.Simplex

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Ratio ((%))
import Control.Monad.State

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Linear.Simplex"
  [ QC.testProperty "Solution is Feasible"
      solutionIsFeasible
  ]


data OptimalCheck = OptimalCheck
  { optimalConstraints :: [LinIneqExpr]
  , optimalObjective   :: LinExpr MainVarName
  , optimalFeasibles   :: [Map.Map MainVarName Rational]
  } deriving (Show, Eq)

instance Arbitrary OptimalCheck where
  arbitrary = do
    names <- arbitrary `suchThat` (not . Set.null)
    let (objName, vars) = fromJust (Set.maxView names)
    n <- choose (10, 100)
    constraints <- replicateM n (LinIneqExpr Lteq <$> arbitraryLinExpr vars)
    objective'  <- arbitraryLinExpr vars
    let objective = objective'
                       { linExprVars = Map.insert objName 1 (linExprVars objective')
                       }
    feasibles <- replicateM 10 $ do
      suchThat (arbitrary :: Gen (Map.Map MainVarName Rational))
        (\s -> all (isFeasible s) constraints)
    return $ OptimalCheck constraints objective feasibles

arbitraryRational :: Gen Rational
arbitraryRational = do
  numerator <- arbitrary
  return $ numerator % 1

arbitraryLinExpr :: Set.Set MainVarName -> Gen (LinExpr MainVarName)
arbitraryLinExpr vars = do
  varsIncluded <- foldM (\acc v -> do shouldInclude <- arbitrary
                                      if shouldInclude
                                      then return $ Set.insert v acc
                                      else return acc)
                        Set.empty
                        vars
  varmap <- foldM (\acc v -> do coeff <- arbitraryRational
                                return $ Map.insert v coeff acc)
                  Map.empty
                  varsIncluded
  const <- abs <$> arbitraryRational
  return $ LinExpr varmap const

solutionIsFeasible :: OptimalCheck -> Bool
solutionIsFeasible (OptimalCheck constraints objective feasibles) =
  let tableau         = execState (mapM_ addConstraint constraints) (newTableau objective)
      optimalTableau  = primalSimplex tableau
      optimalSolution = solution optimalTableau
  in  all (isFeasible optimalSolution) constraints
