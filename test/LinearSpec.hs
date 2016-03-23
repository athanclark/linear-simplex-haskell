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

import Debug.Trace


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
    names <- arbitrary `suchThat` (\s -> Set.size s > 2 && all (\v -> length v > 1 && length v < 10) s)
    let (objName, vars) = fromJust (Set.maxView names)
    n <- choose (10, 100)
    constraints <- replicateM n (LinIneqExpr Lteq <$> arbitraryLinExpr vars)
    objective'  <- arbitraryLinExpr vars
    let objective = objective'
                      { linExprVars = Map.insert objName 1 (linExprVars objective')
                      }
    feasibles <- replicateM 10 $ do
      values <- replicateM (Set.size vars) arbitrary
      let solution' = (% 1) <$> Map.fromList (Set.toList vars `zip` values)
      return solution' `suchThat` (\s -> all (isFeasible s) constraints)
    trace ("Feasibles: " ++ show feasibles) $
      return $ OptimalCheck constraints objective feasibles

arbitraryRational :: Gen Rational
arbitraryRational = do
  numerator <- arbitrary `suchThat` (> 0)
  return $ numerator % 1

arbitraryLinExpr :: Set.Set MainVarName -> Gen (LinExpr MainVarName)
arbitraryLinExpr vars = do
  varsIncluded <- (foldM (\acc v -> do shouldInclude <- arbitrary
                                       return $ if shouldInclude
                                                then Set.insert v acc
                                                else acc)
                         Set.empty
                         vars) `suchThat` (not . Set.null)
  varmap <- (foldM (\acc v -> do coeff <- arbitraryRational
                                 return $ Map.insert v coeff acc)
                   Map.empty
                   varsIncluded) `suchThat` (not . Map.null)
  const <- abs <$> arbitraryRational
  return (LinExpr varmap const)

solutionIsFeasible :: OptimalCheck -> Bool
solutionIsFeasible opt@(OptimalCheck constraints objective feasibles) =
  trace ("Example Test:" ++ show opt) $
  let tableau         = execState (mapM_ addConstraint constraints) (newTableau objective)
      optimalTableau  = primalSimplex tableau
      optimalSolution = solution optimalTableau
  in  all (isFeasible optimalSolution) constraints
