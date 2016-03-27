{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Simplex where

import Linear.Grammar
import Linear.Context

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error (throwError)

import Debug.Trace


-- * Tableau

data Tableau =
  Tableau { -- | For dereferencing the basic feasible solution into a summed
            --   total - the original objective function.
            tableauObjectiveDeref   :: LinVarMap MainVarName
          , -- | For use during the computation itself.
            tableauObjectiveCompute :: LinVarMap VarName
          , -- | The constraints themselves
            tableauContext          :: Context
          }
  deriving (Show)

-- | Create a new tableau, starting with an objective measurement / function.
--   Note that the @z@ value is implicit across this summation of variables -
--   @(a1 * x1) + (a2 * x2) + (a3 * x3) = z@
newTableau :: LinVarMap MainVarName
           -> Tableau
newTableau objective =
  let (LinExpr objective' _, mainVars) = sanitizeExpr (LinExpr objective 0)
  in  Tableau
        objective
        (negate <$> objective')
        (initContext {contextMainVars = mainVars})

-- | Adds a constraint to the tableau
addConstraint :: ( MonadState Tableau m
                 ) => LinIneqExpr -> m ()
addConstraint ineq =
  modify go
  where
    go (Tableau deref objective context) =
      Tableau deref objective
        (execState (addConstraintContext ineq) context)

-- * Primal Simplex

data PivotError
  = IsOptimal
  | UnboundedSolution
  deriving (Show, Eq)

primalPivot :: Tableau -> Either PivotError Tableau
primalPivot (Tableau deref objective context) = do
  let isOptimal :: Bool
      isOptimal = Map.null (Map.filter (< 0) objective)

  when isOptimal (throwError IsOptimal)

  let entry :: VarName
      entry = let go :: VarName -- Objective variable
                     -> Rational -- Objective coefficient
                     -> Maybe (VarName, Rational)
                     -> Maybe (VarName, Rational)
                  go k c Nothing = Just (k, c)
                  go k c acc@(Just (k', c')) | c < c'    = Just (k, c)
                                             | otherwise = acc
              in  case fst <$> Map.foldrWithKey go Nothing objective of
                    Nothing   -> error "impossible case - qualified vars empty"
                    Just name -> name

  trace ("Entry var: " ++ show entry) $ do
    leaving <- let go :: VarName -- Basic variable
                      -> LinExpr VarName -- Constraint
                      -> Maybe (VarName, Rational)
                      -> Maybe (VarName, Rational)
                   go name expr Nothing = (\ratio -> (name, ratio))
                                       <$> primalBlandRatio entry expr
                   go name expr acc@(Just (k, ratio)) = case primalBlandRatio entry expr of
                     Just ratio' | ratio' < ratio -> Just (name, ratio')
                     _                            -> acc
              in  case fst <$> Map.foldrWithKey go Nothing (contextConstraints context) of
                    Nothing   -> throwError UnboundedSolution
                    Just name -> return name

        -- Note that the entry var is still intact
    let exprToRemove :: LinExpr VarName
        exprToRemove = let expr = case Map.lookup leaving (contextConstraints context) of
                                    Nothing    -> error "impossible - leaving var not in constraints"
                                    Just expr' -> expr'
                       in  expr { linExprVars = Map.insert leaving 1 (linExprVars expr)
                                }

        exprToInclude :: LinExpr VarName
        exprToInclude = exprToRemove { linExprVars = Map.delete entry (linExprVars exprToRemove)
                                     }

        constraints :: Map.Map VarName (LinExpr VarName)
        constraints = Map.insert entry exprToInclude
                    $ fmap (\expr -> case substitute entry exprToRemove expr of
                                       Nothing    -> error "impossible - entry not in expression to remove"
                                       Just expr' -> expr')
                    $ Map.delete leaving (contextConstraints context)

    return $ Tableau
              deref
              (case substitute entry exprToRemove (LinExpr objective 0) of
                  Nothing                     -> error "impossible - entry not in expression to remove, for objective"
                  Just (LinExpr objective' _) -> objective')
              (context { contextConstraints = constraints
                       })

primalSimplex :: Tableau -> Either PivotError Tableau
primalSimplex tab =
  trace "Pivoting..." $
  let postPivot = primalPivot tab
  in case postPivot of
       Left err | err == IsOptimal -> Right tab
                | otherwise        -> Left err
       Right tab'                  -> primalSimplex tab'

-- | Using Bland's finite pivoting method
primalBlandRatio :: VarName -> LinExpr VarName -> Maybe Rational
primalBlandRatio name (LinExpr varmap const) = do
  coeff <- Map.lookup name varmap
  guard (coeff > 0)
  let ratio = const / coeff
  return ratio


-- | The dereferenced basic feasible solution
solution :: Tableau -> Map.Map MainVarName Rational
solution (Tableau _ _ context) =
  let mainVars :: Set.Set MainVarName
      mainVars = contextMainVars context

      varmap :: Map.Map VarName Rational
      varmap = basicFeasibleSolution context

      tempExpr :: LinExpr VarName
      tempExpr = LinExpr varmap 0

      go :: MainVarName
         -> LinExpr VarName
         -> LinExpr VarName
      go name expr = case derefError name expr of
                       Nothing    -> error $ "error variable " ++ show name ++ " not in expression"
                       Just expr' -> expr'
      (LinExpr varmap' _) = foldr go tempExpr mainVars
  in  Map.mapKeys mainVarName $
        Map.filterWithKey (\k _ -> isMainVar k) varmap'

-- | Dereferences and evaluates to the @z@ value of the objective function with the
--   current basic feasible solution.
solutionValue :: Tableau -> Rational
solutionValue tab@(Tableau deref _ _) =
  let bfs = solution tab
  in  sum (Map.unionWith (*) bfs deref)
