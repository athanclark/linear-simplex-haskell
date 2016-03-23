module Linear.Simplex where

import Linear.Grammar
import Linear.Context

import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Monad.State


-- * Tableau

data Tableau =
  Tableau { tableauObjective :: LinExpr VarName
          , tableauContext   :: Context
          }
  deriving (Show)

newTableau :: LinExpr MainVarName
           -> Tableau
newTableau objective =
  let (objective', mainVars) = sanitizeExpr objective
  in  Tableau objective' (initContext { contextMainVars = mainVars
                                      })


-- * Primal Simplex

primalPivot :: Tableau -> Maybe Tableau
primalPivot (Tableau objective context) = do
  let qualifiedMainVars :: LinVarMap VarName
      qualifiedMainVars = Map.filter (< 0) $
        Map.mapWithKey (\k c -> if isMainVar k
                                then -c
                                else c) (linExprVars objective)

      isOptimal :: Bool
      isOptimal = Map.null qualifiedMainVars

  guard (not isOptimal)
  let entry :: VarName
      entry = let go :: VarName -- Objective variable
                      -> Rational -- Objective coefficient
                      -> Maybe (VarName, Rational)
                      -> Maybe (VarName, Rational)
                  go k c Nothing = Just (k, c)
                  go k c acc@(Just (k', c')) | c < c'    = Just (k, c)
                                              | otherwise = acc
              in  fromJust $ fst <$> Map.foldrWithKey go Nothing qualifiedMainVars

      leaving :: VarName
      leaving = let go :: VarName -- Basic variable
                        -> LinExpr VarName -- Constraint
                        -> Maybe (VarName, Rational)
                        -> Maybe (VarName, Rational)
                    go name expr Nothing = (\ratio -> (name, ratio))
                                        <$> blandRatio entry expr
                    go name expr acc@(Just (k, ratio)) = case blandRatio entry expr of
                      Just ratio' | ratio' < ratio -> Just (name, ratio')
                      _                            -> acc
                in  fromJust $ fst <$> Map.foldrWithKey go Nothing (contextConstraints context)

      -- Note that the entry var is still intact
      exprToRemove :: LinExpr VarName
      exprToRemove = let expr = fromJust $ Map.lookup leaving (contextConstraints context)
                     in  expr { linExprVars = Map.insert leaving 1 (linExprVars expr)
                              }

      exprToInclude :: LinExpr VarName
      exprToInclude = exprToRemove { linExprVars = Map.delete entry (linExprVars exprToRemove)
                                   }

      constraints :: Map.Map VarName (LinExpr VarName)
      constraints = Map.insert entry exprToInclude
                  $ fmap (fromJust . substitute entry exprToRemove)
                  $ Map.delete leaving (contextConstraints context)

  return $ Tableau
             (fromJust $ substitute entry exprToRemove objective)
             (context { contextConstraints = constraints
                      })

primalSimplex :: Tableau -> Tableau
primalSimplex tab = case primalPivot tab of
  Nothing   -> tab
  Just tab' -> primalSimplex tab'

-- | Using Bland's finite pivoting method
blandRatio :: VarName -> LinExpr VarName -> Maybe Rational
blandRatio name (LinExpr varmap const) = do
  coeff <- Map.lookup name varmap
  guard (coeff > 0) -- FIXME: View Bland's source, this is wonky.
  let ratio = const / coeff
  -- guard (ratio > 0)
  return ratio


-- | The dereferenced basic feasible solution
solution :: Tableau -> Map.Map MainVarName Rational
solution (Tableau _ context) =
  let mainVars = contextMainVars context
      varmap   = basicFeasibleSolution context
      tempExpr = LinExpr varmap 0
      (LinExpr varmap' _) =
        foldr (\name -> fromJust . derefError name) tempExpr mainVars
  in  Map.mapKeys mainVarName $
        Map.filterWithKey (\k _ -> isMainVar k) varmap'
