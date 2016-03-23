module Linear.Simplex where

import Linear.Grammar
import Linear.Context

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
primalPivot (Tableau objective context) =
  let isMainVar (MainVar _) = True
      isMainVar _           = False

      qualifiedMainVars :: LinVarMap VarName
      qualifiedMainVars = filter (< 0) $
        Map.mapWithKey (\k c -> if isMainVar k
                                then -c
                                else c) (linExprVars objective)

      isOptimal :: Bool
      isOptimal = Map.null qualifiedMainVars

  in  guard (not isOptimal) $ do
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

-- | Using Bland's finite pivoting method
blandRatio :: VarName -> LinExpr VarName -> Maybe Rational
blandRatio name (LinExpr varmap const) = do
  coeff <- Map.lookup name varmap
  guard (coeff > 0) -- FIXME: View Bland's source, this is wonky.
  let ratio = const / coeff
  guard (ratio > 0)
  return ratio
