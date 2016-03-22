module Linear.Grammar where

import qualified Data.Map as Map


type MainVarName  = String
type SlackVarName = Int

data VarName
  = MainVar  MainVarName
  | SlackVar SlackVarName
  deriving (Show, Eq, Ord)


-- | Unique mapping from variable names to their coefficient
type LinVarMap = Map.Map VarName Rational

coeffAt :: VarName -> LinVarMap -> Rational
coeffAt name varmap =
  case Map.lookup name varmap of
    Just coeff -> coeff
    Nothing    -> 0

data LinExpr =
  LinExpr { linExprVars  :: LinVarMap
          , linExprConst :: Rational
          }
  deriving (Show)

addVar :: VarName -> Rational -> LinExpr -> LinExpr
addVar name coeff (LinExpr varmap const) =
  LinExpr (Map.unionWith (+) (Map.singleton name coeff) varmap) const

-- | Will fail if the variable isn't used in the expression
orient :: LinExpr -> VarName -> Maybe LinExpr
orient (LinExpr varmap const) name = do
  coeff <- Map.lookup name varmap
  return $ LinExpr (fmap (/ coeff) varmap) const


data LinIneq
  = Equ
  | Lteq
  | Gteq
  deriving (Show)


data LinIneqExpr =
  LinIneqExpr { linIneqSign :: LinIneq
              , linIneqExpr :: LinExpr
              }
  deriving (Show)
