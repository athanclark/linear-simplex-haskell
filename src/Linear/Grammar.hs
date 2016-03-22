module Linear.Grammar where

import qualified Data.Map as Map


type MainVarName  = String
type SlackVarName = Int
type ErrorVarName = String
type ArtifVarName = Int

data ErrorVarSign
  = ErrorPos
  | ErrorNeg
  deriving (Show, Eq, Ord)

data VarName
  = MainVar  MainVarName
  | SlackVar SlackVarName
  | ErrorVar ErrorVarName ErrorVarSign
  | ArtifVar ArtifVarName
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

-- | Multiplies the whole expression by some coefficient
-- FIXME: What if I multiply by 0?
magnify :: Rational -> LinExpr -> LinExpr
magnify x (LinExpr varmap const) =
  LinExpr (fmap (* x) varmap) (const * x)

-- | Will fail if the variable isn't used in the expression
orient :: LinExpr -> VarName -> Maybe LinExpr
orient (LinExpr varmap const) name = do
  coeff <- Map.lookup name varmap
  return $ LinExpr (fmap (/ coeff) varmap) const

-- | Remove a variable from an expression, by leveraging an expression to
--   facilitate the (sound) removal.
substitute :: VarName -- ^ Variable to orient
           -> LinExpr -- ^ Expression /removed/
           -> LinExpr -- ^ Target Expression
           -> Maybe LinExpr
substitute name toRemove (LinExpr varmap const) = do
  toRemove' <- orient toRemove name
  let coeff = coeffAt name varmap
      (LinExpr varmap' const') = magnify coeff toRemove'
  return $ LinExpr (Map.unionWith (-) varmap varmap') (const - const')


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
