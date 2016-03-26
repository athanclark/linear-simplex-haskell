module Linear.Grammar where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)


-- * Variable Names

type MainVarName  = String
type SlackVarName = Int
type ErrorVarName = String
type ArtifVarName = Int

data ErrorVarSign
  = ErrorPos
  | ErrorNeg
  deriving (Show, Eq, Ord)

data VarName
  = MainVar  { mainVarName :: MainVarName }
  | SlackVar SlackVarName
  | ErrorVar ErrorVarName ErrorVarSign
  | ArtifVar ArtifVarName
  deriving (Show, Eq, Ord)

isMainVar :: VarName -> Bool
isMainVar (MainVar _) = True
isMainVar _           = False


-- * Sets of Variables

-- | Unique mapping from variable names to their coefficient
type LinVarMap name = Map.Map name Rational

-- | If a coefficient doesn't exist in the map, it's coefficient is @0@.
coeffAt :: Ord name => name -> LinVarMap name -> Rational
coeffAt name varmap = fromMaybe 0 (Map.lookup name varmap)


-- * Expressions

data LinExpr name =
  LinExpr { linExprVars  :: LinVarMap name
          , linExprConst :: Rational
          }
  deriving (Show, Eq)

addVar :: Ord name => name -> Rational -> LinExpr name -> LinExpr name
addVar name coeff (LinExpr varmap const) =
  LinExpr (Map.insertWith (+) name coeff varmap) const

-- | Multiplies the whole expression by some coefficient
magnify :: Rational -> LinExpr name -> LinExpr name
magnify x (LinExpr varmap const) =
  LinExpr ((* x) <$> varmap) (const * x)

-- | Will fail if the variable isn't used in the expression
orient :: Ord name => LinExpr name -> name -> Maybe (LinExpr name)
orient expr@(LinExpr varmap _) name = do
  coeff <- Map.lookup name varmap
  return (magnify (recip coeff) expr)

-- | Remove a variable from an expression, by leveraging an expression to
--   facilitate the (sound) removal.
substitute :: ( Ord name
              ) => name         -- ^ Variable to orient
                -> LinExpr name -- ^ Expression /removed/
                -> LinExpr name -- ^ Target Expression
                -> Maybe (LinExpr name)
substitute name toRemove (LinExpr varmap const) = do
  toRemove' <- orient toRemove name
  let coeff = coeffAt name varmap
      (LinExpr varmap' const') = magnify coeff toRemove'
  return $ LinExpr
             (Map.filter (/= 0) $ Map.unionWith (-) varmap varmap')
             (const - const')


-- * Inequality Expressions

data LinIneq
  = Equ
  | Lteq
  | Gteq
  deriving (Show, Eq)

-- | This is only used in the user-facing API, hence why we hardcode the variable name
--   type to 'MainVarName'.
data LinIneqExpr =
  LinIneqExpr { linIneqSign :: LinIneq
              , linIneqExpr :: LinExpr MainVarName
              }
  deriving (Show, Eq)

-- | Evaluate a variable with a value in this expression
evaluate :: Ord name => name -> Rational -> LinExpr name -> LinExpr name
evaluate var value (LinExpr varmap const) =
  let coeff = coeffAt var varmap
  in  LinExpr (Map.delete var varmap) (const - (coeff * value))


-- | Checks to see if a set of values is feasible with respect to this inequality
isFeasible :: Map.Map MainVarName Rational -> LinIneqExpr -> Bool
isFeasible solution (LinIneqExpr sign (LinExpr varmap const)) =
  let value = sum (Map.intersectionWith (*) solution varmap)
  in  case sign of
    Equ  -> value == const
    Lteq -> value <= const
    Gteq -> value >= const
