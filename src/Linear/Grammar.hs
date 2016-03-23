module Linear.Grammar where

import qualified Data.Map as Map


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

isMainVar (MainVar _) = True
isMainVar _           = False


-- * Sets of Variables

-- | Unique mapping from variable names to their coefficient
type LinVarMap name = Map.Map name Rational

coeffAt :: Ord name => name -> LinVarMap name -> Rational
coeffAt name varmap =
  case Map.lookup name varmap of
    Just coeff -> coeff
    Nothing    -> 0


-- * Expressions

data LinExpr name =
  LinExpr { linExprVars  :: LinVarMap name
          , linExprConst :: Rational
          }
  deriving (Show)

addVar :: Ord name => name -> Rational -> LinExpr name -> LinExpr name
addVar name coeff (LinExpr varmap const) =
  LinExpr (Map.unionWith (+) (Map.singleton name coeff) varmap) const

-- | Multiplies the whole expression by some coefficient
-- FIXME: What if I multiply by 0?
magnify :: Rational -> LinExpr name -> LinExpr name
magnify x (LinExpr varmap const) =
  LinExpr (fmap (* x) varmap) (const * x)

-- | Will fail if the variable isn't used in the expression
orient :: Ord name => LinExpr name -> name -> Maybe (LinExpr name)
orient (LinExpr varmap const) name = do
  coeff <- Map.lookup name varmap
  return $ LinExpr (fmap (/ coeff) varmap) (const / coeff)

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
  deriving (Show)

-- | This is only used in the user-facing API, hence why we hardcode the variable name
--   type to 'MainVarName'.
data LinIneqExpr =
  LinIneqExpr { linIneqSign :: LinIneq
              , linIneqExpr :: LinExpr MainVarName
              }
  deriving (Show)
