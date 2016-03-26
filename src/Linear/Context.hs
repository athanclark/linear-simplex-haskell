{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Context where

import Linear.Grammar
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State


-- | The stateful component of the computation
data Context =
  Context { -- | latest generated slack name
            contextSlackState  :: Int
          , -- | latest generated artificial var name
            contextArtifState  :: Int
          , -- | unrestricted variable names
            contextMainVars    :: Set.Set MainVarName
          , -- | the constraints, indexed by basic variables
            contextConstraints :: Map.Map VarName (LinExpr VarName)
          }
  deriving (Show)

-- | The empty context
initContext :: Context
initContext = Context 0 0 Set.empty Map.empty

-- | Add a constraint into the context
addConstraintContext :: ( MonadState Context m
                        ) => LinIneqExpr -> m ()
addConstraintContext (LinIneqExpr sign expr) = do
  (Context slack artif mainVars constraints) <- get
  let (LinExpr varmap const, newMainVars) = sanitizeExpr expr
      (sign', newExpr) =
        if const < 0
        then (invertSign sign, LinExpr (negate <$> varmap) (negate const))
        else (sign           , LinExpr varmap const)
      newBasic :: VarName
      newBasic = case sign' of
        Equ  -> ArtifVar artif
        Lteq -> SlackVar slack
        Gteq -> ArtifVar artif
  case sign' of
    Equ  -> put $ Context
                    slack
                    (artif + 1)
                    (Set.union mainVars newMainVars)
                    (Map.insert newBasic newExpr constraints)
    Lteq -> put $ Context
                    (slack + 1)
                    artif
                    (Set.union mainVars newMainVars)
                    (Map.insert newBasic newExpr constraints)
    Gteq -> let newExpr' = newExpr { linExprVars = Map.insert
                                                     (SlackVar slack)
                                                     (-1)
                                                     (linExprVars newExpr)
                                   }
            in  put $ Context
                        (slack + 1)
                        (artif + 1)
                        (Set.union mainVars newMainVars)
                        (Map.insert newBasic newExpr' constraints)


-- | Escapes the unrestricted variables to their restricted form
sanitizeExpr :: LinExpr MainVarName
             -> (LinExpr VarName, Set.Set MainVarName)
sanitizeExpr expr =
  let mainVars :: Set.Set MainVarName
      mainVars = Map.keysSet (linExprVars expr)

      substitution :: MainVarName -> LinExpr VarName -> LinExpr VarName
      substitution name expr' =
        case substitute (MainVar name) (errorExpr name) expr' of
          Nothing     -> error "substitution failed"
          Just expr'' -> expr''

      -- pun intended
      wellNamedExpr :: LinExpr VarName
      wellNamedExpr = expr { linExprVars = Map.mapKeys MainVar (linExprVars expr)
                           }

  in  (foldr substitution wellNamedExpr mainVars, mainVars)

-- | Builds the substitution expression for error variables
errorExpr :: MainVarName -> LinExpr VarName
errorExpr name =
  let varmap = Map.insert (MainVar name) (-1) errorDiff
  in  LinExpr varmap 0
  where
    --   x = (x_e_+) - (x_e_-)
    -- ~ 0 = (x_e_+) - (x_e_-) - x
    errorDiff = Map.fromList [ (ErrorVar name ErrorPos, 1)
                             , (ErrorVar name ErrorNeg, -1)
                             ]

-- | Replace error variable combo with its corrosponding main variable
derefError :: ErrorVarName -> LinExpr VarName -> Maybe (LinExpr VarName)
derefError name (LinExpr varmap const) = do
  let posName = ErrorVar name ErrorPos
      negName = ErrorVar name ErrorNeg
  posCoeff <- Map.lookup posName varmap
  negCoeff <- Map.lookup negName varmap
  let varmap'  = Map.delete negName . Map.delete posName $ varmap
      varmap'' = Map.insert (MainVar name) (posCoeff - negCoeff) varmap'
  return $ LinExpr varmap'' const


-- | Using the basic variables in the context, map to their constraints' constant values.
basicFeasibleSolution :: Context -> Map.Map VarName Rational
basicFeasibleSolution (Context _ _ _ constraints) = linExprConst <$> constraints
