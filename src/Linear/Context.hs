module Linear.Context where

import Linear.Grammar
import qualified Data.Map as Map
import Control.Monad.State

data Context =
  Context { contextSlackState  :: Int -- the latest generated slack name
          , contextArtifState  :: Int -- the latest generated artificial var name
          , contextConstraints :: Map.Map VarName LinExpr
          }

initContext :: Context
initContext = Context 0 Map.empty


addConstraint :: ( MonadState Context m
                 ) => LinIneqExpr -> m ()
addConstraint (LinIneqExpr sign expr) (Context slack constraints) =
  
  where
    makeSlackVar :: ( MonadState Context m
                    ) => LinIneqExpr -> m LinExpr
    makeSlackVar (LinIneqExpr sign expr) ctx@(Context slack cs) =
      case sign of
        Equ  -> (expr, ctx)
        Lteq -> let newExpr = addVar (SlackVarName slack) 1 expr
                in  (newExpr, Context (slack + 1) (newExpr : cs))
        Gteq -> let newExpr = addVar (SlackVarName slack) (-1) expr
                in  (newExpr, Context (slack + 1) (newExpr : cs))


-- | Builds the substitution expression for error variables
errorExpr :: VarName -> LinExpr
errorExpr name =
  LinExpr (Map.union (Map.singleton (MainVar name) (-1))
                     errorDiff)
          0
  where
    --   x = (x_e_+) - (x_e_-)
    -- ~ 0 = (x_e_+) - (x_e_-) - x
    errorDiff = Map.fromList [ (ErrorVar name ErrorPos, 1)
                             , (ErrorVar name ErrorNeg, -1)
                             ]
