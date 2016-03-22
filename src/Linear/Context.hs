module Linear.Context where

import Linear.Grammar
import qualified Data.Set as Set


data Context =
  Context { contextSlackState  :: Int -- the latest generated slack name
          , contextConstraints :: [LinExpr]
          }

initContext :: Context
initContext = Context 0 []

addConstraint :: LinIneqExpr -> Context -> Context
addConstraint (LinIneqExpr sign expr) (Context slack constraints) =

  where
    makeSlackVar :: LinIneqExpr -> Context -> (LinExpr, Context)
    makeSlackVar (LinIneqExpr sign expr) ctx@(Context slack cs) =
      case sign of
        Equ  -> (expr, ctx)
        Lteq -> let newExpr = addVar (SlackVarName slack) 1 expr
                in  (newExpr, Context (slack + 1) (newExpr : cs))
        Gteq -> let newExpr = addVar (SlackVarName slack) (-1) expr
                in  (newExpr, Context (slack + 1) (newExpr : cs))
