{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Context where

import Linear.Grammar
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Monad.State


-- | The stateful component of the computation
data Context =
  Context { contextSlackState  :: Int -- the latest generated slack name
          , contextArtifState  :: Int -- the latest generated artificial var name
          , contextConstraints :: Map.Map VarName (LinExpr VarName)
          }

initContext :: Context
initContext = Context 0 0 Map.empty


addConstraint :: ( MonadState Context m
                 ) => LinIneqExpr -> m ()
addConstraint (LinIneqExpr sign expr) = do
  (Context slack artif constraints) <- get
  let newBasic :: VarName
      newBasic = case sign of
        Equ  -> ArtifVar artif
        Lteq -> SlackVar slack
        Gteq -> SlackVar slack
      newExpr :: LinExpr VarName
      newExpr  = let mainVars :: [MainVarName]
                     mainVars = Map.keys (linExprVars expr)
                     substitution :: LinExpr VarName -> MainVarName -> Maybe (LinExpr VarName)
                     substitution expr' name =
                       substitute (MainVar name) (errorExpr name) expr'
                     expr' :: LinExpr VarName
                     expr' = expr { linExprVars = Map.mapKeys MainVar (linExprVars expr)
                                  }
                 in  fromJust $ foldM substitution expr' mainVars
  case sign of
    Equ  -> put $ Context slack (artif + 1) (Map.insert newBasic newExpr constraints)
    Lteq -> put $ Context (slack + 1) artif (Map.insert newBasic newExpr constraints)
    Gteq -> let newExpr' = magnify (-1) newExpr
            in put $ Context (slack + 1) artif (Map.insert newBasic newExpr' constraints)


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
