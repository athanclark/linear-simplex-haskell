{-# LANGUAGE
    FlexibleContexts
  #-}

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
  case sign of
    Equ  -> put $ Context slack (artif + 1) (Map.insert newBasic expr constraints)
    Lteq -> put $ Context (slack + 1) artif (Map.insert newBasic expr constraints)
    Gteq -> let newExpr = magnify (-1) expr
            in put $ Context (slack + 1) artif (Map.insert newBasic newExpr constraints)


-- | Builds the substitution expression for error variables
errorExpr :: MainVarName -> LinExpr
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
