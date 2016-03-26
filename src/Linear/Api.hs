module Linear.Api where

import Linear.Grammar

import qualified Data.Map as Map


-- | Convenience for building variable maps
(.+.) :: LinVarMap MainVarName -> (MainVarName, Rational) -> LinVarMap MainVarName
varmap .+. (name, coeff) = Map.insert name coeff varmap

infixl 7 .+.


(.==.) :: LinVarMap MainVarName -> Rational -> LinIneqExpr
varmap .==. const = LinIneqExpr Equ (LinExpr varmap const)

infixl 6 .==.

(.<=.) :: LinVarMap MainVarName -> Rational -> LinIneqExpr
varmap .<=. const = LinIneqExpr Lteq (LinExpr varmap const)

infixl 6 .<=.

(.>=.) :: LinVarMap MainVarName -> Rational -> LinIneqExpr
varmap .>=. const = LinIneqExpr Gteq (LinExpr varmap const)

infixl 6 .>=.


-- Should work like this:
-- Map.empty .+. ("foo", 5) .+. ("bar", 6) .+. ("baz", 7) .<=. 10 :: LinIneqExpr

