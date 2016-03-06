{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lisper.Eval (eval, exec, progn, resolve, run) where

import Lisper.Core
import Lisper.Parser
import Lisper.Primitives
import Data.Maybe (fromJust)
import Data.List (nub, (\\))

-- Evaluate an expression and return the new environment and the result of the
-- evaluation. Env should mostly be unmodified unless the body is of the form of
-- a `define` or a `set`. A `set` or `define` should return an environment with
-- a new key, while a let expression should return the same env unmodified.

eval :: Env -> LispVal -> (Env, LispVal)

-- Eval on primitive values is no-op
eval env val@(List []) = (env, val)
eval env val@(String _) = (env, val)
eval env val@(Number _) = (env, val)
eval env val@(Bool _) = (env, val)
eval env val@(Function{}) = (env, val)
eval env val@(DottedList _ _) = (env, val)

eval env (List [Quote, val]) = (env, val)

-- Variable lookup, forcing an evaluation
eval env (Atom key) = eval env $ fromJust $ resolve env key

-- Let special form
eval env (Let args body) = (env, snd $ eval extended body)
    where
      -- Transforms a let args tuple list to env
      argsToEnv :: LispVal -> Env
      argsToEnv (List xs) = map (\(List[Atom a, val]) -> (a, val)) xs
      argsToEnv _ = error "Second argument to let should be an alist"

      extended :: Env
      extended = argsToEnv args ++ env

-- If special form
eval env (If predicate conseq alt) =
    let f NIL = eval env alt
        f (Bool True) = eval env conseq
        f (Bool False) = eval env alt
        f _ = error "If needs a Boolean predicate"
    in f $ snd $ eval env predicate

-- Set special form
eval env (Set var val) =
    let real = snd $ eval env val
    in ((var, real) : env, real)

-- Function definitions
eval env (Define name args body) =
    case duplicates args of
      [] -> (env', fn)
      x -> error $ "Duplicate argument " ++ show x ++ " in function definition"
    where
      fn = Function env' (Just name) args body
      env' = (name, fn) : env

-- Lambda definition
eval env (Lambda args body) =
    case duplicates args of
      [] -> (env, fn)
      x -> error $ "Duplicate argument " ++ show x ++ " in function definition"
  where fn = Function env Nothing args body

-- Function application with name
eval env (List (Atom func : args)) =
    case lookup func env of
      -- Function application with name
      Just fn -> apply env fn args
      Nothing -> (env, applyPrimitive func $ map (snd . eval env) args)

-- Inline function invocation
eval env (List (function : args)) = apply env fn args
  where fn = snd $ eval env function

-- Apply a function with a list of arguments

-- The `alist` is constructed in such a way that all bindings refer to concrete
-- values, rather than other references.
--
-- The alist of the form `((x a))`, rather than `((x 42))` will cause `a` to be
-- looked up in the function closure, rather than the caller's environment.
-- This is prevented by `resolving` the value of each argument to a value other
-- than an atom before zipping with the formal arguments.
--
-- This gives the added benefit that the caller's environment is not needed
-- while evaluating the function body, preventing behavior similar to dynamic
-- scoping.
apply :: Env -> LispVal -> [LispVal] -> (Env, LispVal)
apply env (Function closure _name formal body) args =
    (env, snd $ eval closure $ Let alist body)

  where
    -- We are strict! Zipper evaluates arguments before passing to functions
    zipper :: LispVal -> LispVal -> LispVal
    zipper x (Atom y) = List [x, fromJust $ resolve env y]
    zipper x value = List [x, snd $ eval env value]

    alist :: LispVal
    alist = List $ zipWith zipper formal args

apply _env fn _args = error $ "Apply needs a function, called with " ++ show fn

-- Progn, evaluate a list of expressions sequentially
progn :: Env -> [LispVal] -> (Env, LispVal)
progn env [] = (env, NIL)
progn env [x] = eval env x
progn env (x:xs) = case eval env x of
                     (env', _) -> progn env' xs

-- Evaluate a script and return result and env
run :: String -> (Env, LispVal)
run = progn [] . readExpr

-- Evaluate a script and return result
exec :: String -> LispVal
exec = snd . run

-- Helpers
applyPrimitive :: String -> [LispVal] -> LispVal
applyPrimitive func args =
    case lookup func primitives of
      Just primitive -> primitive args
      Nothing -> error $ "Undefined function " ++ show func

-- Return duplicate items in the list
duplicates :: [LispVal] -> [LispVal]
duplicates xs = xs \\ nub xs

-- Resolves a variable reference to a concrete value by walking up the link
resolve :: Env -> String -> Maybe LispVal
resolve env key =
    case lookup key env of
      Just (Atom link) -> resolve env link
      Just v -> Just v
      Nothing -> Nothing
