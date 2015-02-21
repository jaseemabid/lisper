{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval (eval, progn, exec) where

import Core
import Parser
import Primitives
import Data.List (nub, (\\))

-- Evaluate an expression and return the new environment and the result of the
-- evaluation. Env should mostly be unmodified unless the body is of the form of
-- a `define` or a `set`. A `set` or `define` should return an environment with
-- a new key, while a let expression should return the same env unmodified.

eval :: Env -> LispVal -> (Env, LispVal)

eval env NIL = (env, NIL)
eval env val@(String _) = (env, val)
eval env val@(Number _) = (env, val)
eval env val@(Bool _) = (env, val)

eval env (List [Quote, val]) = (env, val)

-- Variable lookup, forcing an evaluation
eval env (Atom key) = eval env $ resolve env key

-- Let special form
eval env (Let args body) = eval' env args
    where
      makeEnv :: LispVal -> Env -> Env
      makeEnv item env' =
          case item of
            List[Atom a, val] -> (a, val) : env'

      eval' env' args' =
          case args' of
            List[v] -> (env, snd $ eval (makeEnv v env') body)
            List(v:rest) -> eval' (makeEnv v env') (List rest)
            _ -> error "Second argument to let should be an alist"

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
      [] -> ((name, fn) : env, fn)
      x -> error $ "Duplicate argument " ++ show x ++ " in function definition"
    where fn = Function env (Just name) args body

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
      Just fn@Function {} -> apply env fn args

      -- Lambda invocation with name
      Just lambda@Lambda {} -> apply env fn args
          where fn = snd $ eval env lambda

      Nothing -> (env, applyPrimitive func $ map (snd . eval env) args)

-- Inline lambda invocation
eval env (List (lambda@Lambda {} : args)) = apply env fn args
    where fn = snd $ eval env lambda

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
apply env fn args = case fn of
      (Function closure _ formal body) ->
          let
              zipper x (Atom y) = List [x, resolve env y]
              alist = List $ zipWith zipper formal args
          in
            (env, snd $ eval closure $ Let alist body)

-- Progn, evaluate a list of expressions sequentially
progn :: Env -> [LispVal] -> (Env, LispVal)
progn env [] = (env, NIL)
progn env [x] = eval env x
progn env (x:xs) = case eval env x of
                     (env', _) -> progn env' xs

-- Top level evaluator.
exec :: String -> IO ()
exec = print . snd . progn [] . readExpr

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
resolve :: Env -> String -> LispVal
resolve env key =
    case lookup key env of
      Just (Atom link) -> resolve env link
      Just v -> v
      Nothing -> error $ "Undefined variable " ++ key
