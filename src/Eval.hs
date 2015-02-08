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
eval env (Atom key) = case lookup key env of
                        Just v -> eval env v
                        Nothing -> error $ "Undefined variable " ++ key

-- Let special form
eval env (Let args body) = eval' env args
    where
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
    case val of
      Atom alias ->
          case lookup alias env of
            Just link -> eval env (Set var link)
            Nothing -> error $ "Undefined variable " ++ show alias
      _ -> ((var, val) : env, val)

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
apply :: Env -> LispVal -> [LispVal] -> (Env, LispVal)
apply env fn args = case fn of
      (Function closure _ formal body) ->
          let
              alist = List $ zipWith (\x y -> List [x, y]) formal args
          in
            (env, snd $ eval (closure ++ env) (Let alist body))

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
