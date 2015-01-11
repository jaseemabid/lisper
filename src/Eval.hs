{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval (eval, progn, exec) where

import Core
import Parser
import Primitives

-- Evaluate an expression and return the new environment and the result of the
-- evaluation. Env should mostly be unmodified unless the body is of the form of
-- a `defun` or a `set`. A `set` or `defun` should return an environment with a
-- new key, while a let expression should return the same env unmodified.

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
eval env (Set var val) = ((var, val) : env, val)

-- Function definitions
eval env (Defun name args body) = ((name, fn) : env, fn) where
    fn = Function env name args body

-- Function application
eval env (List (Atom func : args)) =
    case lookup func env of
      Just(Function closure _ formal body) ->
          let
              args' = List $ zipWith (\x y -> List [x, y]) formal args
          in
            eval closure (Let args' body)
      Nothing -> (env, apply func $ map (snd . eval env) args)

-- Progn, evaluate a list of expressions sequentially
progn :: Env -> [LispVal] -> (Env, LispVal)
progn env [x] = eval env x
progn env (x:xs) = case eval env x of
                     (env', _) -> progn env' xs

-- Top level evaluator.
exec :: String -> IO ()
exec = print . snd . progn [] . readExpr

-- Helpers
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (error err) ($ args) $ lookup func primitives where
    err = "Undefined function " ++ show func
