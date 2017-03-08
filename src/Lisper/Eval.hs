{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Eval where

import Control.Monad.State.Lazy
import Data.Either
import Data.List (nub, (\\))

import Lisper.Core
import Lisper.Parser
import Lisper.Primitives

-- | Evaluate an expression and return the result or error if any.
--
-- The environment maybe updated by specifc commands like `set!`, and is handled
-- by the `State` monad. Env should be mostly left unmodified unless the body is
-- of the form of a `define` or a `set`.

-- [TODO] - Eventually replace `String` with a Stacktrace or something.
-- [TODO] - Clean up this API with a monad transformer
eval :: LispVal -> State Env (Either String LispVal)

-- Eval on primitive values is no-op
eval val@(List []) = return $ Right val
eval val@(String _) = return $ Right val
eval val@(Number _) = return $ Right val
eval val@(Bool _) = return $ Right val
eval val@Function{} = return $ Right val
eval val@(DottedList _ _) = return $ Right val

eval (List [Quote, val]) = return $ Right val

-- Variable lookup
eval (Atom key) = do
    env <- get
    case resolve key env of
      Just val -> return $ Right val
      Nothing -> return $ Left $ "Undefined variable " ++ show key

-- Let special form
eval (Let args body) = do
    env <- get
    Right env' <- alistToEnv args
    return $ evalState (progn body) (env' ++ env)

-- [TODO] - Verify default value of `cond` if no branches match
-- Return `NIL if no branches match
eval (Cond body) =
    case body of
      (List [Atom "else", value]: _xs) -> eval value
      (List [predicate, value]: xs) ->
          eval predicate >>= \case
              Right NIL -> eval (Cond xs)
              Right (Bool False) -> eval (Cond xs)
              Right _ -> eval value
              Left err -> return $ Left err
      [] -> return $ Right NIL
      err -> return $ Left $ "Syntax error: expected alist; got " ++ show err ++ " instead"

-- If special form
eval (If predicate conseq alt) = do
    Right result <- eval predicate
    case result of
        Bool True -> eval conseq
        Bool False -> eval alt
        NIL -> eval alt
        _ -> return $ Left "If needs a Boolean predicate"

-- Set special form
eval (Set var val) = do
    Right result <- eval val
    modify $ \env -> (var, result) : env
    return $ Right result

-- Function definitions
eval (Define name args body) = do
    env <- get
    case duplicates args of
      [] -> do
          put env'
          return $ Right fn
        where
          fn = Function env' (Just name) args body
          env' = (name, fn) : env
      x -> return $ Left err
        where
          err = "Duplicate argument " ++ show x ++ " in function definition"

-- Lambda definition
eval (Lambda args body) = do
    env <- get
    case duplicates args of
      [] -> return $ Right fn
        where
          fn = Function env Nothing args body
      x -> return $ Left err
        where
          err = "Duplicate argument " ++ show x ++ " in function definition"

-- Function application with name
eval (List (Atom func : args)) = do
    env <- get
    case lookup func env of
        -- Function application with name
        Just fn -> apply fn args
        Nothing -> do
            -- [TODO] - Rewrite this. This is only the first version I could
            -- manage to get compiled after a few hours of struggles with mtl
            -- errors.
            args' <- mapM (\lv -> return $ evalState (eval lv) env) args
            -- [TODO] - `rights` is probably eating errors silently
            return $ Right $ applyPrimitive func (rights args')

-- Inline function invocation
eval (List (function : args)) = do
    Right fn <- eval function
    apply fn args

-- | Apply a function with a list of arguments
--
-- The `alist` is constructed in such a way that all bindings refer to concrete
-- values, rather than other references.
--
-- The alist of the form `((x a))`, rather than `((x 42))` will cause `a` to be
-- looked up in the function closure, rather than the caller's environment. This
-- is prevented by `resolving` the value of each argument to a value other than
-- an atom before zipping with the formal arguments.
--
-- This gives the added benefit that the caller's environment is not needed
-- while evaluating the function body, preventing behavior similar to dynamic
-- scoping.
apply :: LispVal -> [LispVal] -> State Env (Either String LispVal)
apply (Function closure _name formal body) args = do
    env <- get
    local' <- zipWithM zipper formal args
    -- [TODO] - `rights` is probably eating errors silently
    let local = rights local' :: Env
    let extended = closure ++ local ++ env

    return $ evalState (progn body) extended

  where
    -- We are strict! Zipper evaluates arguments before passing to functions
    zipper :: LispVal -> LispVal -> State Env (Either String (String, LispVal))
    zipper (Atom var) val = do
        -- [TODO] - This pattern match could fail
        Right result <- eval val
        return $ Right (var, result)
    zipper a b = return $ Left $ "Malformed function arguments" ++ show a  ++ " " ++ show b

apply fn _args = return $ Left $ "Function Application Error. Fn: " ++ show fn

-- | Evaluate a list of expressions sequentially; and return the result of last
--
-- Progn needs to stop at the first failure and hence the intermediatary results
-- are forced with a `seq`. I'm not sure if this is the right way to do things,
-- but works for now.
progn :: [LispVal] -> State Env (Either String LispVal)
progn [] = return $ Right NIL
progn [x] = eval x
progn (x:xs) = eval x >>= \case
    Right lv -> seq lv $ progn xs
    err -> return err

-- | Evaluate a string and return result
--
-- This method is stateless and subsequent applications wont behave like a repl.
exec :: String -> Either String LispVal
exec str = fst $ run [] str

-- | Evaluate a string and return result, along with new env
--
-- This method is stateless and subsequent applications wont behave like a repl.
run :: Env -> String -> (Either String LispVal, Env)
run env str = case readExpr str of
                Right lv -> runState (progn lv) env
                Left err -> (Left $ show err, env)

-- Helpers
applyPrimitive :: String -> [LispVal] -> LispVal
applyPrimitive func args =
    case lookup func primitives of
        Just primitive -> primitive args
        Nothing -> error $ "Undefined primitive function " ++ show func

-- | Return duplicate items in the list
--
-- >>> duplicates [1, 2, 3, 4, 1]
-- [1]
duplicates :: [LispVal] -> [LispVal]
duplicates xs = xs \\ nub xs

-- | Transforms a let args tuple list to env
alistToEnv :: LispVal -> State Env (Either String Env)
alistToEnv (List xs) = do
    env <- get
    let (Right sets) = mapM trans xs
    let (Right _lv, env') = runState (progn sets) env
    return $ Right env'
  where
     -- | Transform an alist of the form `(a 1)` to a `(set! a 1)` expression
     trans :: LispVal -> Either String LispVal
     trans (List[Atom a, val']) = Right $ List[Atom "set!", Atom a, val']
     trans _ = Left "Malformed alist"

alistToEnv _ = return $ Left "Second argument to let should be an alist"

-- | Resolves a variable reference
resolve :: String -> Env -> Maybe LispVal
resolve = lookup
