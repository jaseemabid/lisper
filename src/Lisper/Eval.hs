{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Eval where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Either
import Data.List (nub, (\\))
import Prelude hiding (read)

import Lisper.Core
import Lisper.Parser
import Lisper.Primitives

type Result a = StateT Env (ExceptT String Identity) a

-- | Evaluate an expression and return the result or error if any.
--
-- The environment maybe updated by specifc commands like `set!`, and is handled
-- by the `State` monad. Env should be mostly left unmodified unless the body is
-- of the form of a `define` or a `set`.
eval :: Scheme -> Result Scheme

-- Eval on primitive values is no-op:
eval val@(Bool _) = return val
eval val@(List []) = return val
eval val@(Number _) = return val
eval val@(Pair _ _) = return val
eval val@Procedure{} = return val
eval val@(String _) = return val

eval (List [Quote, val]) = return val

-- Variable lookup
eval (Symbol key) = do
    env <- get
    case lookup key env of
        Just val -> return val
        Nothing -> throwError $ "Undefined variable `" ++ key ++ "`"

-- Let special form
-- [TODO] - `let` should be implemented as a macro
eval (Let args body) = do
    arguments <- alistToEnv args
    withStateT (arguments ++) $ progn body

-- [TODO] - Verify default value of `cond` if no branches match
-- [TODO] - `cond` should be implemented as a macro
-- Return `NIL if no branches match
eval (Cond body) =
    case body of
      (List [Symbol "else", value]: _xs) -> eval value
      (List [predicate, value]: xs) ->
          eval predicate >>= \case
              NIL -> eval (Cond xs)
              Bool False -> eval (Cond xs)
              _ -> eval value
      [] -> return NIL
      err -> throwError $ "Syntax error: Expected alist; got " ++ show err ++ " instead"

-- If special form
eval (If1 predicate conseq alt) =
    eval predicate >>= \case
        Bool True -> eval conseq
        Bool False -> eval alt
        NIL -> eval alt
        err -> throwError $ "Expected boolean; got `" ++ show err ++ "` instead"

eval (If2 predicate conseq) =
    eval predicate >>= \case
        Bool True -> eval conseq
        _ -> throwError "Unspecified return value"

-- Set special form
--
--`set!` can change any existing binding, but not introduce a new one
eval (Set var val) = do
    void $ eval $ Symbol var
    eval val >>= \result -> modify $ \env -> (var, result) : env
    return NIL

-- Define special form, simple case
eval (Define1 var expr) = do
    result <- eval expr
    modify $ \env -> (var, result) : env
    return result

-- Procedure definitions
eval (Define2 name args body) = do
    env <- get
    case duplicates args of
        [] -> do
            put env'
            return fn
          where
            fn = Procedure env' args body
            env' = (name, fn) : env

        x -> throwError $ "Duplicate argument " ++ show x ++ " in function definition"

-- Lambda definition
eval (Lambda args body) = do
    env <- get
    case duplicates args of
      [] -> return $ Procedure env args body
      x -> throwError $ "Duplicate argument " ++ show x ++ " in function definition"

-- Procedure application with name
eval (List (Symbol func : args)) = do
    env <- get
    case lookup func env of
        Just fn -> apply fn args
        Nothing -> apply (Symbol func) args

-- Inline function invocation
eval (List (function : args)) = eval function >>= \fn -> apply fn args

eval lv = throwError $ "Unknown value; " ++ show lv

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
apply :: Scheme -> [Scheme] -> Result Scheme
apply (Procedure closure formal body) args = do
    local <- zipWithM zipper formal args
    withStateT (\env -> closure ++ local ++ env) $ progn body

apply (Symbol func) args =
    case lookup func primitives of
        Just primitive ->
            mapM eval args >>= \args' -> return $ primitive args'
        Nothing ->
            throwError $ "Undefined primitive function " ++ show func

apply fn _args = throwError $ "Procedure Application Error. Fn: " ++ show fn

-- | Evaluate a list of expressions sequentially; and return the result of last
--
-- Progn needs to stop at the first throwErrorure and hence the intermediatary results
-- are forced with a `seq`. I'm not sure if this is the right way to do things,
-- but works for now.
progn :: [Scheme] -> Result Scheme
progn [] = return NIL
progn [x] = eval x
progn (x:xs) = eval x >>= \lv -> seq lv $ progn xs

-- | Return duplicate items in the list
--
-- >>> duplicates [1, 2, 3, 4, 1]
-- [1]
duplicates :: [Scheme] -> [Scheme]
duplicates xs = xs \\ nub xs

-- | Transforms a let args tuple list to env
--
-- `(let ((a 1) (b (+ 1 1))) (+ a b))` -> `[(a, 1), (b, 2)]`

alistToEnv :: Scheme -> Result Env
alistToEnv (List xs) = mapM trans xs
  where
    -- | Transform an alist of the form `(a (+ 1 1))` to a `(a 1)` expression
    -- [TODO] - trans and zipper look a bit too similar; refactor into one
    trans :: Scheme -> Result (String, Scheme)
    trans (List[Symbol var, val]) = eval val >>= \result -> return (var, result)
    trans _ = throwError "Malformed alist passed to let"

alistToEnv _ = throwError "Second argument to let should be an alist"

-- We are strict! Zipper evaluates arguments before passing to functions
zipper :: Scheme -> Scheme -> Result (String, Scheme)
zipper (Symbol var) val = eval val >>= \result -> return (var, result)
zipper a b = throwError $ "Malformed function arguments" ++ show a  ++ " " ++ show b

-- Exposed API

-- | Evaluate a string and return result
--
-- This method is stateless and subsequent applications wont behave like a repl.
exec :: String -> Either String Scheme
exec str = fst $ run [] str

-- | Evaluate a string and return result, along with new env
--
-- This method is stateless and subsequent applications wont behave like a repl.
run :: Env -> String -> (Either String Scheme, Env)
run env str =
    case read str of
        Right lv ->
            case runIdentity $ runExceptT $ runStateT (progn lv) env of
              Right (result, env') -> (Right result, env')
              Left err -> (Left err, env)

        Left err -> (Left $ show err, env)
