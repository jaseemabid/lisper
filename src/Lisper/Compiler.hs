-----------------------------------------------------------------------------
-- |
-- Module      :  Lisper.Compiler
--
-- Public interface of lisper
-----------------------------------------------------------------------------

module Lisper.Compiler (Scheme, Env, read, evaluate, compile, exec)
  where

import Prelude hiding (read)

import Lisper.Core (Env, Scheme)
import Lisper.Eval (evaluate)
import Lisper.Macro (compile)
import Lisper.Parser (read)


-- | Compile and evaluate a string and return result
--
exec :: String -> Either String Scheme
exec str = do
    ast <- read str
    (source, _env) <- compile ast
    (val, _env) <- evaluate source
    return val
