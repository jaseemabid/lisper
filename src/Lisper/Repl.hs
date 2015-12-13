module Lisper.Repl (runRepl) where

import Lisper.Eval (exec)

import System.IO (hFlush, stdout)
import Control.Monad (unless)

-- REPL helpers
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  input <- prompt
  unless (predicate input) $ action input >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "/q") (readPrompt "λ> ") $ print . exec
