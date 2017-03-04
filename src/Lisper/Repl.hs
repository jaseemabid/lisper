{-# LANGUAGE LambdaCase        #-}

module Lisper.Repl (runRepl) where

import           Control.Monad.State
import           Lisper.Core              (Env)
import           Lisper.Parser
import           Lisper.Eval

import           System.Console.Haskeline
import           System.Directory         (getHomeDirectory)

-- [todo] - REPL must be stateful. *HIGH PRIORITY*
-- [todo] - Improve input from stdin, Ie echo "(+ 1 1)" | lisper
-- [todo] - If possible handle empty input from user with a no-op

runRepl :: IO ()
runRepl = do
    f <- fmap (++ "/.lisper_history") getHomeDirectory
    let settings = defaultSettings { historyFile = Just f}
    runInputT settings $ loop []
  where
    loop :: MonadException m => Env -> InputT m ()
    loop env =
        getInputLine "λ " >>= \case
            Nothing  -> return ()
            Just "q" -> return ()
            Just line -> do
                -- [TODO] - Move this entire computation into a State Monad
                -- [TODO] - Pretty print avoiding `Left` and `Right`
                let (result, env') = runState (progn $ readExpr line) env
                outputStrLn $ show result
                loop env'
