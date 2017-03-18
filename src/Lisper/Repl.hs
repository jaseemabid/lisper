{-# LANGUAGE LambdaCase        #-}

module Lisper.Repl (runRepl) where

import Lisper.Core (Env)
import Lisper.Eval

import System.Console.Haskeline
import System.Directory (getHomeDirectory)

-- [todo] - Improve input from stdin, Ie echo "(+ 1 1)" | lisper
-- [TODO] - Move the entire REPL into a State Monad

runRepl :: IO ()
runRepl = do
    f <- (++ "/.lisper_history") <$> getHomeDirectory
    let settings = defaultSettings { historyFile = Just f}
    runInputT settings $ loop []

  where
    loop :: MonadException m => Env -> InputT m ()
    loop env =
        getInputLine "λ " >>= \case
            Nothing  -> return ()
            Just "q" -> return ()
            Just "" -> loop env
            Just line ->
                case run env line of
                  (Right result, env') -> do
                      outputStrLn $ show result
                      loop env'
                  (Left err, _) -> do
                      outputStrLn err
                      loop env
