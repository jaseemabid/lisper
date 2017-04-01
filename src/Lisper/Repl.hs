{-# LANGUAGE LambdaCase        #-}

module Lisper.Repl (runRepl) where

import Prelude hiding (read)

import Lisper.Compiler (Env, read, evaluate)

import Control.Monad.State.Lazy
import System.Console.Haskeline
import System.Directory (getHomeDirectory)

-- [todo] - Improve input from stdin, Ie echo "(+ 1 1)" | lisper

runRepl :: IO ()
runRepl = do
    f <- (++ "/.lisper_history") <$> getHomeDirectory
    let settings = defaultSettings { historyFile = Just f}
    runInputT settings (evalStateT loop [])

  where
    loop :: MonadException m => StateT Env (InputT m) ()
    loop =
        lift (getInputLine "λ ") >>= \case
            Nothing  -> return ()
            Just "q" -> return ()
            Just "" -> loop
            Just line -> do
                case read line of
                    Right ast ->
                      case evaluate ast of
                          (Right (result, env')) -> do
                              put env'
                              lift $ outputStrLn (show result)

                          (Left err) ->
                              lift $ outputStrLn err

                    Left err -> lift $ outputStrLn err

                loop
