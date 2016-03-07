module Lisper.Repl (runRepl) where

import           Lisper.Core              (Env)
import           Lisper.Eval              (exec)
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
    loop env = do
        input <- getInputLine "λ "
        case input of
            Nothing  -> return ()
            Just "q" -> return ()
            Just line -> do
                let (env', result) = exec env line
                outputStrLn $ show result
                loop env'
