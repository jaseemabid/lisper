module Lisper.Repl (runRepl) where

import           Lisper.Eval              (exec)
import           System.Console.Haskeline

-- [todo] - REPL must be stateful. *HIGH PRIORITY*
-- [todo] - Improve input from stdin, Ie echo "(+ 1 1)" | lisper
-- [todo] - If possible handle empty input from user with a no-op

runRepl :: IO ()
runRepl = runInputT settings loop
  where
    loop :: MonadException m => InputT m ()
    loop = do
        input <- getInputLine "λ "
        case input of
            Nothing  -> return ()
            Just "q" -> return ()
            Just line -> (outputStrLn . show . exec $ line) >> loop

    -- [todo] - Move history file to ~/
    settings :: MonadException m => Settings m
    settings = defaultSettings { historyFile = Just ".lisper_history" }
