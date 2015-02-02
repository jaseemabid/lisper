{-# LANGUAGE OverloadedStrings #-}

module Main where

import Eval (exec)
import Repl (runRepl)

import System.Environment (getArgs)
import System.Exit (exitSuccess)

-- Main
main :: IO ()
main = getArgs >>= parseArgs >>= putStr

parseArgs :: [String] -> IO a
parseArgs ["-c", sexp] = exec sexp >> exitSuccess
parseArgs ["-h"] = usage >> exitSuccess
parseArgs ["-v"] = version >> exitSuccess
parseArgs [] = runRepl >> exitSuccess
parseArgs [file] = readFile file >>= exec >> exitSuccess
parseArgs _ = usage >> exitSuccess

usage :: IO ()
usage = putStrLn "Usage: lisper [-vh] [-c expr] [file] "

version :: IO ()
version = putStrLn "The Glorious lisp, version 0.1.0.0"
