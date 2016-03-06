{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lisper.Eval (exec)
import Lisper.Repl (runRepl)

import System.Environment (getArgs)

-- Main
main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-c", sexp] = (print . exec) sexp
parseArgs ["-h"] = usage
parseArgs ["-v"] = version
parseArgs [file] = readFile file >>= print . exec
parseArgs _ = runRepl

usage :: IO ()
usage = putStrLn "Usage: lisper [-vh] [-c expr] [file] "

version :: IO ()
version = putStrLn "The Glorious lisp, version 0.1.0.0"
