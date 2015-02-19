{-# LANGUAGE OverloadedStrings #-}

module Main where

import Eval (exec)
import Repl (runRepl)

import System.Environment (getArgs)

-- Main
main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-c", sexp] = exec sexp
parseArgs ["-h"] = usage
parseArgs ["-v"] = version
parseArgs [] = runRepl
parseArgs [file] = readFile file >>= exec
parseArgs _ = usage

usage :: IO ()
usage = putStrLn "Usage: lisper [-vh] [-c expr] [file] "

version :: IO ()
version = putStrLn "The Glorious lisp, version 0.1.0.0"
