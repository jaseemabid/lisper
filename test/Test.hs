module Main where

import Test.HUnit

import Lisper
import Lisper.Core
import Lisper.Eval

env = [("one", Number 1)]

testSimple = TestCase $ assertEqual
  "Should get simple references" (Number 1) (resolve env "one")

testFail = TestCase $ assertEqual
  "Should get simple references" (Number 42) (resolve env "one")

-- Main
main :: IO Counts
main = runTestTT $ TestList [testSimple, testFail]
