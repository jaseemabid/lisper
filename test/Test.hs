module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lisper
import Lisper.Core
import Lisper.Eval

env :: Env
env = [("one", Number 1)]

testSimple = testCase "Should get simple references"  $
             resolve env "one" @?= Number 1

testFail = testCase "Should fail for missing references" $
           resolve env "t" @?= Number 1

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testSimple, testFail]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
