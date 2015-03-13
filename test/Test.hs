module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lisper
import Lisper.Core
import Lisper.Eval

env :: Env
env = [("one", Number 1), ("ref", Atom "one"), ("t", Atom "two")]

testSimple = testCase "Should get variables"  $
             resolve env "one" @?= Just (Number 1)

testFail = testCase "Should fail for missing variables" $
           resolve env "nope" @?= Nothing

testRef = testCase "Should get references" $
          resolve env "ref" @?= Just (Number 1)

-- Should this be `Atom "two"` ?
testBadRef = testCase "Should fail for missing references" $
             resolve env "t" @?= Nothing

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testSimple, testFail, testRef, testBadRef]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
