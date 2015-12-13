module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lisper.Core
import Lisper.Eval

env :: Env
env = [("one", Number 1), ("ref", Atom "one"), ("t", Atom "two")]

testSimple :: TestTree
testSimple = testCase "Should get variables"  $
             resolve env "one" @?= Just (Number 1)

testFail :: TestTree
testFail = testCase "Should fail for missing variables" $
           resolve env "nope" @?= Nothing
testRef :: TestTree
testRef = testCase "Should get references" $
          resolve env "ref" @?= Just (Number 1)

-- Should this be `Atom "two"` ?
testBadRef :: TestTree
testBadRef = testCase "Should fail for missing references" $
             resolve env "t" @?= Nothing

lambdaEval :: TestTree
lambdaEval = testCase "Should evaluate lambda expresssions" $
             exec "((lambda (x) (+ 1 x)) 41)" @?= Number 42

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [testSimple, testFail, testRef, testBadRef, lambdaEval]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
