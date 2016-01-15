module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lisper.Core
import Lisper.Eval

env :: Env
env = [("one", Number 1), ("ref", Atom "one"), ("t", Atom "two")]

-- Resolve tests
resSimple :: TestTree
resSimple = testCase "Should get variables"  $
            resolve env "one" @?= Just (Number 1)

resRef :: TestTree
resRef = testCase "Should get references" $
         resolve env "ref" @?= Just (Number 1)

resFail :: TestTree
resFail = testCase "Should fail for missing variables" $
          resolve env "nope" @?= Nothing

resBadRef :: TestTree
resBadRef = testCase "Should fail for missing references" $
            resolve env "t" @?= Nothing

-- Special forms evaluations
lambda :: TestTree
lambda = testCase "Should define lambda expressions" $
         case run "(lambda (x) (+ 1 x))" of
             -- [todo] - Compare AST
             ([], _) -> return ()
             x -> assertString $ show x

lambdaExec :: TestTree
lambdaExec = testCase "Should apply lambda expressions" $
  exec "((lambda (x) (+ 1 x)) 41)" @?= Number 42

define :: TestTree
define = testCase "Should define named functions" $
  case run "(define (add x) (+ 1 x))" of
      ([("add", _)], _) -> return ()
      x -> assertString $ show x

defineExec :: TestTree
defineExec = testCase "Should apply named functions" $ do
    exec "(define (add x) (+ 10 x)) (add 32)" @?= Number 42
    exec "((define (const) 42) (const))" @?= Number 42

fact :: TestTree
fact = testCase "Should do recursive functions" $
  exec "(define (fact x) \
       \    (if (= x 0) \
       \      1 \
       \      (* x (fact (- x 1))))) \
       \ (fact 5)" @?= Number 120

let_ :: TestTree
let_ = testCase "Should evaluate let bindings " $
       exec "(let ((a 12) (b 42)) (+ a b))" @?= Number 54

res :: TestTree
res = testGroup "Resolve" [resSimple, resRef, resFail, resBadRef]

special :: TestTree
special = testGroup "Special forms" [lambda, lambdaExec, define, defineExec,
                                     fact, let_]

tests :: TestTree
tests = testGroup "Unit Tests" [res, special]

main :: IO ()
main = defaultMain tests
