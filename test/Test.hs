module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lisper.Core
import           Lisper.Eval

env :: Env
env = [("one", Number 1), ("ref", Atom "one"), ("t", Atom "two")]

-- Evaluate string in empty env and return value
run = snd . exec []

-- Resolve tests
resSimple :: TestTree
resSimple = testCase "Should get variables" $
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

-- Primitives
eq :: TestTree
eq = testCase "Should have eq, car and cdr" $ do
    run "(eq (car '(1 2 3)) 1)" @?= Bool True
    run "(eq (cdr '(1 2 3)) '(2 3))" @?= Bool True
    run "(eq 1 2)" @?= Bool False
    run "(eq 1 '(1))" @?= Bool False

cons :: TestTree
cons = testCase "Should make lists with cons" $
  run "(cons 1 '(2 3))" @?= List [Number 1, Number 2, Number 3]

-- Special forms evaluations
quote :: TestTree
quote = testCase "(quote a) should be equivalent to 'a " $ do
    run "(quote a)" @?= run "'a"
    run "(quote (1 2 3))" @?= run "'(1 2 3)"

let_ :: TestTree
let_ = testCase "Should evaluate let bindings" $ do
    run "(let ((a 12) (b 42)) (+ a b))" @?= Number 54
    run "(let ((a (car '(1 2 3 4)))) a)" @?= Number 1
    run "(let ((a '(1))) a)" @?= List [Number 1]

lambda :: TestTree
lambda = testCase "Should define lambda expressions" $
  case exec [] "(lambda (x) (+ 1 x))" of
      -- [todo] - Compare AST
      ([], _) -> return ()
      x -> assertString $ show x

lambdaExec :: TestTree
lambdaExec = testCase "Should apply lambda expressions" $
  run "((lambda (x) (+ 1 x)) 41)" @?= Number 42

define :: TestTree
define = testCase "Should define named functions" $
  case exec [] "(define (add x) (+ 1 x))" of
      ([("add", _)], _) -> return ()
      x -> assertString $ show x

defineExec :: TestTree
defineExec = testCase "Should apply named functions" $ do
    run "(define (add x) (+ 10 x)) (add 32)" @?= Number 42
    run "((define (const) 42) (const))" @?= Number 42

defineMulti :: TestTree
defineMulti = testCase "Body of define may have multiple expressions" $
  run "(define (add x) (set! ret (+ 1 x)) ret) (add 41)" @?= Number 42

fact :: TestTree
fact = testCase "Should do recursive functions" $
  run "(define (fact x) \
       \    (if (= x 0) \
       \      1 \
       \      (* x (fact (- x 1))))) \
       \ (fact 5)" @?= Number 120

curry' :: TestTree
curry' = testCase "Should do simple currying" $
  run "(define (curry fn x)        \
       \  (lambda (y) (fn x y)))         \
       \(define (add x y) (+ x y))  \
       \(let ((add4 (curry add 4))) \
       \  (add4 4))" @?= Number 8

-- [todo] - Fix failing merge sort test
merge :: TestTree
merge = testCase "Should do merge sort" $
  run "(define (even l)                                            \
      \  (if (null? l)                                             \
      \      '()                                                   \
      \    (if (null? (cdr l))                                     \
      \        '()                                                 \
      \      (cons (car (cdr l)) (even (cdr (cdr l)))))))          \
      \                                                            \
      \(define (odd l)                                             \
      \  (if (null? l)                                             \
      \      '()                                                   \
      \    (if (null? (cdr l))                                     \
      \        (list (car l))                                      \
      \      (cons (car l) (odd (cdr (cdr l)))))))                 \
      \                                                            \
      \(define (merge left right)                                  \
      \  (cond ((null? left) right)                                \
      \        ((null? right) left)                                \
      \        ((> (car left) (car right))                         \
      \         (cons (car right) (merge left (cdr right))))       \
      \        (else (cons (car left) (merge (cdr left) right))))) \
      \                                                            \
      \(define (merge-sort l)                                      \
      \  (if (null? l)                                             \
      \      l                                                     \
      \    (if (null? (cdr l))                                     \
      \        l                                                   \
      \      (merge (merge-sort (odd l))                           \
      \             (merge-sort (even l))))))                      \
      \                                                            \
      \(merge-sort '(9 1 6 8))" @?=
  List [Number 1, Number 6, Number 8, Number 9]

primitives :: TestTree
primitives = testGroup "Primitives" [eq, cons]

res :: TestTree
res = testGroup "Resolve" [resSimple, resRef, resFail, resBadRef]

special :: TestTree
special = testGroup "Special forms"
  [quote, let_, lambda, lambdaExec, define, defineExec, defineMulti, fact]

sample :: TestTree
sample = testGroup "Sample Programs" [curry', merge]

tests :: TestTree
tests = testGroup "Unit Tests" [primitives, res, special, sample]

main :: IO ()
main = defaultMain tests
