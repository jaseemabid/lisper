module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lisper.Core
import Lisper.Eval

env :: Env
env = [("one", Number 1),
       ("name", Atom "Gollum"),
       ("l", List [Number 1, Number 2])]

-- Resolve tests
res1 :: TestTree
res1 = testCase "Should get numeric variables" $
  resolve "one" env @?= Just (Number 1)

res2 :: TestTree
res2 = testCase "Should get atom variables" $
  resolve "name" env @?= Just (Atom "Gollum")

res3 :: TestTree
res3 = testCase "Should get non native variables" $
  resolve "l" env @?= Just (List [Number 1, Number 2])

res4 :: TestTree
res4 = testCase "Should fail for missing variables" $
          resolve "nope" env @?= Nothing

-- Primitives
eq :: TestTree
eq = testCase "Should have eq, car and cdr" $ do
    exec "(eq (car '(1 2 3)) 1)" @?= Right (Bool True)
    exec "(eq (cdr '(1 2 3)) '(2 3))" @?= Right (Bool True)
    exec "(eq 1 2)" @?= Right (Bool False)
    exec "(eq 1 '(1))" @?= Right (Bool False)

cons :: TestTree
cons = testCase "Should make lists with cons" $
  exec "(cons 1 '(2 3))" @?= Right (List [Number 1, Number 2, Number 3])

-- Special forms evaluations
quote :: TestTree
quote = testCase "(quote a) should be equivalent to 'a " $ do
    exec "(quote a)" @?= exec "'a"
    exec "(quote (1 2 3))" @?= exec "'(1 2 3)"

let_ :: TestTree
let_ = testCase "Should evaluate let bindings" $ do
    exec "(let ((a 12) (b 42)) (+ a b))" @?= Right (Number 54)
    exec "(let ((a (car '(1 2 3 4)))) a)" @?= Right (Number 1)
    exec "(let ((a '(1))) a)" @?= Right (List [Number 1])

closure :: TestTree
closure = testCase "Should evaluate let bindings with closures" $
    exec "(set! a 1) (let ((b 2)) (+ a b))" @?= Right (Number 3)

override :: TestTree
override = testCase "Let bindings should overrides closure" $
    exec "(set! a 1) (let ((a 2) (b 2)) (+ a b))" @?= Right (Number 4)

lambda :: TestTree
lambda = testCase "Should define lambda expressions" $
  case exec "(lambda (x) (+ 1 x))" of
      Right _lv -> return ()
      x -> assertString $ show x

defLambda :: TestTree
defLambda = testCase "Define should handle lambda expressions" $ do
  -- This seems to be a common style; not sure why
  f <- readFile "scripts/fact.ss"
  exec f @?= Right (Number 120)

lambdaExec :: TestTree
lambdaExec = testCase "Should apply lambda expressions" $
  exec "((lambda (x) (+ 1 x)) 41)" @?= Right (Number 42)

define :: TestTree
define = testCase "Should define named functions" $
  case run [] "(define (add x) (+ 1 x))" of
      (Right _, [("add", _)]) -> return ()
      x -> assertString $ show x

identifiers :: TestTree
identifiers = testCase "Named functions should support all valid identifiers" $
  case run [] "(define (int->bool x) (if (= x 0) #f #t)" of
      (Right _, [("add", _)]) -> return ()
      x -> assertString $ show x

comments :: TestTree
comments = testCase "Parser should handle comments" $
    exec ";; 'hello" @?= Right NIL

defineExec :: TestTree
defineExec = testCase "Should apply named functions" $ do
    exec "(define (add x) (+ 10 x)) (add 32)" @?= Right (Number 42)
    exec "((define (const) 42) (const))" @?= Right (Number 42)

defineMulti :: TestTree
defineMulti = testCase "Body of define may have multiple expressions" $
  exec "(define (add x) (set! ret (+ 1 x)) ret) (add 41)" @?= Right (Number 42)

fact :: TestTree
fact = testCase "Should do recursive functions" $
  exec "(define (fact x) \
       \    (if (= x 0) \
       \      1 \
       \      (* x (fact (- x 1))))) \
       \ (fact 5)" @?= Right (Number 120)

curry' :: TestTree
curry' = testCase "Should do simple currying" $
  exec "(define (curry fn x) (lambda (y) (fn x y)))                 \
       \(define (add x y) (+ x y))                                  \
       \(let ((add4 (curry add 4))) (add4 4))"
  @?= Right (Number 8)

-- [todo] - Fix failing merge sort test
merge :: TestTree
merge = testCase "Should do merge sort" $
  exec "(define (even l)                                            \
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
  Right (List [Number 1, Number 6, Number 8, Number 9])

primitives :: TestTree
primitives = testGroup "Primitives" [eq, cons]

res :: TestTree
res = testGroup "Resolve" [res1, res2, res3, res4]

special :: TestTree
special = testGroup "Special forms"
  [quote, let_, closure, override, lambda, lambdaExec, defLambda, define,
   identifiers, comments, defineExec, defineMulti, fact]

sample :: TestTree
sample = testGroup "Sample Programs" [curry', merge]

tests :: TestTree
tests = testGroup "Unit Tests" [primitives, res, special, sample]

-- | Test a single test, super convenient in the repl
test :: TestTree -> IO ()
test a = defaultMain $ testGroup "" [a]

main :: IO ()
main = defaultMain tests
