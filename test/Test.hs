module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lisper.Core
import Lisper.Eval

main :: IO ()
main = defaultMain tests

-- | Test a single test, super convenient in the repl
test :: TestTree -> IO ()
test a = defaultMain $ testGroup "Test" [a]

tests :: TestTree
tests = testGroup "Unit Tests" [parser
                               , references
                               , literal
                               , calls
                               , procedures
                               , conditionals
                               , assignments
                               , cond
                               , binding
                               , sample]

-- Parser tests

everything :: TestTree
everything = testCase "Understand all primitive types" $
    exec "'(hello 1 -4 \"YES\" 'ok a->b <=? () '() #t #f)" @?=
        Right (List [Symbol "hello"
                    , Number 1
                    , Number (-4)
                    , String "YES"
                    , List [Symbol "quote", Symbol "ok"]
                    , Symbol "a->b"
                    , Symbol "<=?"
                    , NIL
                    , List [Symbol "quote", NIL]
                    , Bool True
                    , Bool False
                    ])

comments :: TestTree
comments = testCase "Handle comments" $
    exec ";; 'hello \n (+ 1 1)" @?= Right (Number 2)

parser :: TestTree
parser = testGroup "Parser" [everything, comments]

-- § 4.1; Primitive expression types
-- § 4.1.1; Variable references

def :: TestTree
def = testCase "Simple define" $
    exec "(define x 28) x" @?= Right (Number 28)

res1 :: TestTree
res1 = testCase "Numeric reference" $
    exec "(define one 1) one" @?= Right (Number 1)

res2 :: TestTree
res2 = testCase "Symbolic reference" $
    exec "(define name 'j) name" @?= Right (Symbol "j")

res3 :: TestTree
res3 = testCase "Non native reference" $
    exec "(define l '(a b)" @?= Right (List [Symbol "a", Symbol "b"])

res4 :: TestTree
res4 = testCase "Fail for missing variables" $ do
    exec "nope" @?= Left "Undefined variable `nope`"
    exec "(+ 1 err)" @?= Left "Undefined variable `err`"

references :: TestTree
references = testGroup "Variable References" [def, res1, res2, res3, res4]

-- § 4.1.2; Literal Expressions

quote :: TestTree
quote = testCase "(quote a) should be equivalent to 'a" $ do
    exec "(quote a)" @?= exec "'a"
    exec "(quote (1 2 3))" @?= exec "'(1 2 3)"

literal :: TestTree
literal = testGroup "Literal Expressions" [
    quote,

    testCase "(quote a)" $
        exec "(quote a)" @?= Right (Symbol "a"),

    testCase "(quote '(a b c))" $
        exec "(quote '(a b c))" @?=
          Right (List [Symbol "quote", List [Symbol "a", Symbol "b", Symbol "c"]]),

    testCase "(quote '(+ 1 2))" $
        exec "(quote '(+ 1 2))" @?=
            Right (List [Symbol "quote", List [Symbol "+", Number 1, Number 2]])]

-- § 4.1.3; Procedure calls

eq :: TestTree
eq = testCase "Primitives; eq, car and cdr" $ do
    exec "(eq (car '(1 2 3)) 1)" @?= Right (Bool True)
    exec "(eq (cdr '(1 2 3)) '(2 3))" @?= Right (Bool True)
    exec "(eq 1 2)" @?= Right (Bool False)
    exec "(eq 1 '(1))" @?= Right (Bool False)

cons :: TestTree
cons = testCase "Make lists with cons" $
    exec "(cons 1 '(2 3))" @?= Right (List [Number 1, Number 2, Number 3])

calls :: TestTree
calls = testGroup "Procedure calls" [eq, cons]

-- § 4.1.4; Procedures
-- [TODO] - Add tests for alternate forms of lambda formals; see § 4.1.4

lambda :: TestTree
lambda = testCase "The obvious lambda expression" $
    -- Equality on functions doesn't make sense, but this is required here
    case exec "(define a 1)                                            \
             \ (lambda (x) (+ 1 x))" of
        Right (Procedure env args body) -> do
            env @?= [("a", Number 1)]
            args @?= [Symbol "x"]
            body @?= [List [Symbol "+", Number 1, Symbol "x"]]
        x -> assertString $ show x

iffe :: TestTree
iffe = testCase "Immediately exec defined lambda expression" $
    exec "((lambda (x) (+ x x)) 4)" @?= Right (Number 8)

add :: TestTree
add = testCase "Simple adder" $
    exec "(define add (lambda (x y) (+ x y)))                          \
        \ (add 10 20)" @?= Right (Number 30)

define :: TestTree
define = testCase "Define with let closure" $
    exec "(define add4                                                 \
        \   (let ((x 4))                                               \
        \     (lambda (y) (+ x y))))                                   \
        \ (add4 6) " @?= Right (Number 10)

factorial :: TestTree
factorial = testCase "Recursive factorial" $ do
    f <- readFile "scripts/fact.ss"
    exec f @?= Right (Number 120)

named :: TestTree
named = testCase "Apply named functions" $ do
    exec "(define (add x) (+ 10 x)) (add 32)" @?= Right (Number 42)
    exec "((define (const) 42) (const))" @?= Right (Number 42)

procedures :: TestTree
procedures = testGroup "Procedures" [lambda, iffe, add, define, factorial, named]

-- § 4.1.5; conditionals
-- [TODO] - See § 6.3.1 and verify what values scheme consider truthy

ifOk :: TestTree
ifOk = testCase "The obvious if" $ do
    exec "(if #t 1 2)" @?= Right (Number 1)
    exec "(if #f 1 2)" @?= Right (Number 2)
    exec "(let ((a 1)                                                   \
        \       (b 2))                                                  \
        \   (if (> a b) a b))" @?= Right (Number 2)

ifOne :: TestTree
ifOne = testCase "If should work with just one branch" $ do
  exec "(if #t 42)" @?= Right (Number 42)
  exec "(if #f 42)" @?= Left "Unspecified return value"

conditionals :: TestTree
conditionals = testGroup "Conditionals" [ifOk, ifOne]

--  § 4.1.6; Assignments
-- [TODO] - Its OK to set! at top level; I don't like that :/
set :: TestTree
set = testCase "Nuances of set" $ do
    exec "(define a #t) (set! a #f) a" @?= Right (Bool False)

    exec "(let ((a 1))                                                  \
        \   (set! a 2)                                                  \
        \   a)" @?= Right (Number 2)

    exec "(set! a #f) a" @?= Left "Undefined variable `a`"

assignments :: TestTree
assignments = testGroup "Assignments" [set]

-- § 4.2; Derived expression types
-- § 4.2.1; Conditionals (cond)

cond :: TestTree
cond = testGroup "Cond" []

-- § 4.2.2; Binding constructs

let_ :: TestTree
let_ = testCase "Evaluate let bindings" $ do
    exec "(let ((a 12) (b 42)) (+ a b))" @?= Right (Number 54)
    exec "(let ((a (car '(1 2 3 4)))) a)" @?= Right (Number 1)
    exec "(let ((a '(1))) a)" @?= Right (List [Number 1])

closure :: TestTree
closure = testCase "Evaluate let bindings with closures" $
    exec "(set! a 1) (let ((b 2)) (+ a b))" @?= Right (Number 3)

override :: TestTree
override = testCase "Let bindings should overrides closure" $
    exec "(set! a 1) (let ((a 2) (b 2)) (+ a b))" @?= Right (Number 4)

binding :: TestTree
binding = testGroup "Binding Constructs" [let_, closure, override]

-- Sample programs

curry' :: TestTree
curry' = testCase "Simple currying" $
  exec "(define (curry fn x) (lambda (y) (fn x y)))                    \
      \ (define (add x y) (+ x y))                                     \
      \ (let ((add4 (curry add 4))) (add4 4))" @?= Right (Number 8)

merge :: TestTree
merge = testCase "Simple merge sort" $
    readFile "scripts/merge.ss" >>= \file ->
        exec file @?= Right (List [Number 1, Number 6, Number 8, Number 9])

sample :: TestTree
sample = testGroup "Sample Programs" [curry', merge]
