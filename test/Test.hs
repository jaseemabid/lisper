module Main where

import Prelude hiding (read)

import Test.Tasty
import Test.Tasty.HUnit

import Lisper.Compiler (exec)
import Lisper.Core
import Lisper.Macro
import Lisper.Parser (read)
import Lisper.Token

main :: IO ()
main = defaultMain tests

-- | Test a single test, super convenient in the repl
test :: TestTree -> IO ()
test a = defaultMain $ testGroup "Test" [a]

-- | Make a single Scheme value out of string
lisp :: String -> Scheme
lisp source = case read source of
                  Right x -> head x
                  Left err -> error err

tests :: TestTree
tests = testGroup "Unit Tests"
    [parser, references, literal, calls, procedures, conditionals, assignments,
     cond, binding, sample] --, macros]

-- Parser tests

everything :: TestTree
everything = testCase "Understand all primitive types" $
    exec "'(hello + - ... 1 -4 \"YES\" 'ok a->b <=? () '() #t #f)" @?=
        Right (List [Symbol "hello"
                   , Plus
                   , Minus
                   , Ellipses
                   , Number 1
                   , Number (-4)
                   , String "YES"
                   , List [Quote, Symbol "ok"]
                   , Symbol "a->b"
                   , Symbol "<=?"
                   , NIL
                   , List [Quote, NIL]
                   , Yes
                   , No
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
    exec "(define l '(a b)) l" @?= Right (List [Symbol "a", Symbol "b"])

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
          Right (List [Quote, List [Symbol "a", Symbol "b", Symbol "c"]]),

    testCase "(quote '(+ 1 2))" $
        exec "(quote '(+ 1 2))" @?=
            Right (List [Quote, List [Plus, Number 1, Number 2]])]

-- § 4.1.3; Procedure calls

eq :: TestTree
eq = testCase "Primitives; eq, car and cdr" $ do
    exec "(eq (car '(1 2 3)) 1)" @?= Right Yes
    exec "(eq (cdr '(1 2 3)) '(2 3))" @?= Right Yes
    exec "(eq 1 2)" @?= Right No
    exec "(eq 1 '(1))" @?= Right No

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
            args @?= List [Symbol "x"]
            body @?= [List [Plus, Number 1, Symbol "x"]]
        x -> assertString $ show x

iffe :: TestTree
iffe = testCase "Immediately exec defined lambda expression" $
    exec "((lambda (x) (+ x x)) 4)" @?= Right (Number 8)

add :: TestTree
add = testCase "Simple adder" $
    exec "(define add (lambda (x y) (+ x y)))                          \
        \ (add 10 20)" @?= Right (Number 30)

variadic :: TestTree
variadic = testCase "Lambda with any number of args" $
    exec "((lambda x (car x)) 1 2 3)"@?= Right (Number 1)

arity :: TestTree
arity = testCase "Report arity mismatch" $
        exec "(define add (lambda (x y) (+ x y)))                      \
             \ (add 10 20 30)" @?= Left "Expected 2 arguments; got 3 instead"

define :: TestTree
define = testCase "Define with let closure" $
    exec "(define add4                                                 \
        \   (let ((x 4))                                               \
        \     (lambda (y) (+ x y))))                                   \
        \ (add4 6) " @?= Right (Number 10)

leak :: TestTree
leak = testCase "let should not leak closure outside" $
    exec "(let ((result (let ((a 1)                                    \
       \                      (b 2))                                   \
       \                   (+ a b)))))                                 \
       \  (+ result a b)" @?= Left "Undefined variable `result`"

factorial :: TestTree
factorial = testCase "Recursive factorial" $ do
    f <- readFile "scripts/fact.ss"
    exec f @?= Right (Number 120)

named :: TestTree
named = testCase "Apply named functions" $ do
    exec "(define (add x) (+ 10 x)) (add 32)" @?= Right (Number 42)
    exec "(define (const) 1) (const)" @?= Right (Number 1)

procedures :: TestTree
procedures = testGroup "Procedures"
    [lambda, iffe, add, arity, variadic, define, leak, factorial, named]

-- § 4.1.5; conditionals. Scheme considers everything other than `#f` truthy
ifOk :: TestTree
ifOk = testCase "The obvious if" $ do
    exec "(if #t 1 2)" @?= Right (Number 1)
    exec "(if #f 1 2)" @?= Right (Number 2)
    exec "(if '() 1 2)" @?= Right (Number 1)
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
    exec "(define a #t) (set! a #f) a" @?= Right No

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
    exec "(define a 1) (let ((b 2)) (+ a b))" @?= Right (Number 3)

override :: TestTree
override = testCase "Let bindings should overrides closure" $
    exec "(define a 1) (let ((a 2) (b 2)) (+ a b))" @?= Right (Number 4)

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

-- Macro tests

-- | `and` macro
andM :: Macro
andM = Macro "and" [] [rule1, rule2, rule3]
  where
    rule1 = Rule (lisp "(and)") (lisp "#t")
    rule2 = Rule (lisp "(and test)") (lisp "test")
    rule3 = Rule (lisp "(and t1 t2 ...)") (lisp "(if t1 (and t2 ...) #f)")

bindM :: Macro
bindM = Macro "bind" [Symbol "=>"] [rule]
  where
    rule = Rule (lisp "(bind a => b)") (lisp "(if a (b a) #f)")

-- Tests

buildAnd :: TestTree
buildAnd = testCase "Build (and a b)" $
    build (lisp source) @?= Right andM
  where
    source :: String
    source = "(syntax-rules ()                                         \
             \  ((and) #t)                                             \
             \  ((and test) test)                                      \
             \  ((and t1 t2 ...)                                       \
             \    (if t1 (and t2 ...) #f)))"

buildBind :: TestTree
buildBind = testCase "Build (bind a => f)" $
    build (lisp source) @?= Right bindM
  where
    source :: String
    source = "(syntax-rules (=>)                                       \
             \  ((bind a => b) (if a (b a) #f)))"

rewriteT :: TestTree
rewriteT = testCase "Generate rewrite maps" $ do

    rewrites [] rule1 (lisp "(x 1 2 3 4 5)") @?=
      [("a", Number 1), ("b", Number 2), ("c", lisp "(3 4 5)")]

    rewrites [] rule2 (lisp "(=> 1 2)") @?=
      [("a", Number 1), ("b", Number 2)]

  where
    rule1 = Rule (lisp "(x a b c ...)") undefined
    rule2 = Rule (lisp "(=> a b)") undefined

replaceT :: TestTree
replaceT = testCase "Simple variable replacement" $ do

    replace rewrite1 (lisp "(x a b '(p q r))") @?= lisp "(x 1 2 '(p q r))"

    -- [BUG] - Recursive expansion is broken
    replace rewrite1 (lisp "(x a b '(x a b))") @?= lisp "(x 1 2 '(x 1 2))"

    -- [BUG] - Splice is broken. `(1 2)` instead of `1 2`
    replace rewrite2 (lisp "(and a ...)") @?= lisp "(and 1 2)"

  where
    rewrite1 = [("a", Number 1), ("b", Number 2)]
    rewrite2 = [("a", List [Number 1, Number 2])]

expandBind :: TestTree
expandBind = testCase "Expand (bind a => f)" $
    expand bindM (lisp "(bind 1 => inc)") @?= lisp "(if 1 (inc 1) #f)"

expandAnd :: TestTree
expandAnd = testCase "Expand (and ...)" $ do
    expand andM (lisp "(and)") @?= lisp "#t"
    expand andM (lisp "(and #t)") @?= lisp "#t"
    expand andM (lisp "(and 1 2)") @?= lisp "(if 1 (if 2) #f)"

-- [TODO] - Get rid of unnecessary () after macro expansion
macros :: TestTree
macros = testGroup "Macros" [buildAnd, buildBind, expandBind, expandAnd]
