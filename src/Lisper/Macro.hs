{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Macro where

import Prelude hiding (read)

-- import Debug.Trace

import Lisper.Core

-- | Identifier is an optional value literally matched in a macro
--
-- For example, the following macro contains one identifier, `=>`
--
-- (define-syntax bind
--   (syntax-rules (=>)
--     ((bind a => b) (b a))))
--
type Identifier = Scheme

-- | A pattern is a list that begins with the keyword for the macro.
--
-- A pattern is an identifier, a constant, or one of the following.
--
-- (pattern ...)
-- (pattern pattern ... . pattern)
--
-- For example, the following macro contains one predicate, `(bind a => b)`
--
-- (define-syntax bind
--   (syntax-rules (=>)
--     ((bind a => b) (b a))))
--
type Pattern = Scheme

-- | Rewrite rule is the result of a `Pattern` match
--
-- For example, the following macro contains one predicate, `(b a)`
--
-- (define-syntax bind
--   (syntax-rules (=>)
--     ((bind a => b) (b a))))
--
type Template = Scheme

-- | A rule is a predicate and a template to rewrite to when matched
data Rule = Rule Pattern Template
  deriving (Eq, Show)

-- | A macro object is a set of `Identifier`s and rewrite `Rules`s
data Macro = Macro [Identifier] [Rule]
  deriving (Eq, Show)

-- | Build a Macro object from AST
--
--
build :: Scheme -> Macro
build (List (Symbol "syntax-rules": List identifiers: rules')) =
    Macro identifiers rules
  where
    rules = map alistToRule rules'

    alistToRule (List [a, b]) = Rule a b
    alistToRule _ = error "Unknown macro rule"

build _ = undefined

-- [TODO] - Handle identifiers in macros, this is too naive

-- | Expand a macro object to AST
--
-- Consider the macro
--
-- (define-syntax bind
--   (syntax-rules (=>)
--     ((bind a => b) (b a))))
--
-- And its usage `(bind #t => not)`
--
-- This should get transformed into `(not #t)`
--
-- An evaluator will evaluate it into `#f` at run time.
expand :: Macro -> Scheme -> Scheme
expand (Macro _indentifiers []) expr =
    error $ "Ill-formed special form: " ++ show expr

expand (Macro identifiers (Rule predicate rewrite : rules)) expr =
  -- Did the predicate match the expression?
  if match identifiers predicate expr
  then rewrite
  -- Try other predicates otherwise
  else expand (Macro identifiers rules) expr

-- | Check if a predicate will match an expression
--
--
-- >>> match _ (bind a => b) (#t => not)
-- True
-- >>> match _ (bind a => b) (+ 1 1)
-- False
--
-- More formally, an input of the form `F` matches a pattern `P` if
-- and only if ...
match :: [Identifier] -> Pattern -> Scheme -> Bool
match ids predicate expr =

    case (literalp, predicate, expr) of

      -- P is a non literal identifier
      -- `a` in `(or a b)` will match `(or #t 42)`
      (False, Symbol _, _anything) -> True

      -- P is a literal identifier and F is an identifier with the same binding.
      -- `else` in (cond ...) or => needs an exact match.
      (True, Bool a, Bool b) -> a == b
      (True, Number a, Number b) -> a == b
      (True, Symbol a, Symbol b) -> a == b

      (_, List [], List []) -> True
      (_, List(x: xs), List(y: ys)) ->
          match ids x y && match ids (List xs) (List ys)

      (a, b, c) -> error $ "Unknown match format" ++ show (a, b, c)

  where
    literalp = predicate `elem` ids
