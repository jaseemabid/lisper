---
--- Tokens with special meanings
---

{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Token where

import Lisper.Core

-- § 4.1.1; Variable references
pattern NIL :: Scheme
pattern NIL = List []

-- § 4.1.2; Literal Expressions
pattern Quote :: Scheme
pattern Quote = Symbol "quote"

-- § 4.1.4; Procedures
pattern Lambda :: Scheme
pattern Lambda = Symbol "lambda"

-- § 4.1.5; Conditionals
pattern If :: Scheme
pattern If = Symbol "if"

-- § 4.1.6; Assignments
pattern Set :: Scheme
pattern Set = Symbol "set!"

pattern Define :: Scheme
pattern Define = Symbol "define"

-- Convenient aliases

pattern Plus :: Scheme
pattern Plus = Symbol "+"

pattern Minus :: Scheme
pattern Minus = Symbol "-"

pattern Ellipses :: Scheme
pattern Ellipses = Symbol "..."

pattern Yes :: Scheme
pattern Yes = Bool True

pattern No :: Scheme
pattern No = Bool False

-- [TODO] - Remove when let and cond are redefined as macros
pattern Let :: Scheme
pattern Let = Symbol "let"

pattern Cond :: Scheme
pattern Cond = Symbol "cond"
