-----------------------------------------------------------------------------
-- |
-- Module      :  Lisper.Primitives
--
-- Scheme primitives implemented in Haskell
--
-- This module has several issues and needs a complete rewrite.
--
-- 1. Find a good architecture for native functions.
-- 2. Cannot use any of these as higher order functions.
-- 3. Remove `error` from everywhere
-- 4. Add a prelude file which can have pure lisp definitions
-----------------------------------------------------------------------------

module Lisper.Primitives (primitives) where

import Lisper.Core

-- Primitives, implemented in terms of haskell
primitives :: [(String, [Scheme] -> Scheme)]
primitives = [("eq", eq),
              ("null?", nullq),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("length", length'),
              ("list", list'),
              ("*", numericBinop (*)),
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("/", numericBinop div),
              ("/=", numBoolBinop (/=)),
              ("<", numBoolBinop (<)),
              ("<=", numBoolBinop (<=)),
              ("=", numBoolBinop (==)),
              (">", numBoolBinop (>)),
              (">=", numBoolBinop (>=)),
              -- [verify] Get quotient from stdlib
              ("quotient", numericBinop mod),
              ("quot", numericBinop quot),
              ("quote", head),
              ("rem", numericBinop rem)]

eq :: [Scheme] -> Scheme
eq [a, b] = Bool $ a == b
eq x = error $ "eq expected 2 arguments" ++ show x

nullq :: [Scheme] -> Scheme
nullq [a] = Bool $ a == List []
nullq x = error $ "null? expected 2 arguments" ++ show x

car :: [Scheme] -> Scheme
car [List (t : _)] = t
car x = error $ "car expected a single list, got " ++ show x

cdr :: [Scheme] -> Scheme
cdr [List (_ : t)] = List t
cdr x = error $ "cdr expected a single list, got " ++ show x

cons :: [Scheme] -> Scheme
cons (h : [List t]) = List (h:t)
cons x = error $ "cons expected a value and a list, got " ++ show x

length' :: [Scheme] -> Scheme
length' [List x] = Number $ toInteger $ length x
length' x = error $ "length expected a single list, got " ++ show x

list' :: [Scheme] -> Scheme
list' = List

-- [TODO] - Add input type to error message
-- [TODO] - Possibly auto generate unpack*
unpackNum :: Scheme -> Integer
unpackNum (Number n) = n
unpackNum x = error $ "Expected number; got " ++ show x ++ " instead"

-- `numericBinop` takes a primitive Haskell function and wraps it with code to
-- unpack an argument list, apply the function to it, and wrap the result up in
-- Scheme Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [Scheme] -> Scheme
numericBinop op params = Number $ foldl1 op $ map unpackNum params

numBoolBinop :: (Integer -> Integer -> Bool) -> [Scheme] -> Scheme
numBoolBinop op [Number one, Number two] = Bool (one `op` two)
numBoolBinop _ _  = error "Unexpected arguments to numeric binary operator"
