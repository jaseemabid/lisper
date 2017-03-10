{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

-- Core data strctures and functions on them
module Lisper.Core where

-- | [TODO] - Replace `LispVal` with `LispVal a`
data LispVal = Atom String
             | List [LispVal]
             | Function Env (Maybe String) [LispVal] [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

type Env = [(String, LispVal)]

instance Show LispVal where
    show (Atom x) = x
    show (List x) =
      case x of
          Quote : _ -> "'" ++ unwords' (tail x)
          _ -> "(" ++ unwords' x ++ ")"
    show (DottedList h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
    show (String s) = "\"" ++ s ++ "\""
    show (Number n) = show n
    show (Function _ (Just name) _ _) = "<λ " ++ name ++ " >"
    show (Function _ Nothing _ _) = "<λ>"
    show (Bool True) = "#t"
    show (Bool False) = "#f"

instance Eq LispVal where
    (==) (Atom a) (Atom b) = a == b
    (==) (List a) (List b) = a == b
    (==) (DottedList a b) (DottedList c d) = a == c && b == d
    (==) (String a) (String b) = a == b
    (==) (Number a) (Number b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) Function{} Function{} = False
    (==) _a _b = False

-- Patterns for pattern matching ;)
pattern NIL :: LispVal
pattern NIL = List []

-- Special forms

-- [TODO] - `define` supports only the 2 simple forms for now.
--
-- Handle expressions of the form `(define a 42)`
pattern Define1 :: String -> LispVal -> LispVal
pattern Define1 var expr = List [Atom "define", Atom var, expr]

-- Handle expressions of the form `(define (add a b) (+ a b))`
pattern Define2 :: String -> [LispVal] -> [LispVal] -> LispVal
pattern Define2 name args body =
    List (Atom "define" : List (Atom name : args) : body)

pattern If :: LispVal -> LispVal -> LispVal -> LispVal
pattern If predicate conseq alt = List [Atom "if", predicate, conseq, alt]

pattern Lambda :: [LispVal] -> [LispVal] -> LispVal
pattern Lambda args body = List (Atom "lambda" : List args: body)

pattern Let :: LispVal -> [LispVal] -> LispVal
pattern Let args body = List (Atom "let" : args : body)

pattern Cond :: [LispVal] -> LispVal
pattern Cond body = List (Atom "cond": body)

pattern Quote :: LispVal
pattern Quote = Atom "quote"

pattern Set :: String -> LispVal -> LispVal
pattern Set var val = List [Atom "set!", Atom var, val]

-- Helpers
unwords' :: [LispVal] -> String
unwords' = unwords . map show
