{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- Core data strctures and functions on them
module Core where

data LispVal = Atom String
             | List [LispVal]
             | Function Env String [LispVal] LispVal
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
  show (Function _ name _ _) = " < λ " ++ name ++ " > "
  show (Bool True) = "#t"
  show (Bool False) = "#f"

instance Eq LispVal where
    (==) (Atom a) (Atom b) = a == b
    (==) (List a) (List b) = a == b
    (==) (DottedList a b) (DottedList c d) = a == c && b == d
    (==) (String a) (String b) = a == b
    (==) (Number a) (Number b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) (Function _ _ _ _) (Function _ _ _ _) =
        error "Cannot compare functions"
    (==) _ _ = error "Cannot compare arbitrary types"


-- Patterns for pattern matching ;)
pattern NIL = List []

-- Special forms
pattern If predicate conseq alt = List [Atom "if", predicate, conseq, alt]
pattern Defun name args body = List [Atom "defun", Atom name, List args, body]

pattern Let args body = List [Atom "let", args, body]
pattern Quote = Atom "quote"
pattern Set var val = List [Atom "set!", (Atom var), val]

-- Helpers
unwords' :: [LispVal] -> String
unwords' = unwords . map show
