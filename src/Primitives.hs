module Primitives (primitives) where
import Core

-- Primitives, implemented in terms of haskell
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("car", car),
              ("cdr", cdr),
              ("cons", cons),
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
              ("mod", numericBinop mod),
              ("quot", numericBinop quot),
              ("quote", head),
              ("rem", numericBinop rem)]

-- Lisp primitives
car :: [LispVal] -> LispVal
car [List (t : _)] = t
car x = error $ "car expected a single list, got " ++ show x

cdr :: [LispVal] -> LispVal
cdr [List (_ : t)] = List t
cdr x = error $ "cdr expected a single list, got " ++ show x

cons :: [LispVal] -> LispVal
cons (h : [List t]) = List (h:t)
cons x = error $ "cons expected a value and a list, got " ++ show x

-- [todo] Add input type to error message
-- [todo] Possibly auto generate unpack*
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ =  error "Unable to convert to number"

-- `numericBinop` takes a primitive Haskell function and wraps it with code to
-- unpack an argument list, apply the function to it, and wrap the result up in
-- LispVal Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numBoolBinop op [Number one, Number two] = Bool (one `op` two)
numBoolBinop _ _  = error "Unexpected arguments to numeric binary operator"
