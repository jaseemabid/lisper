{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}


-- Core data structures and functions on them
module Lisper.Core where



data Scheme a where
    Bool :: Bool -> Scheme Bool
    Char :: Char -> Scheme a
    List :: [Scheme a] -> Scheme [a]
    Number :: (Show  a, Num a, Eq a) => a -> Scheme a
    Pair :: [Scheme ()] -> Scheme a -> Scheme ()
    Port :: Scheme a
    Procedure :: Scheme a -> Env a -> Scheme a -> [Scheme a] -> Scheme ()
    String :: String -> Scheme String
    Symbol :: String -> Scheme String
    Vector :: a -> Scheme a

data Env a = Env [(String, Scheme a)]
  deriving (Eq)

nil :: Scheme [a]
nil = List []

instance Eq (Scheme a) where
    Bool a == Bool b = a == b
    Char a == Char b = a == b
    List a == List b = a == b
    Number a == Number b = a == b
    Procedure{} == Procedure{} = False
    String a == String b = a == b
    Symbol a == Symbol b = a == b
    _ == _ = False

instance Show (Scheme a) where
    show (Symbol x) = x
    show (List (Symbol "quote" : xs)) = "'" ++ unwords' xs
    show (List x) = "(" ++ unwords' x ++ ")"
    show (Pair h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
    show (String s) = "\"" ++ s ++ "\""
    show (Number n) = show n
    -- [TODO] - Make show instance for functions more descriptive in test builds
    show Procedure{} = "<λ>"
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show _ = "Undefined type"

instance Functor (Scheme) where
    fmap f (Bool b) = Bool $ f b

-- Helpers
unwords' :: [Scheme a] -> String
unwords' = unwords . map show
