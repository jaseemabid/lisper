{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}


-- Core data structures and functions on them
module Lisper.Core where


data Scheme a where
    Bool :: Bool -> Scheme Bool
    Char :: Char -> Scheme Char
    List :: [Scheme a] -> Scheme [a]
    Number :: (Show a, Num a) => a -> Scheme a
    Pair :: [Scheme ()] -> Scheme a -> Scheme ()
    Port :: Scheme a
    Procedure :: Scheme a -> Env -> Scheme a -> [Scheme a] -> Scheme ()
    String :: String -> Scheme String
    Symbol :: String -> Scheme String
    Vector :: a -> Scheme a

data Env = forall a. Env [(String, Scheme a)]

nil :: Scheme [a]
nil = List []

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

-- Helpers
unwords' :: [Scheme a] -> String
unwords' = unwords . map show
