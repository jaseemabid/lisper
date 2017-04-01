-- Core data strctures and functions on them
module Lisper.Core where

-- | [TODO] - Replace `Scheme` with `Scheme a`
data Scheme =
    Bool Bool
    | Char
    | List [Scheme]
    | Number Integer
    | Pair [Scheme] Scheme
    | Port
    | Procedure Env Scheme [Scheme]
    | String String
    | Symbol String
    | Vector
   deriving (Eq)

type Env = [(String, Scheme)]

instance Show Scheme where
    show (Symbol x) = x
    show (List x) =
      case x of
          Symbol "quote" : _ -> "'" ++ unwords' (tail x)
          _ -> "(" ++ unwords' x ++ ")"
    show (Pair h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
    show (String s) = "\"" ++ s ++ "\""
    show (Number n) = show n
    -- [TODO] - Make show instance for functions more descriptive in test builds
    show Procedure{} = "<λ>"
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show _ = "Undefined type"

-- Helpers
unwords' :: [Scheme] -> String
unwords' = unwords . map show
