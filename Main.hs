{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

import Debug.Trace (trace)

debug = flip trace

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

nil :: LispVal
nil = List []

type Env = [(String, LispVal)]

getVar :: String -> Env -> LispVal
getVar key env = case lookup key env of
                  Just v -> eval env v
                  _ -> error $ "Undefined variable " ++ key

-- Default environment to start with
env' :: Env
env' = [("ZERO", Number 0),
       ("LIFE", Number 42),
       ("VERSION", String "lisper 0.1")]

-- Helpers to retrieve haskell values from LispVal

-- [todo] Add input type to error message
-- [todo] Possibly auto generate unpack*
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ =  error "Unable to convert to number"

-- Helpers
unwords' :: [LispVal] -> String
unwords' = unwords . map show

instance Show LispVal where
  show (Atom x) = x
  show (List x) =
    case x of
     Atom "quote" : _ -> "'" ++ unwords' (tail x)
     _ -> "(" ++ unwords' x ++ ")"
  show (DottedList h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
  show (String s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = do
  d <- many1 digit
  return $ (Number . read) d

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepEndBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do
           _ <- char '('
           _ <- many spaces
           x <- try parseList <|> parseDottedList
           _ <- many spaces
           _ <- char ')'
           return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right x -> x

-- Evaluator
-- Primitives, implemented in terms of haskell
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("*", numericBinop (*)),
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

-- Helpers for the evaluator
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (error err) ($ args) $ lookup func primitives where
  err = "Undefined function " ++ show func

-- `numericBinop` takes a primitive Haskell function and wraps it with code to
-- unpack an argument list, apply the function to it, and wrap the result up in
-- LispVal Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numBoolBinop op [Number one, Number two] = Bool (one `op` two)
numBoolBinop _ _  = error "Unexpected arguments to numeric binary operator"

-- Evaluation rules
eval :: Env -> LispVal -> LispVal

eval _ val@(String _) = val
eval _ val@(Number _) = val
eval _ val@(Bool _) = val

eval _ (List []) = List []
eval _ (List [Atom "quote", val]) = val

eval env (Atom key) = getVar key env

eval env (List [Atom "let", args, body]) = eval' env args
    where
      makeEnv item env'' =
          case item of
            List[Atom a, val] -> (a, val) : env''

      eval' env'' args' =
          case args' of
            List[v] -> eval (makeEnv v env'') body
            List(v:rest) -> eval' (makeEnv v env'') (List(rest))
            _ -> error "Second argument to let should be an alist"

eval env (List [Atom "if", predicate, conseq, alt]) =
  let result = eval env predicate
  in case result of
      Bool True -> eval env conseq
      Bool False -> eval env alt
      _  -> error "If needs a Boolean predicate"

eval env (List (Atom func : args)) = apply func $ map (eval env) args -- Is this lazy??

-- REPL helpers
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint = print . eval env' . readExpr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  input <- prompt
  unless (predicate input) $ action input >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "q") (readPrompt "λ> ") evalAndPrint

-- Main
main :: IO ()
main = do args <- getArgs
          case length args of
           0 -> runRepl
           1 -> evalAndPrint $ head args
           _ -> putStrLn "Program takes only 0 or 1 argument"
