{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Monad
import Debug.Trace (trace)
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | Function Env String [LispVal] LispVal
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

type Env = [(String, LispVal)]

-- Patterns for pattern matching ;)
pattern NIL = List []

-- Special forms
pattern If predicate conseq alt = List [Atom "if", predicate, conseq, alt]
pattern Defun name args body = List [Atom "defun", Atom name, List args, body]

pattern Let args body = List [Atom "let", args, body]
pattern Quote = Atom "quote"
pattern Set var val = List [Atom "set!", (Atom var), val]

-- Debug helpers
debug :: c -> String -> c
debug = flip trace

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
     Quote : _ -> "'" ++ unwords' (tail x)
     _ -> "(" ++ unwords' x ++ ")"
  show (DottedList h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
  show (String s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (Function _ name _ _) = " < λ " ++ name ++ " > "
  show (Bool True) = "#t"
  show (Bool False) = "#f"

-- Parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
  return $ List [Quote, x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do
           _ <- spaces
           _ <- char '('
           _ <- spaces
           x <- try parseList <|> parseDottedList
           _ <- spaces
           _ <- char ')'
           _ <- spaces
           return x

parser :: Parser [LispVal]
parser = many1 parseExpr

readExpr :: String -> [LispVal]
readExpr input = case parse parser "exp" input of
                   Right x -> x
                   Left err -> error $ "Cannot parse expr : " ++ show err

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

-- Evaluate an expression and return the new environment and the result of the
-- evaluation. Env should mostly be unmodified unless the body is of the form of
-- a `defun` or a `set`. A `set` or `defun` should return an environment with a
-- new key, while a let expression should return the same env unmodified.

eval :: Env -> LispVal -> (Env, LispVal)

eval env NIL = (env, NIL)
eval env val@(String _) = (env, val)
eval env val@(Number _) = (env, val)
eval env val@(Bool _) = (env, val)

eval env (List [Quote, val]) = (env, val)

-- Variable lookup, forcing an evaluation
eval env (Atom key) = case lookup key env of
                        Just v -> eval env v
                        Nothing -> error $ "Undefined variable " ++ key

-- Let special form
eval env (Let args body) = eval' env args
    where
      makeEnv item env' =
          case item of
            List[Atom a, val] -> (a, val) : env'

      eval' env' args' =
          case args' of
            List[v] -> (env, snd $ eval (makeEnv v env') body)
            List(v:rest) -> eval' (makeEnv v env') (List(rest))
            _ -> error "Second argument to let should be an alist"

-- If special form
eval env (If predicate conseq alt) =
    let f NIL = eval env alt
        f (Bool True) = eval env conseq
        f (Bool False) = eval env alt
        f _ = error "If needs a Boolean predicate"
    in f $ snd $ eval env predicate

-- Set special form
eval env (Set var val) = ((var, val) : env, val)

-- Function definitions
eval env (Defun name args body) = ((name, fn) : env, fn) where
    fn = Function env name args body

-- Function application
eval env (List (Atom func : args)) =
    case lookup func env of
      Just(Function closure _ formal body) ->
          let
              args' = List $ zipWith (\x y -> List [x, y]) formal args
          in
            eval closure (Let args' body)
      Nothing -> (env, apply func $ map (snd . eval env) args)

-- Progn, evaluate a list of expressions sequentially
progn :: Env -> [LispVal] -> (Env, LispVal)
progn env [x] = eval env x
progn env (x:xs) = case eval env x of
                     (env', _) -> progn env' xs

-- Top level evaluator.
exec :: String -> IO ()
exec = print . snd . progn [] . readExpr

-- REPL helpers
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  input <- prompt
  unless (predicate input) $ action input >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "q") (readPrompt "λ> ") exec

-- Main
main :: IO ()
main = getArgs >>= parseArgs >>= putStr

parseArgs ["-c", sexp] = exec sexp >> exit
parseArgs ["-h"] = usage   >> exit
parseArgs ["-v"] = version >> exit
parseArgs [] = runRepl >> exit
parseArgs [file] = readFile file >>= exec >> exit
parseArgs _ = usage >> exit

die     = exitWith (ExitFailure 1)
exit    = exitWith ExitSuccess

usage   = putStrLn "Usage: lisper [-vh] [file ..]"
version = putStrLn "The Glorious lisp, version 0.1.0.0"
