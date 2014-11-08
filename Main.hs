import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Helpers
unwords' :: [LispVal] -> String
unwords' = unwords . map show

instance Show LispVal where
    show (Atom x) = x
    show (List x) =
      case x of
       (Atom "quote"):_ -> "'" ++ unwords' (tail x)
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
           _ <- many (spaces)
           x <- try parseList <|> parseDottedList
           _ <- many (spaces)
           _ <- char ')'
           return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right x -> x

-- Evaluator
-- Primitives, implemented in terms of haskell
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("quote", quote),
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quot", numericBinop quot),
              ("rem", numericBinop rem)]

-- Helpers for the evaluator
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (error err) ($ args) $ lookup func primitives where
  err = "Undefined function " ++ show func

-- `numericBinop` takes a primitive Haskell function and wraps it with code to
-- unpack an argument list, apply the function to it, and wrap the result up in
-- our Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params where
  unpackNum :: LispVal -> Integer
  unpackNum (Number n) = n
  unpackNum (String _) = error err where
    err = "Who am I? JavaScript to type cast Strings to Numbers?"
  unpackNum (Atom _) = error "Cannot type cast Atom to Number"
  unpackNum (List _) = error "Cannot type cast List to Number"
  unpackNum (Bool _) = error "Cannot type cast Boolean to Number"
  unpackNum (DottedList _ _) = error "Cannot type cast DottedList to Number"

-- Verify that quote takes only one argument
quote lv = head lv

-- Evaluation rules
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args -- Is this lazy??

-- REPL helpers
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString = eval . readExpr

evalAndPrint :: String -> IO ()
evalAndPrint expr =  (return $ show $ evalString expr) >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   input <- prompt
   if pred input
     then return ()
     else action input >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "q") (readPrompt "λ> ") evalAndPrint

-- Main
main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              otherwise -> putStrLn "Program takes only 0 or 1 argument"
