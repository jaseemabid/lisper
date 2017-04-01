-----------------------------------------------------------------------------
-- |
-- Module      :  Lisper.Parser
--
-- The scheme parser
-- See http://www.scheme.com/tspl2d/grammar.html for formal grammar
--
-- See http://unbui.lt/#!/post/haskell-parsec-basics for a great introduction to
-- Parsec and Monadic parsing
-----------------------------------------------------------------------------
module Lisper.Parser (parser, read) where

import Data.Char (toLower)
import Prelude hiding (read)
import qualified Prelude as P

import Text.Parsec

import Lisper.Core
import Lisper.Token

-- | Parse comment
parseComment :: Parsec String st Scheme
parseComment = string ";" >> manyTill anyChar newline >> return NIL

-- | Parse a quoted string
parseString :: Parsec String st Scheme
parseString = String <$> between dquote dquote (many (noneOf "\""))
  where
    dquote = char '"'

-- | Parse a signed integer
parseNumber :: Parsec String st Scheme
parseNumber = Number <$> p
  where
    p :: Parsec String st Integer
    p = try $ do
        sign <- option ' ' (char '-')
        d <- P.read <$> many1 digit
        return $ if sign == '-' then negate d else d

-- | Parse any valid scheme identifier
--
-- Identifiers may denote variables, keywords, or symbols, depending upon
-- context. They are formed from sequences of letters, digits, and special
-- characters. With three exceptions, identifiers cannot begin with a character
-- that can also begin a number, i.e., they cannot begin with ., +, -, or a
-- digit. The three exceptions are the identifiers ..., +, and -. Case is
-- insignificant in symbols so that, for example, newspaper, NewsPaper, and
-- NEWSPAPER all represent the same identifier.
parseSymbol :: Parsec String st Scheme
parseSymbol = Symbol <$> identifier
  where
    symbol :: Parsec String st Char
    symbol = oneOf "!$%&*/:<=>?~_^"

    initial :: Parsec String st Char
    initial = letter <|> symbol

    subsequent :: Parsec String st Char
    subsequent = initial <|> digit <|> oneOf ".+-"

    identifier :: Parsec String st String
    identifier =
            try (string "+" <* notFollowedBy alphaNum)
        <|> try (string "-" <* notFollowedBy alphaNum)
        <|> try (string "...")
        <|> do
            i <- initial
            s <- many subsequent
            return $ map toLower $ i : s

-- | Parse scheme boolean
--
-- Try is required on the left side of <|> to prevent eagerly consuming #
parseBool :: Parsec String st Scheme
parseBool = Bool . (== "#t") <$> (try (string "#t") <|> string "#f")

-- | Parse a list
parseList :: Parsec String st Scheme
parseList = List <$> sepEndBy parseExpr spaces

-- | Parse dotted list
parsePair :: Parsec String st Scheme
parsePair = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ Pair h t

-- | parse quoted expression
parseQuoted :: Parsec String st Scheme
parseQuoted = char '\'' *> parseExpr >>= \x -> return $ List [Quote, x]

-- | The lisp grammar
parseExpr :: Parsec String st Scheme
parseExpr =
      parseComment
  <|> parseBool
  <|> parseNumber
  <|> parseSymbol
  <|> parseQuoted
  <|> parseString
  <|> do
    spaces >> char '(' >> spaces
    x <- try parseList <|> parsePair
    spaces >> char ')' >> spaces
    return x

parser :: Parsec String st [Scheme]
parser = many1 parseExpr


-- | Parse source and return AST
--
-- Converting ParseError to String is losing information, but helps compose
-- elsewhere. See `Test.exec` for example. This is alright because I'm not doing
-- anything else with it right now.
read :: String -> Either String [Scheme]
read "" = Right []
read input =
    case parse parser "" input of
        Left err -> Left $ show err
        Right val -> Right val
