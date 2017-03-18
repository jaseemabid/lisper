-----------------------------------------------------------------------------
-- |
-- Module      :  Lisper.Parser
--
-- The scheme parser
-- See http://www.scheme.com/tspl2d/grammar.html for formal grammar
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Parser (parser, read) where

import Data.Char (toLower)
import Prelude hiding (read)
import Text.ParserCombinators.Parsec
import qualified Prelude as P

import Lisper.Core
import Lisper.Token

-- | Parse comment
parseComment :: Parser Scheme
parseComment = string ";" >> manyTill anyChar (char '\n') >> return NIL

-- | Parse a quoted string
parseString :: Parser Scheme
parseString = String <$> p
  where
    p :: Parser String
    p = between dquote dquote (many (noneOf "\""))

    dquote = char '"'

-- | Parse a signed integer
parseNumber :: Parser Scheme
parseNumber = Number <$> p
  where
    p :: Parser Integer
    p = try $ do
        sign <- option ' ' (char '-')
        d <- try (P.read <$> many1 digit)
        return $ case sign of
                   '-' -> -1 * d
                   _ ->  d

-- | Parse any valid scheme identifier
--
-- Identifiers may denote variables, keywords, or symbols, depending upon
-- context. They are formed from sequences of letters, digits, and special
-- characters. With three exceptions, identifiers cannot begin with a character
-- that can also begin a number, i.e., they cannot begin with ., +, -, or a
-- digit. The three exceptions are the identifiers ..., +, and -. Case is
-- insignificant in symbols so that, for example, newspaper, NewsPaper, and
-- NEWSPAPER all represent the same identifier.
parseSymbol :: Parser Scheme
parseSymbol = Symbol <$> identifier
  where
    symbol :: Parser Char
    symbol = oneOf "!$%&*/:<=>?~_^"

    initial :: Parser Char
    initial = letter <|> symbol

    subsequent :: Parser Char
    subsequent = initial <|> digit <|> oneOf ".+-"

    identifier :: Parser String
    identifier = try (string "+" <* notFollowedBy alphaNum)
        <|> try (string "-" <* notFollowedBy alphaNum)
        <|> try (string "...")
        <|> do
            i <- initial
            s <- many subsequent
            return $ map toLower $ i : s

-- | Parse scheme boolean
parseBool :: Parser Scheme
parseBool = Bool <$> p
  where
    -- Try is required on the left side of <|> to prevent eagerly consuming #
    p :: Parser Bool
    p = (== "#t") <$> (try (string "#t") <|> string "#f")

-- | Parse a list
parseList :: Parser Scheme
parseList = List <$> sepEndBy parseExpr spaces

-- | Parse dotted list
parsePair :: Parser Scheme
parsePair = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ Pair h t

-- | parse quoted expression
parseQuoted :: Parser Scheme
parseQuoted = do
    x <- char '\'' *> parseExpr
    return $ List [Quote, x]

-- | The lisp grammer
parseExpr :: Parser Scheme
parseExpr = parseComment
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

parser :: Parser [Scheme]
parser = many1 parseExpr

read :: String -> Either ParseError [Scheme]
read "" = Right []
read input = parse parser "" input
