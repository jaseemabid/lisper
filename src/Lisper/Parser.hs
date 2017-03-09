{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

-- Parser

module Lisper.Parser (parser, readExpr) where

import           Lisper.Core
import           Text.ParserCombinators.Parsec

-- See § 2.1 of R5RS for grammar and allowed characters
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

quote :: Parser Char
quote = char '\''

dquote :: Parser Char
dquote = char '"'

bool :: Parser Bool
bool = char '#' >> (char 'f' <|> char 't') >>= \b -> return $ b == 't'

str :: Parser String
str = between dquote dquote (many (noneOf "\""))

-- | Identifiers cannot begin with a digit
identifier :: Parser String
identifier = do
    a <- many1 (letter <|> symbol)
    b <- many (alphaNum <|> symbol)
    return (a ++ b)

-- Lisp types

parseString :: Parser LispVal
parseString = String <$> str

-- [fix] - Parse negative numbers. Sort test fails with neg numbers
parseNumber :: Parser LispVal
parseNumber = Number <$> (read <$> many1 digit)

-- | Identifiers cannot begin with a digit
parseAtom :: Parser LispVal
parseAtom = Atom <$> identifier

parseBool :: Parser LispVal
parseBool = Bool <$> bool

parseList :: Parser LispVal
parseList = List <$> sepEndBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- quote *> parseExpr
    return $ List [Quote, x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseBool
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
    spaces >> char '(' >> spaces
    x <- try parseList <|> parseDottedList
    spaces >> char ')' >> spaces
    return x

parser :: Parser [LispVal]
parser = many1 parseExpr

readExpr :: String -> Either ParseError [LispVal]
readExpr "" = Right []
readExpr input = parse parser "exp" input
