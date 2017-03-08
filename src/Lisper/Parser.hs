{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

-- Parser

module Lisper.Parser (parser, readExpr) where

import           Lisper.Core
import           Text.ParserCombinators.Parsec

-- See § 2.1 of R5RS for grammar and allowed characters
identifier :: Parser Char
identifier = oneOf "!#$%&|*+-/:<=>?@^_~"

quote :: Parser Char
quote = char '\''

bool :: Parser Bool
bool = char '#' >> (char 'f' <|> char 't') >>= \b -> return $ b == 't'

str :: Parser String
str = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return x

-- Lisp types

parseString :: Parser LispVal
parseString = String <$> str

-- [fix] - Parse negative numbers. Sort test fails with neg numbers
parseNumber :: Parser LispVal
parseNumber = Number <$> (read <$> many1 digit)

parseAtom :: Parser LispVal
parseAtom = Atom <$> (many1 (letter <|> digit <|> identifier))

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
    _ <- quote
    x <- parseExpr
    return $ List [Quote, x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseBool
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

readExpr :: String -> Either ParseError [LispVal]
readExpr "" = Right []
readExpr input = parse parser "exp" input
