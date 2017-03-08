{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

-- Parser

module Lisper.Parser (parser, readExpr) where

import           Lisper.Core
import           Text.ParserCombinators.Parsec

-- See § 2.1 of R5RS for grammar and allowed characters
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

-- [fix] - Parse negative numbers. Sort test fails with neg numbers
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
parseList = List <$> sepEndBy parseExpr spaces

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

readExpr :: String -> Either ParseError [LispVal]
readExpr "" = Right []
readExpr input = parse parser "exp" input
