module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
instance Monad Parser where
    return v = Parser $ \s -> Just (v, s)
    mp >>= f = Parser $ \s -> case parse mp s of
                                Nothing -> Nothing
                                Just (v, rest) -> parse (f v) rest

instance Applicative Parser where
    af <*> mp = do
        f <- af
        v <- mp
        return $ f v
    pure = return

instance Functor Parser where
    fmap f mp = do
        x <- mp
        return $ f x

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                 Nothing -> parse p2 s
                                 x       -> x

-- Parses a single character
charParser :: Char -> Parser Char
charParser c = Parser $ \input -> case input of
    (x:xs) -> if x == c then Just (x, xs) else Nothing
    [] -> Nothing

-- Parses a character that satisfies a predicate
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \input -> case input of
    [] -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

macro :: Parser String
macro = some $ predicateParser isUpper <|> predicateParser isDigit

variable :: Parser String
variable = some $ predicateParser isAlphaNum

-- Parses a variable
parseVar :: Parser Lambda
parseVar = Var <$> variable

-- Parses a macro
parseMacro :: Parser Lambda
parseMacro = Macro <$> macro

-- Parses an abstraction
parseAbs :: Parser Lambda
parseAbs = do
    charParser '\\'
    x <- variable
    charParser '.'
    e <- parseExpr
    return $ Abs x e

-- Parses an application
parseApp :: Parser Lambda
parseApp = do
    charParser '('
    e1 <- parseExpr
    charParser ' '
    e2 <- parseExpr
    charParser ')'
    return $ App e1 e2

parseExpr :: Parser Lambda
parseExpr = parseApp <|> parseAbs <|> parseMacro <|> parseVar

-- Parses a lambda expression
parseLambda :: String -> Lambda
parseLambda input = case parse parseExpr input of
    Just (result, "") -> result

-- 3.3.
parseBinding :: Parser Line
parseBinding = do
    name <- macro
    charParser '='
    expr <- parseExpr
    return $ Binding name expr

parseEval :: Parser Line
parseEval = Eval <$> parseExpr

lineParser :: Parser Line
lineParser = parseBinding <|> parseEval

-- Parses a line
parseLine :: String -> Either String Line
parseLine input = case parse lineParser input of
    Just (result, "") -> Right result
    _ -> Left "Invalid line"
