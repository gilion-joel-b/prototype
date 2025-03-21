module Parsing where

import Control.Applicative
import Data.Char

-- Parser type
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Basic parser combinators
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> [(f x, rest) | (x, rest) <- p input]

instance Applicative Parser where
    pure x = Parser $ \input -> [(x, input)]
    (Parser p1) <*> (Parser p2) = Parser $ \input -> 
        [(f x, rest2) | (f, rest1) <- p1 input, (x, rest2) <- p2 rest1]

instance Alternative Parser where
    empty = Parser $ \_ -> []
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input ++ p2 input

-- Basic parsers
item :: Parser Char
item = Parser $ \input -> case input of
    []     -> []
    (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat pred = Parser $ \input -> case input of
    (x:xs) | pred x -> [(x, xs)]
    _              -> []

char :: Char -> Parser Char
char c = sat (== c)

space :: Parser Char
space = sat isSpace

digit :: Parser Char
digit = sat isDigit

-- Parse a natural number
natural :: Parser Integer
natural = read <$> some digit

-- Parse whitespace
spaces :: Parser String
spaces = many space

-- Token parser that handles whitespace
token :: Parser a -> Parser a
token p = do
    spaces
    v <- p
    spaces
    return v

-- Arithmetic expression parser
symbol :: String -> Parser String
symbol xs = token (string xs)
  where
    string [] = return []
    string (x:xs) = do
        char x
        string xs
        return (x:xs)



