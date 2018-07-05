-- Graham Hutton: Programming in Haskell
-- Chapter 13 practice

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
             []     -> []
             (x:xs) -> [(x,xs)])

instance Functor Parser where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap g p = P (\inp -> case parse p inp of
                              []         -> []
                              [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  pg <*> px = P (\inp -> case parse pg inp of
                           []         -> []
                           [(g, out)] -> parse (fmap g px) out)

threeA :: Parser (Char, Char)
threeA = pure g <*> item <*> item <*> item
  where g x y z = (x,z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                           []         -> []
                           [(v, out)] -> parse (f v) out)

threeM :: Parser (Char, Char)
threeM = do x <- item
            item
            z <- item
            return (x,z)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
