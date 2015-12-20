{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative (Alternative, (<|>), empty)
import qualified Data.Map as M

import Tokens

newtype Parser a              =  P (String -> [(a,String)])

instance (Show a) => Show (Parser a) where
  show (P lf) = show (lf "test")

instance Functor Parser where
  fmap f (P lf) = error "You must implement (>>=)"

instance Applicative Parser where
  pure v = P (\inp -> [(v,inp)])
  (P lf) <*> ss = error "You must implement <*>"-- P lf ss(\inp -> [(v,inp)])

instance Monad Parser where
   return v                   =  P (\inp → [(v,inp)])
   p >>= f                    =  P (\inp →
                                        case parse p inp of
                                          [(v, out)]→ parse (f v) out
                                          [] → [])
                                          
     
--   (P pa) >>= f = P (\inp -> (f pa) inp)

instance MonadPlus Parser where
  mzero                      =  P (\inp -> [])
  p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])
instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

failure :: Parser a
failure =  mzero

item :: Parser Char
item =  P (\inp -> case inp of
              []     -> []
              (x:xs) -> [(x,xs)])

              
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p =  do x ← item
            if p x then return x else failure

digit :: Parser Char
digit =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char   :: Char -> Parser Char
char x =  sat (== x)

string                        :: String → Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a → Parser [a]
many p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  ← p
                                    vs ← many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  ← lower
                                    xs ← many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs ← many1 digit
                                    return (read xs)

int :: Parser Int
int = (char '-') >>= (\ c → nat >>= (\ n → (return (-n)+++ nat)))
-- (do char '-'
--          n ← nat
--          return (-n))
--         +++ nat

(☾) :: Parser Token
(☾) =  char '(' >> return LeftBrace

(☽) :: Parser Token
(☽) =  char ')' >> return RightBrace

operator :: Parser Token
operator = do
  c ← char '-' <|> char '+' <|> char '*' <|> char '/' <|> char '^' <|> char '.'
  case (M.lookup [c] operatorsTable) of
    Just iu → return $ iu

tok :: Parser Token
tok = do
  t ← operator <|> (☾) <|> (☽)
  return t

-- all expression
expression :: Parser [Token]
expression = do
  ts ← many1 tok
  return ts