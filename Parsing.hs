module Parsing where

import Prelude hiding (fail)
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Monad Parser where
  return x = Parser (\ s -> [(x, s)])
  p >>= q = Parser (\ s -> [ (y, s'')
                           | (x, s')  <- apply p s
                           , (y, s'') <- apply (q x) s'])

instance Applicative Parser where
  pure x = Parser (\ s -> pure (x, s))
  Parser f <*> Parser x = Parser (\ s -> [ (f' x', s'')
                                         | (f', s')  <- f s
                                         , (x', s'') <- x s'])

instance Functor Parser where
  fmap f xs = xs >>= return . f

-- showsPrec :: Int -> Expr -> ShowS

-- show e = showsPrec 0 e ""

-- showsPrec p (Con n) = showsString (show n)

-- showsPrec p (Bin op e1 e2) = showParen (p->q) (showsPrec q e1 . showSpace
--                            . showsop op . showSpace . showsPrec (q+1) e2)
--                              where
--                                q = prec op

fail = Parser (\s -> [])

guard :: Bool -> Parser ()
guard True  = return ()
guard False = fail

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- getc; if p c then return c else fail }

getc :: Parser Char
getc = Parser f
       where
         f []       = []
         f (c : cs) = [(c, cs)]

char :: Char -> Parser ()
char x = do { c <- sat (== x); return () }

string :: String -> Parser ()
string []       = return ()
string (x : xs) = do { char x; string xs; return () }

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do { d <- sat isDigit; return (cvt d) }
        where
          cvt d = fromEnum d - fromEnum '0'

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
        where
          f s = let ps = apply p s in
                if null ps then apply q s
                else ps

-- many :: Parser a -> Parser [a]
-- many p = do { x <- p; xs <- many p; return (x : xs) }
--     <|> none

none = return []

lowers :: Parser String
lowers = many lower

space :: Parser ()
space = many (sat isSpace) >> return ()

symbol :: String -> Parser ()
symbol xs = space >> string xs

token :: Parser a -> Parser a
token p = space >> p

some :: Parser a -> Parser [a]
some p = do { x <- p; xs <- many p; return (x : xs) }

many :: Parser a -> Parser [a]
many p = optional (some p)

optional :: Parser [a] -> Parser [a]
optional p = p <|> none

showSpace = showChar ' '

manywith :: Parser b -> Parser a -> Parser [a]
manywith q p = optional (somewith q p)

somewith :: Parser b -> Parser a -> Parser [a]
somewith q p = do { x <- p
                  ; xs <- many (q >> p)
                  ; return (x : xs)
                  }

paren :: Parser a -> Parser a
paren p = do {symbol "(";
              x <- p;
              symbol ")";
              return x}
