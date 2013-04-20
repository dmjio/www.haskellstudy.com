\begin{code}
{-# OPTIONS_GHC -XFlexibleInstances -XUndecidableInstances #-}

module Main where
  
import Data.Char
import Control.Monad

--Fokker and Functional Parsers
--http://www.staff.science.uu.nl/~fokke101/article/parsers/parsers.ps

--Good if we are dealing with only one parser
--Not good for combinators. Also, deterministic

--type Parser s = String -> s

--Good, but deterministic, doesn't handle multiple correct parsings

--type Parser s = String -> (s, String)

--An Example

---------------------------------------------------------------------
-- data Expression = Const Double                                  --
--                 | Add Expression Expression                     --
--                 | Mul Expression Expression                     --
--                 deriving (Show)                                 --
-- 
-- -- My sample implemenation of expressionParse                   --
-- expressionParse :: String -> Expression                         --
-- expressionParse [x] = Const (read [x] :: Double)                --
-- expressionParse (x:y:xs)                                        --
--     | isDigit y = Const (read [x] :: Double)                    --
--     | y == '+' = Add (expressionParse [x]) (expressionParse xs) --
--     | y == '*' = Mul (expressionParse [x]) (expressionParse xs) --
--                                                                 --
-- *Main Control.Monad.State Data.Char> expressionParse "1+2*3"    --
-- Add (Const 1.0) (Mul (Const 2.0) (Const 3.0))                   --
---------------------------------------------------------------------

-- The above is good, but we can't handle multiple correct entries here
--(Const 1,"+2*3"), or
--(Add (Const 1) (Const 2), "*3")

--The solution is to use the following type which allows for non-deterministic solutions

type Parser s = String -> [(s, String)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = \s -> case s of
  [] -> []
  a:as -> [ (a,as) | f a ]

alpha, digit :: Parser Char --cool didn't now you could do this
alpha = satisfy isAlpha
digit = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (==c) 

string :: String -> Parser String
string str = \s -> [(str,u) | let (t,u) = splitAt (length str) s, str == t]

fmap' :: (a -> b) -> Parser a -> Parser b
fmap' f p = \s -> [(f a, t) | (a,t) <- p s]

truep, falsep :: Parser Bool
truep = fmap' (const True) $ string "True"
falsep = fmap' (const False) $ string "False"

----------------------------------------------------------------------------------
-- Exercise 10.1. Haskell doesn't automatically make monads into functors, but  --
-- there's a natural way in which it could. Complete the following declaration  --
-- in the natural way:                                                          --
----------------------------------------------------------------------------------

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
  
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Monad m => Functor m where
--  fmap f ma = ma >>= \x -> return $ f x 
--  fmap f ma = ma >>= (return . f) -- eta reduced
--  fmap f ma = return f `ap` ma -- return lifts function f :: (a->b) into m b 
    fmap = liftM -- even simpler 

infixr 5 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \s -> p s ++ q s

boolp :: Parser Bool
boolp = truep +++ falsep

many, many1 :: Parser a -> Parser [a]

succeed :: a -> Parser a
succeed a = \s -> [(a,s)]

many1 p = \s -> [(a:as,u) | (a,t) <- p s, (as,u) <- many p t]
many p = succeed [] +++ many1 p

data IntV = IntV Int
          deriving (Show)
                         

\end{code}
