\begin{code}
-----------------------------------------------------------------------------------------------------------------
-- Exercise 11.1. Give a natural deduction proof of (alpha -> beta) -> (beta -> sigma) -> (alpha -> sigma)     --
-- What standard Haskell function has a similar type?                                                          --
-----------------------------------------------------------------------------------------------------------------

--The haskell function which has the type signature of :: (a -> b) -> (b -> c) -> a -> c
--would be: flip (.)

--------------------------------------------------------------------------
-- Exercise 11.2. Prove the Sherlock Holmes syllogism by Quine's method. --
---------------------------------------------------------------------------

-- The Sherlock Holmes syllogism: (a V b) -> ~a -> b

-- We split on a. 
-- First we substitute phi[a := T] to yield (T V b) -> ~T -> b
-- This further reduces to: T -> _|_ -> b
-- This reduces to T -> T which is a tautology

-- Now we substitute phi[ a := _|_ ] to yield (_|_ V b) -> ~_|_ -> b
-- This further reduces to: b -> T -> b
-- Which further reduces to : b -> b, which is a tautology

module Proposition where

import Text.ParserCombinators.ReadP
import Data.Char

data Proposition = Const Bool
  | Var String
  | Not Proposition
  | And Proposition Proposition
  | Or Proposition Proposition
  | Implies Proposition Proposition deriving (Eq,Ord,Show)
  
prop_parse :: ReadP Proposition  
prop_parse = prec0 where
  wchar c = skipSpaces >> char c
  wstring s = skipSpaces >> string s
  cparse value parser = fmap (const value) parser
  
  prec0 = chainr1 prec1 $ cparse Implies $ wstring "->"
  prec1 = chainl1 prec2 $ cparse Or $ wchar '|'
  prec2 = chainl1 prec3 $ cparse And $ wchar '&'
  prec3 = prefix  prec4 $ cparse Not $ wchar '!'
  prec4 = boolparse +++ varparse +++ parenparse prec0

  varparse = do
    skipSpaces
    var <- munch1 isLower
    return $ Var var

  boolparse = cparse (Const True) (wchar 'T') +++ cparse (Const False) (wchar 'F')
  parenparse = between (wchar '(') (wchar ')')


prefix :: ReadP a -> ReadP (a -> a) ->  ReadP a
prefix p op = pp where
  pp = p +++ do
    f <- op
    a <- pp
    return $ f a

instance Read Proposition where
  readsPrec _ = readP_to_S prop_parse

data Expression = DoubleValue Double   --Precendence = { Product, Quotient, Difference, Sum }
                | Sum Expression Expression
                | Difference Expression Expression
                | Product Expression Expression
                | Quotient Expression Expression
                deriving (Show, Eq, Ord)

dub :: ReadP Expression
dub = do
  a <- munch1 isDigit
  char '.'
  b <- munch1 isDigit
  return $ DoubleValue (read (a++"."++b) :: Double)

expr_parse :: ReadP Expression
expr_parse = prec0 where
  wdub = skipSpaces >> dub 
  wchar c = skipSpaces >> char c
  cparse val parser = fmap (const val) parser
  prec0 = chainl1 prec1 $ cparse Sum $ wchar '+'
  prec1 = chainl1 prec2 $ cparse Difference $ wchar '-'
  prec2 = chainl1 prec3 $ cparse Quotient $ wchar '/'
  prec3 = chainl1 prec4 $ cparse Product $ wchar '*'
  prec4 = dub +++ parenparse prec0
  parenparse = between (wchar '(') (wchar ')')

instance Read Expression where
  readsPrec _ = readP_to_S expr_parse

eval_parse x = eval_p (read x :: Expression)

eval_p :: Expression -> Double
eval_p p  = eval p where
  eval (Product a b) = eval a * eval b
  eval (Sum a b) = eval a + eval b
  eval (Difference a b) = eval a - eval b
  eval (Quotient a b) = eval a / eval b
  eval (DoubleValue val) = val

main = do
  print $ eval_parse "(1.99*(1.3+4.5))"

\end{code}