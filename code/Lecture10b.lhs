Use Text.ParserCombinators.ReadP to implement Read ComplexInt,   
where you can accept either the syntax “12” for  ComplexInt 12 0 
or “(1,2)” for ComplexInt 1 2.                                    

\begin{code}
module Main where

import Text.ParserCombinators.ReadP
import Data.Char

data ComplexInt = ComplexInt Int Int
        deriving (Show)

lparen,rparen,comma :: ReadP Char
lparen = satisfy (=='(') 
rparen = satisfy (==')') 
comma  = satisfy (==',')

parse :: ReadP ComplexInt -> String -> ComplexInt
parse p = (fst . head . readP_to_S p)

-- | Parser for "(1,2)", Use: parse parseTuple "(1,2)"
parseTuple :: ReadP ComplexInt
parseTuple = do
  lparen
  a <- munch isDigit
  comma 
  b <- munch isDigit
  rparen
  return $ ComplexInt (read a :: Int) (read b :: Int)

-- | Parser for "12", Use: parse parseString "12"
parseString :: ReadP ComplexInt  
parseString = do
  nums <- munch isDigit
  return $ ComplexInt (read nums :: Int) 0
  
main :: IO()
main = do
  print "Parsing the tuple \"(1,2)\""
  print $ parse parseTuple "(1,2)"
  print "Parsing the tuple \"(234,2309)\""
  print $ parse parseTuple "(234,2309)"
  print "Parsing the string \"12\""
  print $ parse parseString "12"
  print "Parsing the string \"20394\""
  print $ parse parseString "20394"
\end{code}

