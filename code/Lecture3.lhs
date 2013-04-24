\begin{code}
module Main where

    
import NaturalNumber
        
import Prelude hiding (sum, product)

sum [] = 0
sum (x:xs) = x + sum xs

--Exercise 3.1. Implement the product function. This should take a list of numbers, and return their product.

product [] = 1
product (x:xs) = x * product xs

--Use your implementation of product to determine the product of the squares of the first numbers 1 through 10.

result = product .  map (^2) $ [1..10]

--Exercise 3.2. Provide foldr-based implementations of product and map.

sum' = foldr (+) 0
product' = foldr (*) 1

--Exercise 3.3. Write primrec
--The natural recursive function generator for NaturalNumber. 
--Give implementations of (+) and (*) based on primrec.

-- ? unsure about this one
primrec :: (NaturalNumber -> NaturalNumber) ->
           NaturalNumber -> NaturalNumber -> NaturalNumber

primrec  _ b Zero = b
primrec f b (S x) = f (primrec f b x)

--What is the standard prelude definition of concat?
concat :: [[a]] -> [a]
concat = foldr (++) []
\end{code}