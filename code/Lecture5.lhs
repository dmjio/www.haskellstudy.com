\begin{code}
module Main where

--5.1
--Write the Haskell function cseq
--cseq(10)=[10,5,16,8,4,2,1]

collatz :: Integral a => a -> a
collatz x 
    | odd x = 3*x + 1
    | otherwise = x `div` 2

cseq :: Integer -> [Integer]
cseq = takeUntil (/= 1) . iterate collatz

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs)
    | f x = x : takeUntil f xs
    | otherwise = [x]
 
result = cseq 10

--5.2 (skipped)
-- Use the definitions of Tree and Forest above. A tree is balanced if it is either a leaf, or if it is a node and the contained forest is balanced. A forest is balanced if all of its constituent subtrees are balanced, and they all have the same weight. Using the mutually recursive functions treeWeight and forestWeight, write 
--mutually recursive predicates treeBalance :: (Eq a, Num a) => Tree a -> Bool 
--forestBalance :: (Eq a,Num a) => Forest a -> Bool.

--Once you've done this, reimplement your solution by adding an appropriate balance type declaration to the Forestry class, and implement this function in both instance blocks.
--Not completed
data Tree a = Leaf a | Node (Forest a)
data Forest a = Forest [Tree a]

treeWeight :: Num a => Tree a -> a
treeWeight (Leaf a) = a
treeWeight (Node f) = forestWeight f
    
forestWeight :: Num a => Forest a -> a
forestWeight (Forest ts) = sum $ map treeWeight ts

class Forestry w where
    weight :: Num n => w n -> n
    balance :: Num n => w n -> Bool
  

--Exercise 5.3. Let's suppose we want to compute with polynomials. Consider the data declaration:

data Poly = Poly [Double]

--Where the boxed list contains the coefficients of the polynomial in increasing order, i.e., the constant term comes first. Thus Poly [-2,0,1] represents the polynomial

--Write a function eval :: Poly -> Double -> Double. Your implementation should fully encapsulate (i.e., place in a where clause, so that their definitions are not externally visible) any helper functions.

--for only computing polynomials in the form x^2 + x + 0
eval :: Poly -> Double -> Double
eval (Poly (x:y:z:[])) d = z*(d^2) + y*d + x

\end{code}