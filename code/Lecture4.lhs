\begin{code}
module Main where

--Exercise 4.1. Unsurprisingly, the Int type is documented in Data.Int, but surprisingly, there is no Data.Integer. 
--Where is Integer documented? Hint: the documentation comes with an index.

--Integer is defined in the Standard Prelude

--Exercise 4.2.What is the type of (^2) in the definition below? What is the type of map?

--The type signature of (^2) is num
--(^2) :: Num a => a -> a

as :: [Int]
as = [1..10]
bs = map (^2) as -- (^2) = Int -> Int


--The type of map is: 
map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs

--Exercise 4.3. Complete the instance declaration for Num Complex by implementing (*), abs, signum, and fromInteger. Note that the definition of abs should be via the pythagorean theorem, but that the type definition of abs requires that it return a value of the same type, i.e., a Complex number whose imaginary part is zero. The definition of signum is particularly tricky. If you satisfy the equation in the documentation, then the signum of a non-zero Complex should be an angle, i.e., a Complex number whose length is 1.  

-- (skipped)

--Exercise 4.4. What is the type of the occurence of map in the following?


stars = map (`replicate` '*') [5]

-- map :: (a -> b) -> [a] -> [b]
-- replicate :: Int -> a -> [a]
-- [5] :: [Int]

--map :: (Int -> [Char]) -> [Int] -> [[Char]]


\end{code}