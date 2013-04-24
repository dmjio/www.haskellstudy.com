\begin{code}
module Main where

--Exercise 1.1: Programming systems (even Haskell) perform according to their definitions, which don't always conform to our naive expectations. Consider the following sequence:

--         > let f x = 0
--         > let g x = f x
--         > let f x = 1
--         > g 0

-- Explain why the result of this evaluation is 0, and not 1.

-- Answer: In haskell values are bound to symbols. 
-- These values are immutable. The only way to make a symbol (or function
-- in our case) mean something else is to bind it to a different value.
-- In this case f x is bound to 0. While f x is bound to 0 it is assigned
-- to g x. Therefore, g x has been bound to 0. Even if we assign f x
-- equal to 1 this means that the value bound to f x has changed, but not the
-- value bound to g x.


-- Exercise 1.2: The law of cosines is a generalization of the Pythagorean Theorem, which allows us to compute the length c of the third side of a triangle, given the lengths of the two other sides a and b, and the included angle γ.

--c^2 = a^2 + b^2 - 2ab * cos y

law_of_cosines a b gamma = let a2 = a^2
                               b2 = b^2
                               twoAB = 2*a*b
                               toRadians :: Int -> Double
                               toRadians d = let deg = mod d 360
                                             in (fromIntegral deg) / 180*pi
                               y = cos $ toRadians gamma
                           in  sqrt $ a2 + b2 - twoAB * y

law_of_cosines2 a b g = sqrt $ a^2 + b^2 + 2*a*b 
                               

-- Hypotenuse.hs
--
-- Define a function for computing 
-- the length of the hypotenuse
-- of a right triangle, using the pythagorian theorem.

sq = (**2)

sumSq [] = 0
sumSq (x:xs) = (**2) x + sumSq xs

hyp x y = (sqrt . sumSq) [x,y]
hyp' x y = (sqrt . sum) $ [x,y] >>= \z -> [sq z]
hyp'' x y = sqrt $ foldl (+) 0 $ fmap sq [x,y]                                     

\end{code}