\begin{code}

import Data.List hiding (intersperse)
import Data.Ord
-- | 1.Write the converse of fromList for the List type: a function that takes a List a and generates a [a]. 

data List a = Empty | Cons a (List a) deriving (Show, Eq)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs
   
-- | 2.Define a tree type that has only one constructor, like our Java example. Instead of the Empty constructor, use the Maybe type to refer to a node's children. 

data MaybeTree a = MaybeNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))

-- More exercises

-- | 1.Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- | 2.Add a type signature for your function to your source file. To test it, load the source file into ghci again.

-- see above

-- | 3.Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)

myAvg [] = 0
myAvg xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)

-- | 4.Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1]. 

mkPali :: [a] -> [a]
mkPali [x] = [x,x]
mkPali xs = xs ++ (reverse xs) 
 
-- | 5.Write a function that determines whether its input list is a palindrome

isPali :: Eq a => [a] -> Bool
isPali [] = True
isPali [x] = True
isPali xs = xs == (reverse xs)

-- | 6.Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)

sorter :: Ord a => [[a]] -> [[a]]
sorter = sortBy compare

-- | 7.Define a function that joins a list of lists together using a separator value

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [x] = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)

-- | 8.Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.

data Tree a = Node a (Tree a) (Tree a) 
     | Empty 
     deriving (Show)

treeHeight Leaf = 0
treeHeight (Node _ a b) = 1 + max (treeHeight a) (treeHeight b)

-- | 9.Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.

type Point = (Int,Int)
data Direction = Left | Right | Straight (Eq)

-- |10.Write a function that calculates the turn made by three 2D points and returns a Direction.

-- |11.Define a function that takes a list of 2D points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction.

-- |12.Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull. is, and how the Graham scan algorithm should work, on Wikipedia

\end{code}