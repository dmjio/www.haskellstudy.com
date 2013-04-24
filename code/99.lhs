99 Haskell Problems
\begin{code}
module Main where

import qualified Data.Map as M
import System.Random
import Data.List
import Data.Ord
import Data.Function
import Data.Char


--1 myLast [1,2,3,4] (*) Find the last element of a list.
myLast :: [a] -> a
myLast = head . reverse

--2 myButLast [1,2,3,4] 3, (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- 3 (*) Find the K'th element of a list. The first element in the list is number 1.
-- elementAt [1,2,3] 2
elementAt :: [a] -> Int -> a
elementAt xs num = xs !! (num - 1)

-- 4 myLength [123, 456, 789] 3
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5 reverse [1,2,3,4] = [4,3,2,1]
reverse' :: [a] -> [a]
reverse' (x:xs) = reverse xs ++ [x]

-- 6 (*) Find out whether a list is a palindrome.
isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

-- 7 (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq, Read)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--8 (**) Eliminate consecutive duplicates of list elements.
-- compress ["a","a","a","a","b","c","c","a","a","d","e","e","-- e","e"] = ["a","b","c","a","d","e"]
-- ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]

compress ::(Eq a) => [a] -> [a]
compress = map head . group

-- 9(**) Pack consecutive duplicates of list elements into subl-- ists. If a list contains repeated elements they should be pl-- aced in separate sublists.
--pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', -- 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: (Eq a) => [a] -> [[a]]
pack xs = group xs

-- 10 (*) Run-length encoding of a list. Use the result of prob-- lem P09 to implement the so-called run-length encoding data -- compression method.
-- encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),-- (1,'d'),(4,'e')]

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\x -> (length x, head x)) . group

--or as a list comp [ (length x, head x) | x <- group xs ]

-- 11
-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data Count a = Multiple Int a | Single a deriving (Show)

encodeModified :: Eq a => [a] -> [Count a]
encodeModified ys = trans (encode ys) where
  trans :: Eq a => [(Int,a)] -> [Count a]
  trans [] = []
  trans ((1,x):xs) = [Single x] ++ trans xs
  trans ((x,y):xs) = [Multiple x y] ++ trans xs


--12(**) Decode a run-length encoded list.
  -- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

--decodeModified
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
-- ==  "aaaabccaadeeee"

decodeModified xs = concatMap decoder xs where
  decoder (Multiple x y) = replicate x y
  decoder (Single x) = [x]


--13 (**) Run-length encoding of a list (direct solution).
--Implement the so-called run-length encoding data compression method --directly. I.e. don't explicitly create the sublists containing the d--uplicates, as in problem 9, but only count them. As in problem P11, --simplify the result list by replacing the singleton lists (1 X) by X

--encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple-- 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

encodeDirect :: Eq a => [a] -> [Count a]
encodeDirect xs = encodeHelper xs 1 where
  encodeHelper [] _  = []
  encodeHelper (x:[]) num = case num of
    1 -> [Single x]
    otherwise -> [Multiple num x]
  encodeHelper (x:xs) num
    | x /= head xs =
      if num == 1
      then [Single x] ++ encodeHelper xs num
      else [Multiple num x] ++ encodeHelper xs 1
    | x == head xs = encodeHelper xs (num+1)

--14 duplicating the elements of a list
-- dupli [1, 2, 3]
dupli :: [a] -> [a]
dupli xs =  concatMap (replicate 2) xs

-- point-free:
dupli' = concatMap (replicate 2)
--using list comps: dupli list = concat [ [x,x] <- list ]
--using list monad dupli xs = xs >>= (\x -> [x,x])
--sick foldr usage
dupli'' = foldr (\ x xs -> x : x : xs) []
--sicker point free foldr
--dupli = foldr (\x -> (x:) . (x:)) []


-- 15 5 Problem 15
--(**) Replicate the elements of a list a given number of times.
--repli "abc" 3
--"aaabbbccc"

repli :: [a] -> Int -> [a]
repli xs num = concatMap (replicate num) xs

-- point free
repli' = flip $ concatMap . replicate

--6 Problem 16
--(**) Drop every N'th element from a list.

-- dropEvery "abcdefghik" 3

dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery xs num = dropEvery' xs num 0 where
  dropEvery' (x:xs) num count
    | xs == [] = []
    | num == count = dropEvery' xs num 0
    | num /= count = dropEvery' (x:xs) count (num+1)

--7 Problem 17
--(*) Split a list into two parts; the length of the first part-- is given.

split :: [a] -> Int -> ([a],[a])
split xs num = splitAt num xs

split' :: [a] -> Int -> ([a],[a])
split' = flip splitAt


--18 (**) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs start finish = take (finish - (start-1)) $ drop (start-1) xs

--19 (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs num = let (first, last) = splitAt (numero num) xs
                in last ++ first where
                  numero num
                    | num > len = numero (num - len)
                    | num < 0 = numero (num + len)
                    | otherwise = num
                                  where len = length xs

--20 (*) Remove the K'th element from a list.
--removeAt 2 "abcd" == ('b',"acd")

removeAt :: Eq a => Int -> [a] -> (a,[a])
removeAt num xs = let result = xs !! (num-1)
                      in (result, delete result xs)


--1 Problem 21
--Insert an element at a given position into a list.

insertAt :: Eq a => a -> [a] -> Int -> [a]
insertAt new xs num = let (first,last) = splitAt getNum xs
                      in first ++ [new] ++ last where
                        getNum
                          | num > len = error "Outside of Bounds"
                          | num < 0 = error "Outside of bounds"
                          | otherwise = num
                                        where len = length xs


-- Problem 22
--Create a list containing integers within a given range
-- range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range s f = [s..f]


--or range = enumFromTo

--Problem 23
--Extract a given number of randomly selected elements from a list.
--rnd_select "abcdefgh" 3 >>= putStrLn
--eda

-- rnd_select :: [a] -> Int -> [a]
-- rnd_select xs num = do
--     gen <- newStdGen


--skipping to 26
combinations :: [a] -> Int -> [[a]]
combinations xs num = filter (\x -> length x == num) $ subsequences xs where

--skipping to 28
--Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort xs = sortBy len xs where
  len xs ys = compare (length xs) (length ys)

-- beautiful code. Utterly declarative
ls :: [[a]] -> [[a]]
ls = sortBy (compare `on` length)


--Arithmetic
--determine whether a given integer is prime or not
--isPrime 7 == True

--isPrime :: Int -> Bool
--let isPrime n = (length [d|d<-[1..(n `div` 2)], n `mod` d == 0]) == 1

--find gcd 32
myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = gcd b (a `mod` b)

--33 find coprime
coprime :: Int -> Int -> Bool
coprime a b = case (myGCD a b) of
  1 -> True
  _ -> False

--another cleaner way
coprime' :: Int -> Int -> Bool
coprime' a b = myGCD a b == 1

--Euler's totient function 34
totient :: Int -> Int
totient num = length $ filter (\x -> coprime num x) [1..num-1]


-- logical codes
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nor' :: Bool -> Bool -> Bool
nor' a b = not'( or' a b)

nand' a b = not' $ and' a b

xor' :: Bool -> Bool -> Bool
xor' a b = not' (equ' a b) where
  equ' :: Bool -> Bool -> Bool
  equ' True True = True
  equ' False False = True
  equ' _ _ = False


--my implementaion of a prime finder
isPrime :: Int -> Bool
isPrime x = let result = filter (\z -> x `mod` z == 0)
                         [2..(floor $ sqrt $ fromIntegral x)]
                in null result

primeFacs num = primeHelp (ip num)

primeHelp (x:xs)
  | xs == [] = []
  | ipF x == True = [x] ++ primeHelp xs
  | ipF x == False = primeHelp (ip x) ++ primeHelp xs

ipF = null . ip
--this is beautiful
ip x = filter ((== 0) . mod x) $ takeWhile ((<= x) . (^ 2)) [2..]

factor :: Integer -> [Integer]
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
           in (prime :) $ factor $ div n prime

primes = nubBy (((==0).).rem) [2..]

--54 binary trees...
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty -- helper function for this tree

--55 write a function to construct completely balanced binary trees
-- tbal :: int -> [tree char]
-- tbal 0 = [empty]
-- tbal x = branch x (branch (x-1) branch (x-1))
counter :: Tree a -> Int
counter Empty = 1
counter (Branch c a b) = 1 + (counter a) + (counter b)

makePerfect x = makeHelper 'x' x where
  makeHelper l num
    | num == 0 = Empty
    | otherwise = Branch l (makeHelper l (x-1)) (makeHelper l (x-1))

--61
data MyTree = None | Node Int (MyTree) (MyTree) deriving (Show, Eq, Read)
tree2  = (Node 2
             (Node 7
              (Node 2 None None)
              (Node 6
               (Node 10 None None)
               (Node 11 None None)))
             (Node 5
              (Node 9
               (Node 13 None None)
               None) None))

--this counts every node (not the right answer, but still useful)
reduceTree :: MyTree -> Int
reduceTree (None) = 0
reduceTree (Node num l r) = 1 + (reduceTree l) + (reduceTree r)

--Collect the leaves of a binary tree in a list
countLeaves :: MyTree -> Int
countLeaves (None) = 0
countLeaves (Node x None None) = 1
countLeaves (Node num l r) = countLeaves l + countLeaves r

--Collect the leaves of a binary tree in a list
collectLeaves :: MyTree -> [Int]
collectLeaves (None) = []
collectLeaves (Node x None None) = [x]
collectLeaves (Node num l r) = collectLeaves l ++ collectLeaves r

--62 Collect the internal nodes of a binary tree in a list
collectInternal :: MyTree -> [Int]
collectInternal (None) = []
collectInternal (Node x None None) = []
collectInternal (Node x l None) = [x] ++ collectInternal l
collectInternal (Node x None r) = [x] ++ collectInternal r
collectInternal (Node num l r) = [num] ++ collectInternal l ++ collectInternal r



--96
fullWords :: Int -> String
fullWords x = concat $ intersperse "-" [digits !! digitToInt d | d <- show x ] where
  digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
\end{code}