\begin{code}
--Exercise 9.1. Write a sibling function

--sibling :: Person -> Person -> Bool
--in both monadic and non-monadic form, and show that it works as expected. Let's say that two people are siblings if they are distinct and share at least one parent. In this particular database, "Christine" and "Andrew" are the only two siblings.

module Relations where

import Data.Char
import Control.Monad
import Data.Maybe
import System.IO

people_db = [("Stuart",1),
              ("Kim",2),
              ("Christine",3),
              ("Andrew",4),
              ("George Wilbur",5),
              ("Eleanor",6),
              ("George William",7),
              ("Gladys",8),
              ("Raymond",9),
              ("Helen",10),
              ("Ken",11),
              ("Karen",12)]

parents_db = [(1,5,6),      -- person, father, mother
              (2,11,12),
              (3,1,2),
              (4,1,2),
              (5,7,8),
              (6,9,10)]

name2id :: String -> Maybe Int
name2id person = uniq [id | (name,id) <- people_db, name == person]

id2name :: Int -> Maybe String
id2name id = uniq [name | (name,pid) <- people_db, id == pid]

father :: String -> Maybe String
father name = do
  id <- name2id name
  fid <- uniq [fid | (cid,fid,_) <- parents_db, cid == id]
  fname <- id2name fid
  return fname

mother :: String -> Maybe String
mother name = do
  id <- name2id name
  mid <- uniq [mid | (cid,_,mid) <- parents_db, cid == id]
  fname <- id2name mid
  return fname

grandfather :: String -> Maybe String
grandfather child = do
  f <- father child
  g <- father f
  return g

uniq :: [a] -> Maybe a
uniq [x] = Just x
uniq _   = Nothing

type Person = String

extract :: Maybe Bool -> Bool
extract Nothing = False
extract (Just x) 
  | x = True
  | otherwise = False

--monadic style
sibling :: Person -> Person -> Bool
sibling p1 p2 = extract $ do
  (f1,m1) <- lookup p1 people_db >>= \x -> uniq [(fid,mid) | (pid,fid,mid) <- parents_db, pid == x]
  (f2,m2) <- lookup p2 people_db >>= \x -> uniq [(fid,mid) | (pid,fid,mid) <- parents_db, pid == x]
  case ((f1 == f2) || (m1 == m2)) of
    True -> return True
    False -> return False

--non-monadic style
sibling' p1 p2 = let r1 = case lookup p1 people_db of
                       Just x -> uniq [(fid,mid) | (pid,fid,mid) <- parents_db, pid == x]
                       Nothing -> Nothing
                     r2 = case lookup p2 people_db of
                       Just x -> uniq [(fid,mid) | (pid,fid,mid) <- parents_db, pid == x]
                       Nothing -> Nothing
                 in if (r1 == Nothing || r2 == Nothing) 
                    then False
                    else if (r1 == r2)
                         then True
                         else False

test1 = sibling  "Christine" "Andrew"
test2 = sibling' "Christine" "Andrew"
test3 = sibling  "Stuart" "Andrew"
test4 = sibling' "Stuart" "Andrew"

--Exercise 9.2. Consider the following data type

data Conditional a = Success a | Failure String deriving (Show, Eq)

--We might imagine this is as being useful when we have a computation that is organized as a processing pipeline, where there is a possibility of failure at every stage of the pipeline.

--Make Conditional into a Monad, with the obvious intention that successes get passed along by >>=, but failures are immediately returned. Your code should implement the monadic fail function correctly.

--This is very similar to the Maybe example.

--Traditionally, Haskell programmers use the polymorphic Either type for this purpose, but this involves some minor complications for our purposesExercise 9.2. Consider the following data type

---MAKE ME INTO A MONAD!!!!    

addOne = \x -> Success $ x + 1

instance Monad Conditional where
  return x = Success x
  Failure x >>= f = Failure x
  Success x >>= f = f x
  fail x = Failure x

--Left Identity
left = (return 3 >>= \x -> Success $ x + 1) == Success 4 

--Right Identity
right = (Success 4 >>= return) == Success 4

--Associativity
--(f >=> g) >=> h == f >=> (g >=> h)
--or  
--((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
assocTest = ((Success 3 >>= addOne) >>= addOne) == (Success 3 >>= (\x -> addOne x >>= addOne))

gaurd :: Bool -> [()]
gaurd p = [()|p]

r = do
  x <- [1..3]
  y <- ['a'..'c']
  return (x,y)

tst = do
  let message = putStrLn "hello" --testing message inside of tstj
  message

manyFrogs :: Int -> String
manyFrogs i = unlines $ replicate i "frog"

frog :: IO()
frog = do
  putStr "How many frogs would you like? "
  hFlush stdout
  inputStr <- getLine
  let output = manyFrogs (read inputStr)
  putStr output
  frog
  
main :: IO()
--main = readFile "9.hs" >== map toUpper >>=  writeFile "9.txt" 
main = interact . map $ toUpper

--List Monad       
num = [(x,y) | x <- [1..10], y <- [2..11]] -- cartesian product

--The following 3 methods do exactly the same thing
nums = do
  x <- [1..3]
  y <- [4..6]
  gaurd $ even $ x+y -- could also do _ <- gaurd $ even $ x+y, but verbose..
  return (x,y)

nums2 = [(x,y) | x <- [1..3], y <- [4..6], even (x+y)]
        
nums3 = [1..3] >>= \x -> [4..6] >>= \y -> gaurd (even $ x+y) >> return (x,y)

nums4 = do
  x <- [1..3]
  y <- [4..6]
  let tup = (x,y)
  gaurd $ even $ x + y
  return tup
                                          
result = (nums == nums3)
result2 = (nums2 == nums4)
final = result == result2

infixl 1 >==
(>==) :: Monad m => m a -> (a -> b) -> m b
f >== g = f >>= return . g

\end{code}