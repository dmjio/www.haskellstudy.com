\begin{code}

-- | 1. Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail. As a hint, you might want to consider using the following types

import Data.List
import System.Environment

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast xs = Just $ (head . reverse) xs

-- | 2. Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False.

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith pred (x:xs) = case pred x of
    True -> [x] : splitWith pred xs
    False -> [x] : splitWith pred xs

-- | 3. Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input.



-- | 4. Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

-- | 1. Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the section called “Explicit recursion”.

-- | 2. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite it to fix this problem.

-- | 3. The Prelude function concat concatenates a list of lists into a single list, and has the following type. Write your own definition of concat using foldr

-- | 4. Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr

-- | 5. The Data.List module defines a function, groupBy, which has the following type.

-- | 6. How many of the following Prelude functions can you rewrite using list folds?

-- | 7. 












\end{code}