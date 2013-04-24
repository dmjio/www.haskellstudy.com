\begin{code} 

-- | 1. What at are the types of the following expressions? 

a = (["foo", "bar"], 'a')  -- ([String],Char) or ([[Char]], Char)
b = [(True, []), (False, [['a']])] -- [(Bool, [String]] or [(Bool), [[Char]]

-- | 1. Haskell provides a standard function, last :: [a] -> a, that returns the last element of a list. From reading the type alone, what are the possible valid behaviours (omitting crashes and infinite loops) that this function could have? What are a few things that this function clearly cannot do? 

-- This function clearly cannot take the last element of an empty list

-- | 2. Write a function lastButOne, that returns the element before the last. 

lastButOne = head . reverse . init

-- | 3. Load your lastButOne function into ghci, and try it out on lists of different lengths. What happens when you pass it a list that's too short? 

-- It throws an exception
exception = lastButOne [1]

\end{code}