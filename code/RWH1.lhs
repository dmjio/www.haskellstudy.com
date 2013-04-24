\begin{code}

--Exercises :
--Enter the following expressions into ghci. What are their types? 

a = 5 + 8  -- Num a => a
b = 3 * 5 + 8  -- Num a => a
c = 2 + 4 -- Num a => a
d = (+) 2 -- Num a => a -> a 
e = sqrt 16 -- Floating a => a
f = succ 6 -- (Enum a, Num a) a => a 
g = succ 7 -- Enum a => a 
h = pred 9 -- Enum a => a
i = pred 8 -- Enum a => a
j = sin (pi / 2) -- Floating a => a 
k = truncate pi -- (Integral b) => b
l = round 3.5 -- (Integral b) => b
m = round 3.4 -- (Integral b) => b
n = floor 3.7 --(Integral b) => b
o = ceiling 3.3 --(Integral b) => b
\end{code}