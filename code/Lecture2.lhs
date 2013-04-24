\begin{code}
{- NaturalNumber.hs
   A partial Haskell implementation of the natural numbers via the Peano-Dedekind Axioms for arithmetic, with a unary internal representation.
 -}

module NaturalNumber where

import Debug.Trace

data NaturalNumber = Zero | S NaturalNumber deriving (Show)

zero = Zero
one = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine

--Dedekind axioms :equality is reflexive, symmetric and transitive

infinity = S infinity

instance Eq NaturalNumber where
  Zero == Zero = True
  Zero == S y  = False
  S x  == Zero = False
  S x  == S y  = x == y

--Exercise 2.1: Haskell contains an Ord class, consisting of types with a natural trichotomous ordering. Provide an instance declaration that adds NaturalNumber to the Ord class.

instance Ord NaturalNumber where
  compare Zero Zero = EQ
  compare Zero _ = LT
  compare _ Zero = GT
  compare (S x) (S y) = compare x y

--Exercise 2.2: Complete the definition of instance Nat NaturalNumber by implementing -, abs, and signnum. Note that for natural numbers, subtraction is truncated at Zero, i.e., evaluating one - ten should return Zero.

instance Num NaturalNumber where
  x + Zero = x -- Identity of addition
  x + (S y) = S (x + y)

  x * Zero = Zero -- Identity of multiplication
  x * (S y) = x + x * y

  Zero - Zero = Zero
  (S x) - Zero = S x
  Zero - (S x) = S x
  (S x) - (S y)
      | x == y = Zero
      | x < y = Zero
      | otherwise = x - y

  fromInteger n
        | n > 0 = S (fromInteger (n-1))
        | n == 0 = Zero
  
  signum Zero = 0
  signum _ = one

  abs = id

nat x = x + Zero

--Exercise 2.3. The axiom for the successor case of multiplication in the Wikipedia article is not what I remembered from graduate school. The crux of the matter is whether you consider S x = x + 1, or S x = 1 + x, and the Wikipedia article assumes the second. If we assume the first, as I remember it, the natural definition for the successor case of multiplication is subtly different:

-- x * S y = x * y + x

--The form that I remember is better Haskell, in the sense that it's lazier. Illustrate this by a simple example, as above.

\end{code}