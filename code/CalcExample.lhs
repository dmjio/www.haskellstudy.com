\begin{code}
module Main where

import Calc

square :: CalcOp
square = do
    kDup
    kMul

hypotenuse :: CalcOp
hypotenuse = do
    square
    kSwap
    square
    kAdd
    kSqrt

test :: Double
test = perform $ do
    kEnter 3
    kEnter 4
    hypotenuse


testAdd :: Double
testAdd = perform $ do
  kEnter 3
  kSto 4 --stores the value in memory
  kRec --recalls the value from memory and places it on the top of the Stack
  kAdd
\end{code}            