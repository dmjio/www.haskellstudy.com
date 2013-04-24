\begin{code}
{- The basic functionality of a hypothetical RPN calculator with infinite stack -}
module Calc (CalcOp, kEnter,kAdd,kSub,kMul,kDiv,kSqrt,kSwap,kDup,perform, kSto, kRec) where

import State

data InternalState = InternalState {
    stack :: [Double],
    memory :: Double
}

type CalcState = State InternalState
type CalcOp = CalcState ()

{- private functions -}
pop :: CalcState Double
pop = do
    istate <- get
    case stack istate of
        [] -> return 0.0
        x:xs -> do
            put $ istate { stack = xs }
            return x

push :: Double -> CalcState ()
push d = do
    istate <- get
    put $ istate { stack = d : stack istate }

unop :: (Double -> Double) -> CalcOp
unop op = do
    x <- pop
    push $ op x

binop :: (Double -> Double -> Double) -> CalcOp
binop op = do
    y <- pop
    x <- pop
    push $ op x y

{- exported calculator operations -}
kEnter :: Double -> CalcOp
kEnter = push

kAdd, kSub, kMul, kDiv :: CalcOp
kAdd = binop (+)
kSub = binop (-)
kMul = binop (*)
kDiv = binop (/)

kSqrt :: CalcOp
kSqrt = unop sqrt

kSin,kCos,kTan :: CalcOp
kSin = unop sin
kCos = unop cos
kTan = unop tan

{- exported stack operations -}
kSwap :: CalcOp
kSwap = do
    y <- pop
    x <- pop
    push y
    push x

kDup :: CalcOp
kDup = do
    x <- pop
    push x
    push x

kSto :: Double -> CalcOp
kSto val = do
  istate <- get
  put $ istate { stack = stack istate, memory = val }

getMem :: InternalState -> Double
getMem (InternalState _ mem) = mem
  
kRec :: CalcOp
kRec = do
  istate <- get
  put $ istate { stack = (getMem istate):stack istate }

{- execution of a calculator program -}
perform :: CalcState a -> Double
perform ma = head $ stack $ snd $ runState (ma >> pop >>= push) startState where
    startState = InternalState [] 0.0
\end{code}