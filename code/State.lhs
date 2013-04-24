\begin{code}
{- A pedagogical implementation of the standard State monad -}

module State (State, state, runState, get, put) where

data State s a = State {
    runState :: s -> (a,s)
} 

instance Monad (State s) where
    ma >>= g = State $ \s -> let (a,b) = runState ma s
                             in runState (g a) b
    return a = State $ \s -> (a,s)

{- constructor -}

state :: (s -> (a,s)) -> State s a
state = State

{- primitive state manipulation functions -}

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put = \t -> State $ \s -> ((),t)

{- ma >>= g = State $ (\(a,s) -> runState (g a) s) . runState ma -}
{- Show how to derive the defintion for (>>=) above from the original defintion -}

{-

Dave:

First, we need to examine definition of bind (>>=) which is 
(>>=) :: (m a -> (a -> m b) -> m b)

Which in our case is representative of the following (State s is the type (m a))
(>>=) :: (State s) a -> (a -> (State s) b) -> (State s) b

The (State s a) data (or newtype) declaration is just a wrapper for the function
(s -> (a,s)). So if we substitute that in for the State data decalaration we get:

             (m a)    ->     (a -> m b)    ->    (m b)
(>>=) :: (s -> (a,s)) -> (a -> s -> (b,s)) -> (s -> (b,s))

One other definition we will need to discuss is the definition of runState
which is runState :: State s a -> s -> (a, s)

In order to derive {- ma >>= g = State $ (\(a,s) -> runState (g a) s) . runState ma -}
from "ma >>= g = State $ \s -> let (a,b) = runState ma s in runState (g a) b" we first need to understand "ma >>= g = State $ \s -> let (a,b) = runState ma s in runState (g a) b"

The main function to understand is (runState). (runState ma s) produces a value and a state value (a,b). 
This tuple (a,b) is passed into another runState which applies the function (g) to 'a' to alter the state of our statevalue 'a'. The result of "runState (g a) s" is a tuple that is wrapped in a State function. 

Now that we understand "ma >>= g = State $ \s -> let (a,b) = runState ma s in runState (g a) b" we can nowattempt to derive "ma >>= g = State $ (\(a,s) -> runState (g a) s) . runState ma" from it.

We know the definition for (.) to be :: (b -> c) -> (a -> b) -> a -> c
We also know the definition for the lambda expression  (\(a,s) -> runState (g a) s) to be ::
 (State s a, s) -> (a, s)

(runState ma) curries runState to yield a function with the type:: s -> (a,s)

Since (runState ma) returns a type (a,s) and the lambda expression defined above takes arguments (a,s) as its parameters the can be composed with (.)

The type signature (s -> (a,s)) is our (a -> b) in the context of (.). The parameter "b" is (a,s) which is then passed into the lambda function "(\(a,s) -> runState (g a) s)" (which represents our (b -> c) in the context of (.)). This function applies the function "g" to intermediate state value 'a' and returns a function (s -> (a,s)) which is eligible to be wrapped into a new state with the State data constructor. This new state can subsequently be sequenced and altered further with bind.

q.e.d ? :)

-}
\end{code}