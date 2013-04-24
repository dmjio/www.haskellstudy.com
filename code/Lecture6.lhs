\begin{code}
module Main where

import Data.Char
import System.Environment
import System.Exit
import System.IO


--Exercise 6.1. Modify the hello program so that it capitalizes the user's name. 
--Compile and run your program, and provide a sample interaction.

user :: IO()
user = do
  user <- getEnv "USER"
  putStrLn $ msg user

msg :: String -> String
msg [] = []
msg (x:xs) = "Hello " ++ toUpper x : xs

--Exercise 6.2. Complete the implementation of rot, compile, and run ./rot 3 < rot.hs. 
--Turn in your program, both in cleartext and in cyphertext.

rot13 c num = r c num where
    r 0 c  = c
    r number c = r (number - 1) (rotate c)

rotate :: Char -> Char --a little different and more inefficient, but more concise
rotate c
    | isLower c = if c == 'z' then 'a' else succ c
    | isUpper c = if c == 'Z' then 'A' else succ c
    | otherwise = c

rot :: Int -> String -> String
rot num = map $ rot13 num

rot_stdin :: Int -> IO()
rot_stdin n = interact $ rot n

usage :: IO ()
usage = do
  progname <- getProgName
  hPutStrLn stderr $ "usage: " ++ progname ++ " n"
  exitWith $ ExitFailure 255

main :: IO () -- ./rot 3 < Lecture6.hs works
main = do
  args <- getArgs
  case args of
    [] -> rot_stdin 13
    [x] ->  if (not (null x)) && all isDigit x 
            then rot_stdin (read x) 
            else usage
    _ -> usage
\end{code}