module Main where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (handle)
import qualified Data.ByteString.Lazy as L
import Control.Monad (forever)
import Codec.Compression.GZip (compress)
import System.Directory 


-- | This is code to compress a file using multiple threads
main = do
  putStrLn "Enter a file to compress: "
  fileName <- getLine
  exists <- doesFileExist fileName
  case exists of
    False -> putStrLn "File doesn't exist"
    otherwise -> do
         content <- L.readFile fileName
         forkIO (compressFile fileName content) --spawn a new thread for compression
         return ()
  where compressFile path = L.writeFile (path ++ ".gz") . compress


--Haskell users MVar for threads to communicate to one another.. 
--data is immutable so it poses no risk

--An MVar acts like a single-element box: it can be either full or empty. We can put something into the box, making it full, or take something out, making it empty

communicate = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn $ "received " ++ show v
  putStrLn "sending"
  putMVar m "wake up!"



