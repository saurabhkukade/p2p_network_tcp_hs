module Main where

import System.Environment (getArgs)
import Server

main :: IO ()
main = do
    [port] <- getArgs
    runServer (read $ port :: Int)
