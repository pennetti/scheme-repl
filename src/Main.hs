module Main where

import Parser
import System.Environment

-- |This is the main function
main :: IO()
main = do 
	args <- getArgs
	putStrLn (readExpr (args !! 0))
	