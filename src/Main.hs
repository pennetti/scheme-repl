{-Main.hs-}

module Main where

--------------------------------------------------------------------------------
-- |Import
import LispVal
import LispEval
import LispConsole
import System.Environment
import Control.Monad
-- |End Import
--------------------------------------------------------------------------------

-- |This is the main function
main :: IO ()
main = do
    args <- getArgs
    case length args of
    	0 -> runRepl
    	1 -> runOne $ args !! 0
    	otherwise -> putStrLn "Program takes only 0 or 1 argument"
	