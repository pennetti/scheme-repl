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
    if null args then runRepl else runOne $ args