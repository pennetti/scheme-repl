{-Main.hs-}

module Main where

--------------------------------------------------------------------------------
-- |Import
import LispVal
import LispEval
import System.Environment
import Control.Monad
-- |End Import
--------------------------------------------------------------------------------

-- |This is the main function
-- Program takes a single command-line argument which is the string to be
-- parsed via 'readExpr' and evalutated
main :: IO()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
	