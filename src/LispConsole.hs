{-REPL.hs-}

module LispConsole where

--------------------------------------------------------------------------------
-- |Import
import System.IO
import LispEval
import LispVal
import Control.Monad.Error
import Control.Monad
-- |End Import
--------------------------------------------------------------------------------

-- |Print out stream, then flush the stream (clear output buffer)
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Print prompt, then read line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- |Pull code to parse and evaluate, trap errors in their own function
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- |Evaluate a string and print the result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- |Infinite loop to read string, perform function, produce output;
-- |until_ takes a predicate that signals when to stop, an action to perform 
-- |before the test, and a function-returning-an-action to do to the input
-- An underscore is used in the function name to indicate that it is a monadic 
-- function that does not return a value
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
				result <- prompt
				if pred result
					then return () -- Empty
					else action result >> until_ pred prompt action -- Repeat

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
	where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

-- |Initialize environment with null variable at start
runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

-- |The REPL (Read Eval Print Loop) until "quit" is entered
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint