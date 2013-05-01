{-LispVar.hs-}

module LispVar where 	

--------------------------------------------------------------------------------
-- |Import
import Control.Monad
import Control.Monad.Error
import Data.IORef
import LispVal
-- |End Import
--------------------------------------------------------------------------------

-- IORef which holds a list that maps String to mutable LispVals
type Env = IORef [(String, IORef LispVal)]	
-- |Combined monad for error handling
type IOThrowsError = ErrorT LispError IO

-- |Helper function to create an empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- |Bring functions of lower monad type (IO) into the combined monad (lifting)
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err 	-- Rethrow error
liftThrows (Right val) = return val

-- |Run top level IOThrowsError action and return IO action
runIOThrows :: IOThrowsError String -> IO String
-- Use trapError to turn errors into strings
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- |Check if given variable has been bounded
isBound :: Env -> String -> IO Bool
-- Extract variable, then look it up to see if it exists, lift into IO monad
-- 'const' is used because a function is expected, not a value
isBound envRef var = 
			readIORef envRef >>= return . maybe False (const True) . lookup var

-- |Retrieve current value of variable
-- Get environment from IORef, use IOThrowsError monad for error handling
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
					env <- liftIO $ readIORef envRef
					maybe 	(throwError $ UnboundVar "Getting an unbound variable" var)
							(liftIO . readIORef)
							(lookup var env)

-- |Set current variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = 	do
							env <- liftIO $ readIORef envRef
							maybe 	(throwError $ UnboundVar "Setting an unbound variable" var)
									(liftIO . (flip writeIORef value))
									(lookup var env)
							return value

-- |Define a variable
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =	do						
								alreadyDefined <- liftIO $ isBound envRef var
								if alreadyDefined
									then setVar envRef var value >> return value
									else liftIO $ 	do
													valueRef <- newIORef value
													env <- readIORef envRef
													writeIORef envRef ((var, valueRef) : env)
													return value

-- |Function to bind multiple variables at once
{-bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
			-- Calls addBinding on each member of bindings (mapM) to create 
			-- list of (String, IORef LispVal) pairs, and then appends the 
			-- current environment to the end of that (++ env)
	where 	extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
			-- Create an IORef to hold new variable, then return (name, value) pair
			addBinding (var, value) = 	do
										ref <- newIORef value
										return (var, ref)-}

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)										