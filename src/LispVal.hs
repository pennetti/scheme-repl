{-Parser.hs-}

module LispVal where

--------------------------------------------------------------------------------
-- |Import
-- |Using the Parsec parsing library
-- |Hide the function 'spaces' because it will be overridden.
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Control.Monad
import Data.IORef
-- |End Import
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- |Data Types
-- |Algebraic data type (possible values) to hold any lisp value
data LispVal = Atom String				-- String naming the atom
		| List [LispVal]				-- List of LispVals, indicated by []
		| DottedList [LispVal] LispVal	-- Scheme form (a b . c)
		| Number Integer
		| String String
		| Bool Bool
		| PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
		| Func {params :: [String]
				, vararg :: (Maybe String)
				, body :: [LispVal]
				, closure :: Env}
-- |Data type to hold various error types
data LispError = NumArgs Integer [LispVal]
        | TypeMismatch String LispVal
        | Parser ParseError
        | BadSpecialForm String LispVal
        | NotFunction String String
        | UnboundVar String String
        | Default String
-- |End Data Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LispVal
--------------------------------------------------------------------------------

-- |Parser which recognizes one of the symbols allowed in Scheme identifiers.
symbol :: Parser Char
-- oneOf function from Parsec library matches one symbol in input string
symbol = oneOf "!$%&|*+-/:<=>?@^_~"	-- Symbols allowed in Scheme identifiers

-- |Call parser, handle errors
readExpr :: String -> ThrowsError LispVal
-- 'parse' returns an 'Either' data type Left (error) and Right (value)
-- Use 'parseExpr' on 'input'
readExpr input = case parse parseExpr "lisp" input of 
	Left err -> throwError $ Parser err
	Right val -> return val

-- |Parser which recognizes one or more spaces
spaces :: Parser ()
spaces = skipMany1 space	-- skipMany1 allows the parser to recognize one or 
-- more spaces

-- |String parser
-- |String identified by opening and closing quotation marks
parseString :: Parser LispVal
parseString = do 
			char '"'	-- Opening quotation
			x <- many (noneOf "\"")	-- Make sure no quotations in string
			char '"'	-- Closing quotation
			return $ String x	-- Use String constructor to convert to LispVal

{-NOTE: Instroduction of '<|>' operator tries first parser, and if that fails 
then try the next parser.-}
-- |Atom parser
-- |An atom is a letter or symbol, followed by any number of letters, 
-- |digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do 
			first <- letter <|> symbol 	-- Can't start with digit
			rest <- many (letter <|> digit <|> symbol)
			let atom = [first] ++ rest	-- Convert first to a singleton [first]
			return $ case atom of
				"#t" -> Bool True
				"#f" -> Bool False
				otherwise -> Atom atom

-- |Number parser
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit	-- many1 matches one or 
-- more of its args, in this case a digit.  'liftM' takes the value from the 
-- Parser String monad returned by 'many1 digit'

-- |List parser
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces	-- Parse expressions separated 
-- by whitespace

-- |DottedList parser
parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr	-- (char '.' >> spaces) returns a 
	-- Parser () combined with the string from 'parseExpr' to make a LispVal
	return $ DottedList head tail

-- |Parse single-quote syntactic sugar from Scheme
parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote", x]

-- |Choose the appropriate parser based on input. (<|> indicates go to next 
-- |parser if the first one fails).
parseExpr :: Parser LispVal
parseExpr = parseAtom
		<|> parseString
		<|> parseNumber
		<|> parseQuoted
		<|> do 
			char '('
			x <- (try parseList) <|> parseDottedList	-- 'try' backtracks
			char ')'
			return x

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " 
								++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args
				, vararg = varargs
				, body = body
				, closure = env}) = 
			"(lambda (" ++ unwords (map show args) ++
				(case varargs of
					Nothing -> ""
					Just arg -> " . " ++ arg) ++ ") ...)"

-- |Turns list into a string representation of list elements, point-free style
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal	-- Define the show function for the 
-- LispVal data type

--------------------------------------------------------------------------------
-- LispError
--------------------------------------------------------------------------------

-- |Partially applied, can be used on any data type
type ThrowsError = Either LispError 	-- Define type of function which may 
-- throw a LispError

-- |Error handler which displays relevant error message
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
					++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " 
					++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

-- |Convert errors into strings and return them, returns an Either (Right valid)
trapError action = catchError action (return . show)

-- |Extract error from right Either value returned from 'trapError'
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- No Left value, since errors only return Right values

instance Show LispError where show = showError	-- Define the show function for 
-- the LispError data type

instance Error LispError where	-- Make LispError an instance of Error
	noMsg = Default "An error has occurred"	
	strMsg = Default

--------------------------------------------------------------------------------
-- LispVar
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

-- |Check if given variable has been bounded to environment
isBound :: Env -> String -> IO Bool
-- Extract variable, then look it up to see if it exists, lift into IO monad
-- 'const' is used because a function is expected, not a value
isBound envRef var = 
			readIORef envRef >>= return . maybe False (const True) . lookup var

-- |Retrieve current value of variable
-- Get environment from IORef, use IOThrowsError monad for error handling
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
					env <- liftIO $ readIORef envRef -- Get environment
					maybe 	(throwError $ UnboundVar "Getting an unbound variable" var)
							(liftIO . readIORef)
							(lookup var env)

-- |Set current variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = 	do
							env <- liftIO $ readIORef envRef -- Get environment
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