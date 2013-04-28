{-Parser.hs-}

module LispVal where

--------------------------------------------------------------------------------
-- |Import
-- |Using the Parsec parsing library
-- |Hide the function 'spaces' because it will be overridden.
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Control.Monad
-- |End Import
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- |Data Types
-- |Data type to hold any lisp value
data LispVal = Atom String		-- String naming the atom
		| List [LispVal]	-- List of LispVals, indicated by []
		| DottedList [LispVal] LispVal		-- Scheme form (a b . c),
		-- last value of list is stored separately
		| Number Integer
		| String String
		| Bool Bool
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

{-NOTE: We're back to using the do-notation instead of the >> operator. This is 
because we'll be retrieving the value of our parse (returned by many 
(noneOf "\"")) and manipulating it, interleaving some other parse 
operations in the meantime. In general, use >> if the actions don't return a 
value, >>= if you'll be immediately passing that value into the next 
action, and do-notation otherwise.-}
-- |String parser
-- |String identified by opening and closing quotation marks
parseString :: Parser LispVal
parseString = do 
			char '"'	-- Opening quotation
			x <- many (noneOf "\"")	-- Make sure no quotations in string
			char '"'	-- Closing quotation
			return $ String x	-- Use String constructor to convert to LispVal

{-NOTE: Instroduction of '<|>' operator tries first parser, and if that fails then
try the next parser.-}
-- |Atom parser
-- |An atom is a letter or symbol, followed by any number of letters, 
-- |digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do 
			first <- letter <|> symbol
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
			x <- (try parseList) <|> parseDottedList
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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal	-- Define the show function for the 
-- LispVal data type

--------------------------------------------------------------------------------
-- LispError
--------------------------------------------------------------------------------

instance Show LispError where show = showError	-- Define the show function for 
-- the LispError data type

instance Error LispError where	-- Make LispError an instance of Error
	noMsg = Default "An error has occurred"	
	strMsg = Default

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

-- |Convert errors into strings and return them
trapError action = catchError action (return . show)

-- |Extract error
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- No Left value, since errors only return Right values
