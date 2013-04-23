module Parser where

-- |Import
-- |Hide the function 'spaces' because it will be overridden.
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

-- |End Import

-- |Data Types
-- |Data type to hold any lisp value
data LispVal = Atom String		-- String naming the atom
			| List [LispVal]	-- List of LispVals
			| DottedList [LispVal] LispVal		-- Scheme form (a b . c),
			-- last value of list is stored separately
			| Number Integer
			| String String
			| Bool Bool

-- |End Data Types

-- |Parser which recognizes one of the symbols allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- |Call parser, handle errors
readExpr :: String -> String
-- |'parse' returns an 'Either' data type Left (error) and Right (value)
readExpr input = case parse parseExpr "lisp" input of 
	Left err -> "No match: " ++ show err
	Right _ -> "Found value"

-- |Parser which recognizes one or more spaces
spaces :: Parser ()
spaces = skipMany1 spaces	-- skipMany1 allows the parser to recognize one or 
-- more spaces

{-We're back to using the do-notation instead of the >> operator. This is 
because we'll be retrieving the value of our parse (returned by many 
(noneOf "\"")) and manipulating it, interleaving some other parse 
operations in the meantime. In general, use >> if the actions don't return a 
value, >>= if you'll be immediately passing that value into the next 
action, and do-notation otherwise.-}
parseString :: Parser LispVal
parseString = do char '"'	-- Opening quotation
			x <- many (noneOf "\"")	-- Make sure no quotations in string
			char '"'		-- Closing quotation
			return $ String x	-- Use String constructor to convert to LispVal

{-Instroduction of '<|>' operator tries first parser, and if that fails then
try the next parser.-}
parseAtom :: Paraser LispVal
parseAtom = do first <- letter <|> symbol
			rest <- many (letter <|> digit <|> symbol)
			let atom = [first] ++ rest	-- Convert first to a singleton [first]
			return $ case atom of
				"#t" -> Bool True
				"#f" -> Bool False
				otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit	-- many1 matches one or 
-- more of its args, in this case a digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
		<|> parseString
		<|> parseNumber

