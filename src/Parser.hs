module Parser where

-- |Import
-- |Hide the function 'spaces' because it will be overridden.
import Text.ParserCombinators.Parsec hiding (spaces)

-- |End Import

-- |Parser which recognizes one of the symbols allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- |Call parser, handle errors
readExpr :: String -> String
-- |'parse' returns an 'Either' data type Left (error) and Right (value)
readExpr input = case parse symbol "lisp" input of 
	Left err -> "No match: " ++ show err
	Right value -> "Found value"