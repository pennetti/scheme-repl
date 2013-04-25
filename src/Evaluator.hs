{-Evaluator.hs-}

module Evaluator where

--------------------------------------------------------------------------------
-- |Import
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Control.Monad
import Parser
-- |End Import
--------------------------------------------------------------------------------

-- |Map code data type to 'data' data type, both are LispVals
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val 	-- Bind 'val' to String
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
											result <- eval pred
											case result of
												Bool False -> eval alt
												otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- |Function to apply arguments for functions
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction 
							"Unrecognized primitive function args" func)
						($ args)
						(lookup func primitives)
--(Bool False) ($ args) $ lookup func primitives

-- |Primitives returns a list of pairs for 'lookup', key is String
-- |value is [LispVal] -> LispVal
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [	-- Number Operators --
				("+", numericBinop (+)),
				("-", numericBinop (-)),
				("*", numericBinop (*)),
				("/", numericBinop div),
				("mod", numericBinop mod),
				("quotient", numericBinop quot),
				("remainder", numericBinop rem),
				-- Equality Operators --
				("=", numBoolBinop (==)),
	        	("<", numBoolBinop (<)),
	        	(">", numBoolBinop (>)),
				("/=", numBoolBinop (/=)),
				(">=", numBoolBinop (>=)),
				("<=", numBoolBinop (<=)),
				-- Boolean Operators --
				("&&", boolBoolBinop (&&)),
				("||", boolBoolBinop (||)),
				("string=?", strBoolBinop (==)),
				("string?", strBoolBinop (>)),
				("string<=?", strBoolBinop (<=)),
				("string>=?", strBoolBinop (>=))]

-- |Numeric binary operator 
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- The foldl1 function converts prefix notation to infix for evaluation

-- |Equality Boolean operator
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
							then throwError $ NumArgs 2 args
							else do 
									left <- unpacker $ args !! 0
									right <- unpacker $ args !! 1
									return $ Bool $ left `op` right
-- |Apply boolBinop with necessary unpacker
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- |Unpacks argument list and applies the respective function to it
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 	-- Use weak-typing
						if null parsed
							then throwError $ TypeMismatch "number" $ String n
							else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool