{-# LANGUAGE ExistentialQuantification #-}	--'forall'
{-Evaluator.hs-}

module LispEval where

--------------------------------------------------------------------------------
-- |Import
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import Control.Monad.Error
import Control.Monad
import LispVal
--import LispIO
-- |End Import
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- |Data Types
-- |Data type to allow for existential types (heterogeneous lists)
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
-- |End Data Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LispEval
--------------------------------------------------------------------------------

-- |Map code data type to 'data' data type, both are LispVals
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val 	-- Bind 'val' to LispVal
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
											result <- eval env pred
											case result of
												Bool False -> eval env alt
												otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    										eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    										eval env form >>= defineVar env var										
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
	makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = 
	makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env (List [Atom "load", String filename]) = 
	load filename >>= liftM last . mapM (eval env)
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- |Function to apply arguments for functions
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 
--(Bool False) ($ args) $ lookup func primitives

-- |Primitives returns a list of pairs for 'lookup', key is String
-- |value is [LispVal] -> LispVal
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [	
		-- Numeric Operators --
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
		("string>=?", strBoolBinop (>=)),
		-- List Operators --
		("car", car),
      	("cdr", cdr),
      	("cons", cons),
      	-- Equivalence Operators --
      	("eq?", eqv),
      	("eqv?", eqv),
      	("equal?", equal)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
	where makeFunc constructor (var, func) = (var, constructor func)

-- |Numeric binary operator 
-- |Changes every cons operator in the list to the binary function we supply, op
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
-- |Partially apply boolBinop with necessary unpacker
numBoolBinop 	= boolBinop unpackNum
strBoolBinop 	= boolBinop unpackStr
boolBoolBinop	= boolBinop unpackBool

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

--------------------------------------------------------------------------------
-- LispList
--------------------------------------------------------------------------------

-- |'car' returns the first element of a list (head)
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- |'cdr' returns all elements but the first (tail)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- |'cons' takes two parameters and makes a list from them (:)
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x] -- One item list
cons [x, List xs] = return $ List $ [x] ++ xs	-- Appending to a list
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- |'eqv' equivalence function
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
						(and $ map eqvpair $ zip arg1 arg2)
	where eqvpair (x1, x2) = case eqv [x1, x2] of
						Left err -> False
						Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- |Use unpacker on two value and see if they are equal (helper function)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
					unpacked1 <- unpacker arg1
					unpacked2 <- unpacker arg2
					return $ unpacked1 == unpacked2
			`catchError` (const $ return False)

-- |Use 'unpackEquals' to try all unpackers on each value (ignore type diff)
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
	primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
		[AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
	eqvEquals <- eqv [arg1, arg2]
	return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

--------------------------------------------------------------------------------
-- LispFunc
--------------------------------------------------------------------------------

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

--------------------------------------------------------------------------------
-- LispIO
--------------------------------------------------------------------------------

{-LispIO.hs-}

--module LispIO where

--import LispVal
--import System.IO

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc)
				, ("open-input-file", makePort ReadMode)
				, ("open-output-file", makePort WriteMode)
				, ("close-input-port", closePort)
				, ("close-output-port", closePort)
				, ("read", readProc)
				, ("write", writeProc)
				, ("read-contents", readContents)
				, ("read-all", readAll)]

-- |Wrapper for 'apply'
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

-- |Wrapper for 'openFile'
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- |Wrapper for 'hClose'
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

-- |Wrapper for 'hGetLine
readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

-- |Wrapper for 'hPrint'
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- |Wrapper for 'readFile',  Read file into memory as a string
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- |Read and parse a file full of statements.  It's used in two places: readAll 
-- |(which returns a list of values) and load (which evaluates those values as 
-- |Scheme expressions).
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- |Wrap the return value from 'load' with a list constructor
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

