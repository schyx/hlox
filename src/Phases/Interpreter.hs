{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Phases.Interpreter (
  runInterp,
  interpret,
  interpretExpr,
  defaultInterpreter,
  Interpreter,
) where

import Control.Monad (foldM, foldM_, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState (get, put), StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error
import Numeric (showFFloat)
import Phases.Expr
import Phases.Resolver (Locals (resolverMap))
import Phases.Stmt
import System.IO (hPutStrLn, stderr)
import Tokens

runInterp :: Interpreter -> SomeStmt -> IO (Either String Interpreter)
runInterp startInterpreter stmt = do
  intermediate <- runExceptT $ runStateT (runExceptT runStmt) startInterpreter
  case intermediate of
    Left err -> do
      hPutStrLn stderr err
      return $ Left err
    Right (_, interpreter) -> return $ Right interpreter
 where
  runStmt :: InterpreterOutput ()
  runStmt = do
    interpret stmt

type InterpreterOutput a = ExceptT SomeValue (StateT Interpreter (ExceptT String IO)) a

throwRuntimeError :: String -> InterpreterOutput a
throwRuntimeError = lift . throwError

interpret :: SomeStmt -> InterpreterOutput ()
interpret (SomeStmt (Print expr)) = interpretExpr expr >>= liftIO . print
interpret (SomeStmt (Expression expr)) = void $ interpretExpr expr
interpret (SomeStmt (Var name initializer)) = interpretExpr initializer >>= define name
interpret (SomeStmt (Block stmts)) = createChildEnv >> execBlock stmts >> changeToParent
interpret (SomeStmt (If condition ifBranch (Just elseBranch))) = do
  value <- interpretExpr condition
  interpret (if isTruthy value then ifBranch else elseBranch)
interpret (SomeStmt (If condition ifBranch Nothing)) = do
  value <- interpretExpr condition
  when (isTruthy value) (interpret ifBranch)
interpret stmt@(SomeStmt (While condition whileBlock)) = do
  value <- interpretExpr condition
  when (isTruthy value) (interpret whileBlock >> interpret stmt)
interpret (SomeStmt function@(Function functionName _ _)) = functionToValue False function >>= define functionName . SomeValue
interpret (SomeStmt (Return _ Nothing)) = throwError $ SomeValue VNil
interpret (SomeStmt (Return _ (Just value))) = interpretExpr value >>= throwError
interpret (SomeStmt (Class name superclass classMethods)) = do
  superclassValue <- case superclass of
    Nothing -> return Nothing
    Just superclassExpr@(Variable superclassName) -> do
      value <- interpretExpr superclassExpr
      case value of
        (SomeValue classValue@VClass{}) -> return $ Just classValue
        _ -> throwRuntimeError $ runtimeError superclassName "Superclass must be a class."
    _ -> error "TODO: GADTs for Exprs"
  define name $ SomeValue VNil
  createChildEnv
  klass <- getKlass superclassValue
  changeToParent
  assignTok name klass
 where
  getKlass :: Maybe (Value 'ValueClass) -> InterpreterOutput SomeValue
  getKlass superclassValue = do
    methods <- getMethods
    let arity = case methods Map.!? "init" of
          Nothing -> 0
          Just (VFunction numArgs _ _ _ _ _ _) -> length numArgs
    return $ SomeValue $ VClass (lexeme name) superclassValue arity methods
  foldMFunc :: Map.Map String (Value 'ValueFunction) -> Stmt 'KFunction -> InterpreterOutput (Map.Map String (Value 'ValueFunction))
  foldMFunc buildup method@(Function methodName _ _) = do
    methodObject <- functionToValue (lexeme methodName == "init") method
    return $ Map.insert (lexeme methodName) methodObject buildup
  getMethods :: InterpreterOutput (Map.Map String (Value 'ValueFunction))
  getMethods = foldM foldMFunc Map.empty classMethods

functionToValue :: Bool -> Stmt 'KFunction -> InterpreterOutput (Value 'ValueFunction)
functionToValue isInitializer (Function functionName functionParameters body) = do
  definingInterpreter <- get
  let functionF :: [SomeValue] -> StateT Interpreter (ExceptT String IO) SomeValue
      functionF args = do
        restoreRunningEnv definingInterpreter
        createChildEnv
        foldM_ (\_ (token, value) -> define token value) () (zip functionParameters args)
        run <- runExceptT $ execBlock body
        case run of
          Left value -> return value
          Right () -> return $ SomeValue VNil
   in addFunction
        functionParameters
        functionName
        ("<fn " ++ lexeme functionName ++ ">")
        isInitializer
        (currentEnvironment definingInterpreter)
        functionF

execBlock :: [SomeStmt] -> InterpreterOutput ()
execBlock = foldM_ (\_ stmt -> interpret stmt) ()

interpretExpr :: Expr -> InterpreterOutput SomeValue
interpretExpr (Call callee leftParenthesis argumentExpressions) = do
  interpreter <- get
  value <- interpretExpr callee
  arguments <- interpretExprs argumentExpressions
  case value of
    (SomeValue function@VFunction{}) -> callFunction interpreter arguments function
    (SomeValue loxClass@(VClass className _ _ classMethods)) -> do
      inst <- newInstance className loxClass
      case classMethods Map.!? "init" of
        Nothing ->
          if null arguments
            then return $ SomeValue inst
            else throwRuntimeError $ runtimeError leftParenthesis $ "Expected 0 arguments but got " ++ show (length arguments) ++ "."
        Just initializer@(VFunction _ _ _ _ functionEnvironment _ _) -> do
          assignThis inst functionEnvironment
          callFunction interpreter arguments initializer
    _ -> throwRuntimeError $ runtimeError leftParenthesis "Can only call functions and classes."
 where
  callFunction :: Interpreter -> [SomeValue] -> Value 'ValueFunction -> InterpreterOutput SomeValue
  callFunction interpreter argument (VFunction parameters _ _ isInitializer _ function _) =
    if length parameters == length argument
      then do
        outputValue <- lift $ function argument
        output <- if isInitializer then getAt 1 "this" else return outputValue
        restoreRunningEnv interpreter
        return output
      else
        throwRuntimeError
          $ runtimeError
            leftParenthesis
          $ "Expected " ++ show (length parameters) ++ " arguments but got " ++ show (length argument) ++ "."
  interpretExprs :: [Expr] -> InterpreterOutput [SomeValue] -- TODO maybe try using fold?
  interpretExprs [] = return []
  interpretExprs (expr : exprs) = do
    value <- interpretExpr expr
    values <- interpretExprs exprs
    return $ value : values
interpretExpr expr@(Assign name assigningExpression) = do
  value <- interpretExpr assigningExpression
  assign expr name value
  return value
interpretExpr (Binary left operator right) = do
  leftValue <- interpretExpr left
  rightValue <- interpretExpr right
  getOutput leftValue rightValue
 where
  getOutput :: SomeValue -> SomeValue -> InterpreterOutput SomeValue
  getOutput leftValue rightValue
    | tokenType operator `elem` [BANG_EQUAL, EQUAL_EQUAL] =
        return $
          SomeValue $
            VBoolean
              ( if tokenType operator == EQUAL_EQUAL
                  then leftValue == rightValue
                  else leftValue /= rightValue
              )
    | tokenType operator == PLUS = plusOperator leftValue rightValue operator
    | Map.member (tokenType operator) numericBinaryTable = do
        (leftNumber, rightNumber) <- toNumberPair leftValue rightValue operator
        return $ SomeValue $ VNumber $ (numericBinaryTable Map.! tokenType operator) leftNumber rightNumber
    | otherwise = do
        (leftNumber, rightNumber) <- toNumberPair leftValue rightValue operator
        return $ SomeValue $ VBoolean $ (booleanBinaryTable Map.! tokenType operator) leftNumber rightNumber
  booleanBinaryTable = Map.fromList [(LESS, (<)), (LESS_EQUAL, (<=)), (GREATER, (>)), (GREATER_EQUAL, (>=))]
  numericBinaryTable = Map.fromList [(STAR, (*)), (SLASH, (/)), (MINUS, (-))]
interpretExpr (Unary operator expr) = interpretExpr expr >>= unaryOpTable Map.! tokenType operator
 where
  unaryOpTable =
    Map.fromList
      [ (BANG, return . SomeValue . VBoolean . not . isTruthy)
      , (MINUS, \val -> SomeValue . VNumber . (* (-1)) <$> toNumber val operator)
      ]
interpretExpr (Grouping expr) = interpretExpr expr
interpretExpr expr@(Variable token) = lookupVariable token expr
interpretExpr (AndExpr left _ right) = do
  value <- interpretExpr left
  if not $ isTruthy value
    then return value
    else interpretExpr right
interpretExpr (OrExpr left _ right) = do
  value <- interpretExpr left
  if isTruthy value
    then return value
    else interpretExpr right
interpretExpr (Primary singleLiteral) = return $ fromLiteral singleLiteral
interpretExpr (Get object name) = interpretExpr object >>= getFieldOrMethod name
interpretExpr (Set object name value) = do
  interpretedObject <- interpretExpr object
  case interpretedObject of
    SomeValue (VInstance _ _ _ iid) -> do
      interpretedValue <- interpretExpr value
      setProperty iid name interpretedValue
      return interpretedValue
    _ -> throwRuntimeError $ runtimeError name "Only instances have fields."
interpretExpr expr@(This keyword) = lookupVariable keyword expr

getFieldOrMethod :: Token -> SomeValue -> InterpreterOutput SomeValue
getFieldOrMethod name (SomeValue inst@(VInstance _ properties loxClass _)) =
  case properties Map.!? lexeme name of
    Just value -> return value
    Nothing -> SomeValue <$> findMethod loxClass
 where
  findMethod :: Value 'ValueClass -> InterpreterOutput (Value 'ValueFunction)
  findMethod (VClass _ superclass _ methods) =
    case methods Map.!? lexeme name of
      Just (VFunction params leftParenthesis functionName isInitializer definingEnvironment function _) -> do
        assignThis inst definingEnvironment
        addFunction params leftParenthesis functionName isInitializer definingEnvironment function
      Nothing ->
        maybe
          (throwRuntimeError $ runtimeError name $ "Undefined property '" ++ lexeme name ++ "'.")
          findMethod
          superclass
getFieldOrMethod name _ = throwRuntimeError $ runtimeError name "Only instances have properties."

plusOperator :: SomeValue -> SomeValue -> Token -> InterpreterOutput SomeValue
plusOperator (SomeValue (VNumber left)) (SomeValue (VNumber right)) _ = return $ SomeValue $ VNumber $ left + right
plusOperator (SomeValue (VStr left)) (SomeValue (VStr right)) _ = return $ SomeValue $ VStr $ left ++ right
plusOperator _ _ operator = throwRuntimeError $ runtimeError operator "Operands must be two numbers or two strings."

toNumberPair :: SomeValue -> SomeValue -> Token -> InterpreterOutput (Double, Double)
toNumberPair (SomeValue (VNumber left)) (SomeValue (VNumber right)) _ = return (left, right)
toNumberPair _ _ operator = throwRuntimeError $ runtimeError operator "Operands must be numbers."

toNumber :: SomeValue -> Token -> InterpreterOutput Double
toNumber (SomeValue (VNumber number)) _ = return number
toNumber _ token = throwRuntimeError $ runtimeError token "Operand must be a number."

isTruthy :: SomeValue -> Bool
isTruthy (SomeValue VNil) = False
isTruthy (SomeValue (VBoolean b)) = b
isTruthy _ = True

--------------------- Interpreter Methods ---------------------

data Interpreter = Interpreter
  { environmentTable :: Map.Map EnvID Environment
  , locals :: Map.Map Expr Int
  , currentEnvironment :: EnvID
  , nextEnvironmentId :: EnvID
  , nextInstanceId :: InstanceID
  , funcCounter :: FuncCounter
  }
  deriving (Show)

data Environment = Environment (Map.Map String SomeValue) (Maybe EnvID)
  deriving (Show)

newtype EnvID = EnvID Int
  deriving (Show, Eq, Ord)

nextLargest :: EnvID -> EnvID
nextLargest (EnvID envId) = EnvID $ envId + 1

newtype InstanceID = InstanceID Int
  deriving (Show, Eq, Ord)

incrementInstanceId :: InstanceID -> InstanceID
incrementInstanceId (InstanceID instanceId) = InstanceID $ instanceId + 1

newtype FunctionID = FunctionID Int
  deriving (Show, Eq, Ord)

newtype FuncCounter = FuncCounter {getFuncCounter :: Map.Map ([Token], Token, String, Bool) FunctionID}
  deriving (Show)

addFunction ::
  [Token] ->
  Token ->
  String ->
  Bool ->
  EnvID ->
  ([SomeValue] -> StateT Interpreter (ExceptT String IO) SomeValue) ->
  InterpreterOutput (Value 'ValueFunction)
addFunction parameters leftParenthesis functionName isInitializer definingEnvironment function = do
  interpreter <- get
  let functionCounter = getFuncCounter . funcCounter $ interpreter
  case functionCounter Map.!? (parameters, leftParenthesis, functionName, isInitializer) of
    Nothing -> do
      let newFunctionCounter = FuncCounter $ Map.insert (parameters, leftParenthesis, functionName, isInitializer) (FunctionID 0) functionCounter
      put interpreter{funcCounter = newFunctionCounter}
      return $ VFunction parameters leftParenthesis functionName isInitializer definingEnvironment function $ FunctionID 0
    Just (FunctionID count) -> do
      let newFunctionCounter =
            FuncCounter $
              Map.insert
                (parameters, leftParenthesis, functionName, isInitializer)
                (FunctionID $ count + 1)
                functionCounter
      put interpreter{funcCounter = newFunctionCounter}
      return $ VFunction parameters leftParenthesis functionName isInitializer definingEnvironment function $ FunctionID $ count + 1

defaultInterpreter :: Locals -> Interpreter
defaultInterpreter localVariables =
  let clockToken =
        MkToken
          { tokenType = IDENTIFIER
          , offset = 0
          , literal = Nothing
          , line = 0
          , lexeme = "clock"
          }
      clockFunc _ = do
        time <- liftIO getPOSIXTime
        return $ SomeValue $ VNumber (realToFrac time :: Double)
      clock = VFunction [] clockToken "<native fn>" False undefined clockFunc (FunctionID 0)
      globalEnv = Environment (Map.fromList [("clock", SomeValue clock)]) Nothing
      table = Map.fromList [(EnvID 0, globalEnv)]
   in Interpreter table (resolverMap localVariables) (EnvID 0) (EnvID 0) (InstanceID 0) (FuncCounter Map.empty)

createChildEnv :: (MonadState Interpreter m) => m ()
createChildEnv = do
  interpreter <- get
  let outputId = nextLargest . nextEnvironmentId $ interpreter
      emptyEnv = Environment Map.empty $ Just $ currentEnvironment interpreter
      newTable = Map.insert outputId emptyEnv $ environmentTable interpreter
  put interpreter{environmentTable = newTable, currentEnvironment = outputId, nextEnvironmentId = outputId}

changeToParent :: InterpreterOutput ()
changeToParent = do
  interpreter <- get
  let Environment _ parent = environmentTable interpreter Map.! currentEnvironment interpreter
  case parent of
    Just parentEnvironmentId -> put interpreter{currentEnvironment = parentEnvironmentId}
    Nothing -> error "can't change to parent when no parent"

define :: (MonadState Interpreter m) => Token -> SomeValue -> m ()
define token value = do
  interpreter <- get
  let Environment envTable parent = environmentTable interpreter Map.! currentEnvironment interpreter
      definedTable = Map.insert (lexeme token) value envTable
      definedEnvironment = Environment definedTable parent
  put interpreter{environmentTable = Map.insert (currentEnvironment interpreter) definedEnvironment (environmentTable interpreter)}

assignTok :: Token -> SomeValue -> InterpreterOutput ()
assignTok token value = do
  interpreter <- get
  let Environment envTable parent = environmentTable interpreter Map.! currentEnvironment interpreter
  if Map.member (lexeme token) envTable
    then
      let assignedTable = Map.insert (lexeme token) value envTable
          assignedEnv = Environment assignedTable parent
       in put
            interpreter
              { environmentTable =
                  Map.insert
                    (currentEnvironment interpreter)
                    assignedEnv
                    (environmentTable interpreter)
              }
    else case parent of
      Just parentEnv -> do
        put interpreter{currentEnvironment = parentEnv}
        assignTok token value
        put interpreter{currentEnvironment = currentEnvironment interpreter}
      Nothing -> throwRuntimeError $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

assignThis :: (MonadState Interpreter m) => Value 'ValueInstance -> EnvID -> m ()
assignThis value toDefineIn = do
  interpreter <- get
  let Environment variables parent = case environmentTable interpreter Map.!? toDefineIn of
        Nothing -> error "here"
        Just e -> e
      env = Environment (Map.insert "this" (SomeValue value) variables) parent
  put interpreter{environmentTable = Map.insert toDefineIn env $ environmentTable interpreter}

assign :: Expr -> Token -> SomeValue -> InterpreterOutput ()
assign expr name value = do
  interpreter <- get
  case locals interpreter Map.!? expr of
    Nothing -> assignGlobal name value
    Just distance -> assignAt distance name value

assignAt :: Int -> Token -> SomeValue -> InterpreterOutput ()
assignAt distance name value = do
  interpreter <- get
  ancestor <- getAncestor distance $ currentEnvironment interpreter
  let Environment envTable parent = fromMaybe (error "in assignAt") (environmentTable interpreter Map.!? ancestor)
      newTable =
        Map.insert
          ancestor
          (Environment (Map.insert (lexeme name) value envTable) parent)
          (environmentTable interpreter)
  put interpreter{environmentTable = newTable}

assignGlobal :: Token -> SomeValue -> InterpreterOutput ()
assignGlobal token value = do
  interpreter <- get
  let Environment globalTable parentEnv = environmentTable interpreter Map.! EnvID 0
      newGlobalEnv = Environment (Map.insert (lexeme token) value globalTable) parentEnv
  case globalTable Map.!? lexeme token of
    Just _ ->
      put interpreter{environmentTable = Map.insert (EnvID 0) newGlobalEnv $ environmentTable interpreter}
    Nothing -> throwRuntimeError $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

lookupVariable :: Token -> Expr -> InterpreterOutput SomeValue
lookupVariable name expr = do
  interpreter <- get
  case locals interpreter Map.!? expr of
    Nothing ->
      let Environment envTable _ = environmentTable interpreter Map.! EnvID 0
       in case envTable Map.!? lexeme name of
            Nothing -> throwRuntimeError $ runtimeError name $ "Undefined variable '" ++ lexeme name ++ "'."
            Just value -> return value
    Just distance -> getAt distance $ lexeme name

getAt :: Int -> String -> InterpreterOutput SomeValue
getAt distance name = do
  interpreter <- get
  ancestor <- getAncestor distance $ currentEnvironment interpreter
  let Environment envTable _ = environmentTable interpreter Map.! ancestor
  case envTable Map.!? name of
    Nothing ->
      error $
        "getting "
          ++ name
          ++ " from envTable, distance is "
          ++ show distance
          ++ ", current is "
          ++ show (currentEnvironment interpreter)
          ++ "\n\n     interp is "
          ++ show interpreter
    Just value -> return value

-- TODO: fix this to make it more total?
getAncestor :: Int -> EnvID -> InterpreterOutput EnvID
getAncestor 0 childId = return childId
getAncestor distance childId = do
  interpreter <- get
  let Environment _ parent = environmentTable interpreter Map.! childId
      parentId = fromMaybe undefined parent
  getAncestor (distance - 1) parentId

restoreRunningEnv :: (MonadState Interpreter m) => Interpreter -> m ()
restoreRunningEnv interpreter = setRunningEnv $ currentEnvironment interpreter

setRunningEnv :: (MonadState Interpreter m) => EnvID -> m ()
setRunningEnv envId = get >>= \interpreter -> put interpreter{currentEnvironment = envId}

newInstance :: String -> Value 'ValueClass -> InterpreterOutput (Value 'ValueInstance)
newInstance className loxClass = do
  interpreter <- get
  let iid = nextInstanceId interpreter
  put interpreter{nextInstanceId = incrementInstanceId iid}
  return $ VInstance className Map.empty loxClass iid

setProperty :: InstanceID -> Token -> SomeValue -> InterpreterOutput ()
setProperty iid name value = do
  interpreter <- get
  let mapFunc (SomeValue (VInstance className properties methods otherIid)) =
        if iid == otherIid
          then SomeValue $ VInstance className (Map.insert (lexeme name) value properties) methods otherIid
          else SomeValue $ VInstance className (Map.map mapFunc properties) methods otherIid
      mapFunc other = other
      envMap (Environment variables parentId) = Environment (Map.map mapFunc variables) parentId
  put interpreter{environmentTable = Map.map envMap $ environmentTable interpreter}

data ValueKind
  = ValueNumber
  | ValueStr
  | ValueBoolean
  | ValueNil
  | ValueCall
  | ValueFunction
  | ValueClass
  | ValueInstance
  deriving (Eq, Show)

data SomeValue where
  SomeValue :: Value k -> SomeValue

instance Show SomeValue where
  show (SomeValue v) = show v

instance Eq SomeValue where
  SomeValue (VNumber n1) == SomeValue (VNumber n2) = n1 == n2
  SomeValue (VStr s1) == SomeValue (VStr s2) = s1 == s2
  SomeValue (VBoolean b1) == SomeValue (VBoolean b2) = b1 == b2
  SomeValue VNil == SomeValue VNil = True
  SomeValue (VCall arity1 tok1 name1) == SomeValue (VCall arity2 tok2 name2) = arity1 == arity2 && tok1 == tok2 && name1 == name2
  SomeValue (VFunction params1 callee1 name1 isInit1 _ _ fid1) == SomeValue (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    params1 == params2 && callee1 == callee2 && name1 == name2 && isInit1 == isInit2 && fid1 == fid2
  SomeValue (VClass name1 _ _ _) == SomeValue (VClass name2 _ _ _) = name1 == name2
  SomeValue (VInstance _ _ _ iid1) == SomeValue (VInstance _ _ _ iid2) = iid1 == iid2
  _ == _ = False

data Value (k :: ValueKind) where
  VNumber :: Double -> Value 'ValueNumber
  VStr :: String -> Value 'ValueStr
  VBoolean :: Bool -> Value 'ValueBoolean
  VNil :: Value 'ValueNil
  VCall :: Int -> Token -> String -> Value 'ValueCall
  VFunction :: [Token] -> Token -> String -> Bool -> EnvID -> ([SomeValue] -> StateT Interpreter (ExceptT String IO) SomeValue) -> FunctionID -> Value 'ValueFunction
  VClass :: String -> Maybe (Value 'ValueClass) -> Int -> (Map.Map String (Value 'ValueFunction)) -> Value 'ValueClass
  VInstance :: String -> (Map.Map String SomeValue) -> Value 'ValueClass -> InstanceID -> Value 'ValueInstance

instance Eq (Value k) where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VStr s1) == (VStr s2) = s1 == s2
  (VBoolean b1) == (VBoolean b2) = b1 == b2
  VNil == VNil = True
  (VCall arity1 tok1 name1) == (VCall arity2 tok2 name2) = arity1 == arity2 && tok1 == tok2 && name1 == name2
  (VFunction params1 callee1 name1 isInit1 _ _ fid1) == (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    params1 == params2 && callee1 == callee2 && name1 == name2 && isInit1 == isInit2 && fid1 == fid2
  (VClass name1 _ _ _) == (VClass name2 _ _ _) = name1 == name2
  (VInstance _ _ _ iid1) == (VInstance _ _ _ iid2) = iid1 == iid2

instance Ord (Value k) where
  compare (VNumber n1) (VNumber n2) = compare n1 n2
  compare (VStr s1) (VStr s2) = compare s1 s2
  compare (VBoolean b1) (VBoolean b2) = compare b1 b2
  compare VNil VNil = EQ
  compare (VCall arity1 tok1 name1) (VCall arity2 tok2 name2) = compare (arity1, tok1, name1) (arity2, tok2, name2)
  compare (VFunction params1 callee1 name1 isInit1 _ _ fid1) (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    compare (params1, callee1, name1, isInit1, fid1) (params2, callee2, name2, isInit2, fid2)
  compare (VClass name1 _ _ _) (VClass name2 _ _ _) = compare name1 name2
  compare (VInstance _ _ _ iid1) (VInstance _ _ _ iid2) = compare iid1 iid2

instance Show (Value k) where
  show (VNumber n) = formatNumber n
   where
    formatNumber :: Double -> String
    formatNumber x
      | isNegativeZero x = "-0"
      | x == fromInteger (round x) = show (round x :: Integer)
      | otherwise = showFFloat Nothing x ""
  show (VStr s) = s
  show (VBoolean b) = if b then "true" else "false"
  show VNil = "nil"
  show (VFunction _ _ s _ _ _ _) = s
  show (VCall _ _ s) = s
  show (VClass name _ _ _) = name
  show (VInstance name _ _ _) = name ++ " instance"

fromLiteral :: Literal -> SomeValue
fromLiteral (Tokens.Number n) = SomeValue $ VNumber n
fromLiteral (Tokens.Str s) = SomeValue $ VStr s
fromLiteral (Tokens.Boolean b) = SomeValue $ VBoolean b
fromLiteral Tokens.Nil = SomeValue VNil
