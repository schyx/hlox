{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Phases.Interpreter (
  interpret,
  interpretExpr,
  defaultInterpreter,
  Interpreter,
) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error
import Numeric (showFFloat)
import Phases.Expr
import Phases.Resolver (Locals (resolverMap))
import Phases.Stmt
import Tokens

type InterpreterOutput a = ExceptT (SomeValue, Interpreter) (ExceptT String IO) a

throwRuntimeError :: String -> InterpreterOutput a
throwRuntimeError = lift . throwError

interpret :: Interpreter -> SomeStmt -> InterpreterOutput Interpreter
interpret interpreter (SomeStmt (Print expr)) = do
  (val, interp') <- interpretExpr interpreter expr
  liftIO $ print val
  return interp'
interpret interpreter (SomeStmt (Expression expr)) = do
  (_, interp') <- interpretExpr interpreter expr
  return interp'
interpret interpreter (SomeStmt (Var name initializer)) = do
  (val, interp') <- interpretExpr interpreter initializer
  return $ define interp' name val
interpret interpreter (SomeStmt (Block stmts)) = do
  let interp' = createChildEnv interpreter
  interp'' <- execBlock interp' stmts
  return $ changeToParent interp''
interpret interpreter (SomeStmt (If condition ifBranch (Just elseBranch))) = do
  (val, interp') <- interpretExpr interpreter condition
  interpret interp' (if isTruthy val then ifBranch else elseBranch)
interpret interpreter (SomeStmt (If condition ifBranch Nothing)) = do
  (val, interp') <- interpretExpr interpreter condition
  if isTruthy val then interpret interp' ifBranch else return interp'
interpret interpreter (SomeStmt (While condition whileBlock)) = do
  (value, interpreter') <- interpretExpr interpreter condition
  if isTruthy value
    then do
      interpreter'' <- interpret interpreter' whileBlock
      interpret interpreter'' (SomeStmt (While condition whileBlock))
    else return interpreter'
interpret interpreter (SomeStmt function@(Function functionName _ _)) =
  let (functionObject, interpreter') = functionToValue interpreter False function
   in return $ define interpreter' functionName $ SomeValue functionObject
interpret interpreter (SomeStmt (Return _ returnExpression)) =
  case returnExpression of
    Nothing -> throwError (SomeValue VNil, interpreter)
    Just value -> do
      (returnValue, interpreter') <- interpretExpr interpreter value
      throwError (returnValue, interpreter')
interpret interpreter (SomeStmt (Class name classMethods)) =
  let interpreter' = define interpreter name $ SomeValue VNil
      foldFunc function@(Function functionName _ _) =
        Map.insert
          (lexeme functionName)
          (fst $ functionToValue (createChildEnv interpreter) (lexeme functionName == "init") function)
      methods = foldr foldFunc Map.empty classMethods
      arity = case methods Map.!? "init" of
        Nothing -> 0
        Just (VFunction numArgs _ _ _ _ _ _) -> length numArgs
      klass = SomeValue $ VClass (lexeme name) arity methods
   in do
        interpreter'' <- assignTok interpreter' name klass
        return (restoreRunningEnv interpreter'' $ createChildEnv interpreter'')

functionToValue :: Interpreter -> Bool -> Stmt 'KFunction -> (Value 'ValueFunction, Interpreter)
functionToValue definingInterpreter isInitializer (Function functionName functionParameters body) =
  let functionF callingInterpreter args = do
        let enclosingInterpreter = restoreRunningEnv definingInterpreter callingInterpreter
        let initialFunctionInterpreter = createChildEnv enclosingInterpreter
        let functionInterpreter =
              foldl
                (\previousInterpreter (token, value) -> define previousInterpreter token value)
                initialFunctionInterpreter
                (zip functionParameters args)
        run <- runExceptT $ execBlock functionInterpreter body
        case run of
          Left (value, outputInterpreter) -> ExceptT $ return $ Right (value, outputInterpreter)
          Right outputInterpreter -> ExceptT $ return $ Right (SomeValue VNil, outputInterpreter)
   in addFunction
        functionParameters
        functionName
        ("<fn " ++ lexeme functionName ++ ">")
        isInitializer
        definingInterpreter
        functionF
        definingInterpreter

execBlock :: Interpreter -> [SomeStmt] -> InterpreterOutput Interpreter
execBlock = foldM interpret

interpretExpr :: Interpreter -> Expr -> InterpreterOutput (SomeValue, Interpreter)
interpretExpr interpreter (Call callee leftParenthesis argumentExpressions) = do
  (value, callerInterp) <- interpretExpr interpreter callee
  (arguments, argumentsInterp) <- interpretExprs callerInterp argumentExpressions
  case value of
    (SomeValue function@VFunction{}) -> callFunction argumentsInterp arguments function
    (SomeValue (VClass className _ classMethods)) ->
      let (inst, instanceInterpreter) = newInstance className classMethods argumentsInterp
       in case classMethods Map.!? "init" of
            Nothing ->
              if null arguments
                then return (SomeValue inst, instanceInterpreter)
                else
                  throwRuntimeError $
                    runtimeError leftParenthesis $
                      "Expected 0 arguments but got " ++ show (length arguments) ++ "."
            Just initializer@(VFunction _ _ _ _ functionInterpreter _ _) ->
              callFunction (assignThis inst functionInterpreter instanceInterpreter) arguments initializer
    _ -> throwRuntimeError $ runtimeError leftParenthesis "Can only call functions and classes."
 where
  callFunction :: Interpreter -> [SomeValue] -> Value 'ValueFunction -> InterpreterOutput (SomeValue, Interpreter)
  callFunction callingInterpreter argument (VFunction parameters _ _ isInitializer _ function _) =
    if length parameters == length argument
      then do
        (outputValue, outputInterpreter) <- lift $ function callingInterpreter argument
        return
          ( if isInitializer
              then getAt outputInterpreter 1 "this"
              else outputValue
          , restoreRunningEnv interpreter outputInterpreter
          )
      else
        throwRuntimeError
          $ runtimeError
            leftParenthesis
          $ "Expected " ++ show (length parameters) ++ " arguments but got " ++ show (length argument) ++ "."
  interpretExprs :: Interpreter -> [Expr] -> InterpreterOutput ([SomeValue], Interpreter)
  interpretExprs argumentsInterpreter [] = return ([], argumentsInterpreter)
  interpretExprs argumentsInterpreter (expr : exprs) = do
    (value, interpreter') <- interpretExpr argumentsInterpreter expr
    (parameters, afterParametersInterpreter) <- interpretExprs interpreter' exprs
    return (value : parameters, afterParametersInterpreter)
interpretExpr interpreter expr@(Assign name assigningExpression) = do
  (value, afterExpressionInterpreter) <- interpretExpr interpreter assigningExpression
  assignedInterpeter <- assign afterExpressionInterpreter expr name value
  return (value, assignedInterpeter)
interpretExpr interpreter (Binary left operator right) = do
  (leftValue, afterLeftInterp) <- interpretExpr interpreter left
  (rightValue, afterRightInterp) <- interpretExpr afterLeftInterp right
  output <- getOutput leftValue rightValue
  return (output, afterRightInterp)
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
  booleanBinaryTable =
    Map.fromList
      [ (LESS, (<))
      , (LESS_EQUAL, (<=))
      , (GREATER, (>))
      , (GREATER_EQUAL, (>=))
      ]
  numericBinaryTable =
    Map.fromList
      [ (STAR, (*))
      , (SLASH, (/))
      , (MINUS, (-))
      ]
interpretExpr interpreter (Unary operator expr) = do
  (value, afterExpressionInterpreter) <- interpretExpr interpreter expr
  (unaryOpTable Map.! tokenType operator) value afterExpressionInterpreter
 where
  unaryOpTable =
    Map.fromList
      [ (BANG, \value interpreter' -> return (SomeValue $ VBoolean $ not $ isTruthy value, interpreter'))
      ,
        ( MINUS
        , \val interpreter' -> do
            n <- toNumber val operator
            return (SomeValue $ VNumber $ -n, interpreter')
        )
      ]
interpretExpr interpreter (Grouping expr) = interpretExpr interpreter expr
interpretExpr interpreter expr@(Variable token) = do
  value <- lookupVariable interpreter token expr
  return (value, interpreter)
interpretExpr interpreter (AndExpr left _ right) = do
  (value, afterLeftExpressionInterpreter) <- interpretExpr interpreter left
  if not $ isTruthy value
    then return (value, afterLeftExpressionInterpreter)
    else interpretExpr afterLeftExpressionInterpreter right
interpretExpr interpreter (OrExpr left _ right) = do
  (value, afterLeftExpressionInterpreter) <- interpretExpr interpreter left
  if isTruthy value
    then return (value, afterLeftExpressionInterpreter)
    else interpretExpr afterLeftExpressionInterpreter right
interpretExpr interpreter (Primary singleLiteral) = return (fromLiteral singleLiteral, interpreter)
interpretExpr interpreter (Get object name) = do
  (interpretedObject, interpreter') <- interpretExpr interpreter object
  getFieldOrMethod interpreter' name interpretedObject
interpretExpr interpreter (Set object name value) = do
  (interpretedObject, interpreter') <- interpretExpr interpreter object
  case interpretedObject of
    SomeValue (VInstance _ _ _ iid) -> do
      (interpretedValue, interpreter'') <- interpretExpr interpreter' value
      return (interpretedValue, setProperty iid name interpretedValue interpreter'')
    _ -> throwRuntimeError $ runtimeError name "Only instances have fields."
interpretExpr interpreter expr@(This keyword) = do
  value <- lookupVariable interpreter keyword expr
  return (value, interpreter)

getFieldOrMethod :: Interpreter -> Token -> SomeValue -> InterpreterOutput (SomeValue, Interpreter)
getFieldOrMethod interpreter name (SomeValue inst@(VInstance _ properties methods _)) =
  case properties Map.!? lexeme name of
    Just value -> return (value, interpreter)
    Nothing -> case methods Map.!? lexeme name of
      Just (VFunction params leftParenthesis functionName isInitializer definingInterp function _) ->
        let interpWithThis = assignThis inst definingInterp interpreter
            (method, interpWithMethod) =
              addFunction params leftParenthesis functionName isInitializer definingInterp function interpWithThis
         in return (SomeValue method, restoreRunningEnv interpreter interpWithMethod)
      Nothing -> throwRuntimeError $ runtimeError name $ "Undefined property '" ++ lexeme name ++ "'."
getFieldOrMethod _ name _ = throwRuntimeError $ runtimeError name "Only instances have properties."

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
  Interpreter ->
  (Interpreter -> [SomeValue] -> ExceptT String IO (SomeValue, Interpreter)) ->
  Interpreter ->
  (Value 'ValueFunction, Interpreter)
addFunction parameters leftParenthesis functionName isInitializer definingInterpreter function interpreter =
  let functionCounter = (getFuncCounter . funcCounter $ interpreter)
   in case functionCounter Map.!? (parameters, leftParenthesis, functionName, isInitializer) of
        Nothing ->
          let newFunctionCounter = FuncCounter $ Map.insert (parameters, leftParenthesis, functionName, isInitializer) (FunctionID 0) functionCounter
           in ( VFunction parameters leftParenthesis functionName isInitializer definingInterpreter function $ FunctionID 0
              , interpreter{funcCounter = newFunctionCounter}
              )
        Just (FunctionID count) ->
          let newFunctionCounter = FuncCounter $ Map.insert (parameters, leftParenthesis, functionName, isInitializer) (FunctionID $ count + 1) functionCounter
           in ( VFunction parameters leftParenthesis functionName isInitializer definingInterpreter function $ FunctionID $ count + 1
              , interpreter{funcCounter = newFunctionCounter}
              )

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
      clockFunc interpreter _ = do
        time <- liftIO getPOSIXTime
        return (SomeValue $ VNumber (realToFrac time :: Double), interpreter)
      clock = VFunction [] clockToken "<native fn>" False undefined clockFunc (FunctionID 0)
      globalEnv = Environment (Map.fromList [("clock", SomeValue clock)]) Nothing
      table = Map.fromList [(EnvID 0, globalEnv)]
   in Interpreter table (resolverMap localVariables) (EnvID 0) (EnvID 0) (InstanceID 0) (FuncCounter Map.empty)

createChildEnv :: Interpreter -> Interpreter
createChildEnv interpreter =
  let outputId = nextLargest . nextEnvironmentId $ interpreter
      emptyEnv = Environment Map.empty $ Just $ currentEnvironment interpreter
      newTable = Map.insert outputId emptyEnv $ environmentTable interpreter
   in interpreter
        { environmentTable = newTable
        , currentEnvironment = outputId
        , nextEnvironmentId = outputId
        }

changeToParent :: Interpreter -> Interpreter
changeToParent interpreter =
  let Environment _ parent = environmentTable interpreter Map.! currentEnvironment interpreter
   in case parent of
        Just parentEnvironmentId -> interpreter{currentEnvironment = parentEnvironmentId}
        Nothing -> error "can't change to parent when no parent"

define :: Interpreter -> Token -> SomeValue -> Interpreter
define interpreter token value =
  let Environment envTable parent = environmentTable interpreter Map.! currentEnvironment interpreter
   in let definedTable = Map.insert (lexeme token) value envTable
          definedEnvironment = Environment definedTable parent
       in interpreter
            { environmentTable =
                Map.insert
                  (currentEnvironment interpreter)
                  definedEnvironment
                  (environmentTable interpreter)
            }

assignTok :: Interpreter -> Token -> SomeValue -> InterpreterOutput Interpreter
assignTok interpreter token value =
  case environmentTable interpreter Map.! currentEnvironment interpreter of
    (Environment envTable parent)
      | Map.member (lexeme token) envTable ->
          let assignedTable = Map.insert (lexeme token) value envTable
              assignedEnv = Environment assignedTable parent
           in return
                interpreter
                  { environmentTable =
                      Map.insert
                        (currentEnvironment interpreter)
                        assignedEnv
                        (environmentTable interpreter)
                  }
      | Just parentEnv <- parent -> do
          assignedInParent <- assignTok interpreter{currentEnvironment = parentEnv} token value
          return $ interpreter{environmentTable = environmentTable assignedInParent}
      | Nothing <- parent ->
          throwRuntimeError $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

assignThis :: Value 'ValueInstance -> Interpreter -> Interpreter -> Interpreter
assignThis value (Interpreter _ _ toDefineIn _ _ _) interpreter =
  let Environment variables parent = case environmentTable interpreter Map.!? toDefineIn of
        Nothing -> error "here"
        Just e -> e
      env = Environment (Map.insert "this" (SomeValue value) variables) parent
   in interpreter{environmentTable = Map.insert toDefineIn env $ environmentTable interpreter}

assign :: Interpreter -> Expr -> Token -> SomeValue -> InterpreterOutput Interpreter
assign interpreter expr name value =
  case locals interpreter Map.!? expr of
    Nothing -> assignGlobal interpreter name value
    Just distance -> assignAt interpreter distance name value

assignAt :: Interpreter -> Int -> Token -> SomeValue -> InterpreterOutput Interpreter
assignAt interpreter distance name value =
  let ancestorId = ancestor distance interpreter $ currentEnvironment interpreter
      Environment envTable parent = fromMaybe (error "in assignAt") (environmentTable interpreter Map.!? ancestorId)
      newTable =
        Map.insert
          ancestorId
          (Environment (Map.insert (lexeme name) value envTable) parent)
          (environmentTable interpreter)
   in return interpreter{environmentTable = newTable}

assignGlobal :: Interpreter -> Token -> SomeValue -> InterpreterOutput Interpreter
assignGlobal interpreter token value =
  let Environment globalTable parentEnv = environmentTable interpreter Map.! EnvID 0
      newGlobalEnv = Environment (Map.insert (lexeme token) value globalTable) parentEnv
   in case globalTable Map.!? lexeme token of
        Just _ ->
          return
            interpreter
              { environmentTable = Map.insert (EnvID 0) newGlobalEnv $ environmentTable interpreter
              }
        Nothing -> throwRuntimeError $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

lookupVariable :: Interpreter -> Token -> Expr -> InterpreterOutput SomeValue
lookupVariable interpreter name expr =
  case locals interpreter Map.!? expr of
    Nothing ->
      let Environment envTable _ = environmentTable interpreter Map.! EnvID 0
       in case envTable Map.!? lexeme name of
            Nothing -> throwRuntimeError $ runtimeError name $ "Undefined variable '" ++ lexeme name ++ "'."
            Just value -> return value
    Just distance -> return $ getAt interpreter distance $ lexeme name

getAt :: Interpreter -> Int -> String -> SomeValue
getAt interpreter distance name =
  let Environment envTable _ =
        fromMaybe
          (error "in getAt")
          (environmentTable interpreter Map.!? ancestor distance interpreter (currentEnvironment interpreter))
   in case envTable Map.!? name of
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
        Just value -> value

ancestor :: Int -> Interpreter -> EnvID -> EnvID
ancestor distance interpreter childId
  | distance == 0 = childId
  | otherwise = ancestor (distance - 1) interpreter $
      case environmentTable interpreter Map.!? childId of
        Just (Environment _ parent) -> fromMaybe undefined parent
        Nothing -> error "in ancestor"

restoreRunningEnv :: Interpreter -> Interpreter -> Interpreter
restoreRunningEnv interpreter = setRunningEnv $ currentEnvironment interpreter

setRunningEnv :: EnvID -> Interpreter -> Interpreter
setRunningEnv envId interpreter = interpreter{currentEnvironment = envId}

newInstance :: String -> Map.Map String (Value 'ValueFunction) -> Interpreter -> (Value 'ValueInstance, Interpreter)
newInstance className classMethods interpreter =
  let iid = nextInstanceId interpreter
   in ( VInstance className Map.empty classMethods iid
      , interpreter{nextInstanceId = incrementInstanceId iid}
      )

setProperty :: InstanceID -> Token -> SomeValue -> Interpreter -> Interpreter
setProperty iid name value interpreter =
  let mapFunc (SomeValue (VInstance className properties methods otherIid)) =
        if iid == otherIid
          then SomeValue $ VInstance className (Map.insert (lexeme name) value properties) methods otherIid
          else SomeValue $ VInstance className (Map.map mapFunc properties) methods otherIid
      mapFunc other = other
      envMap (Environment variables parentId) = Environment (Map.map mapFunc variables) parentId
   in interpreter{environmentTable = Map.map envMap $ environmentTable interpreter}

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
  SomeValue (VClass name1 _ _) == SomeValue (VClass name2 _ _) = name1 == name2
  SomeValue (VInstance _ _ _ iid1) == SomeValue (VInstance _ _ _ iid2) = iid1 == iid2
  _ == _ = False

data Value (k :: ValueKind) where
  VNumber :: Double -> Value 'ValueNumber
  VStr :: String -> Value 'ValueStr
  VBoolean :: Bool -> Value 'ValueBoolean
  VNil :: Value 'ValueNil
  VCall :: Int -> Token -> String -> Value 'ValueCall
  VFunction :: [Token] -> Token -> String -> Bool -> Interpreter -> (Interpreter -> [SomeValue] -> ExceptT String IO (SomeValue, Interpreter)) -> FunctionID -> Value 'ValueFunction
  VClass :: String -> Int -> (Map.Map String (Value 'ValueFunction)) -> Value 'ValueClass
  VInstance :: String -> (Map.Map String SomeValue) -> (Map.Map String (Value 'ValueFunction)) -> InstanceID -> Value 'ValueInstance

instance Eq (Value k) where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VStr s1) == (VStr s2) = s1 == s2
  (VBoolean b1) == (VBoolean b2) = b1 == b2
  VNil == VNil = True
  (VCall arity1 tok1 name1) == (VCall arity2 tok2 name2) = arity1 == arity2 && tok1 == tok2 && name1 == name2
  (VFunction params1 callee1 name1 isInit1 _ _ fid1) == (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    params1 == params2 && callee1 == callee2 && name1 == name2 && isInit1 == isInit2 && fid1 == fid2
  (VClass name1 _ _) == (VClass name2 _ _) = name1 == name2
  (VInstance _ _ _ iid1) == (VInstance _ _ _ iid2) = iid1 == iid2

instance Ord (Value k) where
  compare (VNumber n1) (VNumber n2) = compare n1 n2
  compare (VStr s1) (VStr s2) = compare s1 s2
  compare (VBoolean b1) (VBoolean b2) = compare b1 b2
  compare VNil VNil = EQ
  compare (VCall arity1 tok1 name1) (VCall arity2 tok2 name2) = compare (arity1, tok1, name1) (arity2, tok2, name2)
  compare (VFunction params1 callee1 name1 isInit1 _ _ fid1) (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    compare (params1, callee1, name1, isInit1, fid1) (params2, callee2, name2, isInit2, fid2)
  compare (VClass name1 _ _) (VClass name2 _ _) = compare name1 name2
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
  show (VClass name _ _) = name
  show (VInstance name _ _ _) = name ++ " instance"

fromLiteral :: Literal -> SomeValue
fromLiteral (Tokens.Number n) = SomeValue $ VNumber n
fromLiteral (Tokens.Str s) = SomeValue $ VStr s
fromLiteral (Tokens.Boolean b) = SomeValue $ VBoolean b
fromLiteral Tokens.Nil = SomeValue VNil
