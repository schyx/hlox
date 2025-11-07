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
interpret interp (SomeStmt (Print expr)) = do
  (val, interp') <- interpretExpr interp expr
  liftIO $ print val
  return interp'
interpret interp (SomeStmt (Expression expr)) = do
  (_, interp') <- interpretExpr interp expr
  return interp'
interpret interp (SomeStmt (Var name initializer)) = do
  (val, interp') <- interpretExpr interp initializer
  return $ define interp' name val
interpret interp (SomeStmt (Block stmts)) = do
  let interp' = createChildEnv interp
  interp'' <- execBlock interp' stmts
  return $ changeToParent interp''
interpret interp (SomeStmt (If condition ifBranch (Just elseBranch))) = do
  (val, interp') <- interpretExpr interp condition
  interpret interp' (if isTruthy val then ifBranch else elseBranch)
interpret interp (SomeStmt (If condition ifBranch Nothing)) = do
  (val, interp') <- interpretExpr interp condition
  if isTruthy val then interpret interp' ifBranch else return interp'
interpret interp (SomeStmt (While condition whileBlock)) = do
  (val, interp') <- interpretExpr interp condition
  if isTruthy val
    then do
      interp'' <- interpret interp' whileBlock
      interpret interp'' (SomeStmt (While condition whileBlock))
    else return interp'
interpret interp (SomeStmt func@(Function fname _ _)) =
  let (funcObj, interp') = functionToValue interp False func
   in return $ define interp' fname $ SomeValue funcObj
interpret interp (SomeStmt (Return _ returnValue)) =
  case returnValue of
    Nothing -> throwError (SomeValue VNil, interp)
    Just value -> do
      (val, interp') <- interpretExpr interp value
      throwError (val, interp')
interpret interp (SomeStmt (Class name classMethods)) =
  let interp' = define interp name $ SomeValue VNil
      foldFunc func@(Function fname _ _) =
        Map.insert (lexeme fname) (fst $ functionToValue (createChildEnv interp) (lexeme fname == "init") func)
      methods = foldr foldFunc Map.empty classMethods
      arity = case methods Map.!? "init" of
        Nothing -> 0
        Just (VFunction args _ _ _ _ _ _) -> length args
      klass = SomeValue $ VClass (lexeme name) arity methods
   in case assignTok interp' name klass of
        Right interp'' -> return (restoreRunningEnv interp'' $ createChildEnv interp'')
        Left err -> throwRuntimeError err

functionToValue :: Interpreter -> Bool -> Stmt 'KFunction -> (Value 'ValueFunction, Interpreter)
functionToValue interp isInitializer (Function fname params body) =
  let functionF interpreter args = do
        let enclosingInterp = restoreRunningEnv interp interpreter
        let fInterpInitial = createChildEnv enclosingInterp
        let fInterp = foldl (\prevInterp (tok, val) -> define prevInterp tok val) fInterpInitial (zip params args)
        run <- runExceptT $ execBlock fInterp body
        case run of
          Left (val, outputInterp) -> ExceptT $ return $ Right (val, outputInterp)
          Right outputInterp -> ExceptT $ return $ Right (SomeValue VNil, outputInterp)
   in addFunction params fname ("<fn " ++ lexeme fname ++ ">") isInitializer interp functionF interp

execBlock :: Interpreter -> [SomeStmt] -> InterpreterOutput Interpreter
execBlock = foldM interpret

interpretExpr :: Interpreter -> Expr -> InterpreterOutput (SomeValue, Interpreter)
interpretExpr interp (Call callee paren argExprs) = do
  (val, callerInterp) <- interpretExpr interp callee
  (args, argsInterp) <- interpretExprs callerInterp argExprs
  case val of
    (SomeValue function@VFunction{}) -> callFunction argsInterp args function
    (SomeValue (VClass className _ classMethods)) ->
      let (inst, instInterp) = newInstance className classMethods argsInterp
       in case classMethods Map.!? "init" of
            Nothing ->
              if null args
                then return (SomeValue inst, instInterp)
                else throwRuntimeError $ runtimeError paren $ "Expected 0 arguments but got " ++ show (length args) ++ "."
            Just initializer@(VFunction _ _ _ _ funcInterp _ _) ->
              callFunction (assignThis inst funcInterp instInterp) args initializer
    _ -> throwRuntimeError $ runtimeError paren "Can only call functions and classes."
 where
  callFunction :: Interpreter -> [SomeValue] -> Value 'ValueFunction -> InterpreterOutput (SomeValue, Interpreter)
  callFunction callInterp args (VFunction params _ _ isInitializer _ func _) =
    if length params == length args
      then do
        (outputVal, outputInterp) <- lift $ func callInterp args
        return
          ( if isInitializer
              then getAt outputInterp 1 "this"
              else outputVal
          , restoreRunningEnv interp outputInterp
          )
      else
        throwRuntimeError
          $ runtimeError
            paren
          $ "Expected " ++ show (length params) ++ " arguments but got " ++ show (length args) ++ "."
  interpretExprs :: Interpreter -> [Expr] -> InterpreterOutput ([SomeValue], Interpreter)
  interpretExprs argsInterp [] = return ([], argsInterp)
  interpretExprs argsInterp (expr : exprs) = do
    (val, interp') <- interpretExpr argsInterp expr
    (params, afterParamsInterp) <- interpretExprs interp' exprs
    return (val : params, afterParamsInterp)
interpretExpr interp expr@(Assign name value) = do
  (val, interp') <- interpretExpr interp value
  case assign interp' expr name val of
    Right assignedInterp -> return (val, assignedInterp)
    Left err -> throwRuntimeError err
interpretExpr interp (Binary left operator right) = do
  (leftVal, afterLeftInterp) <- interpretExpr interp left
  (rightVal, afterRightInterp) <- interpretExpr afterLeftInterp right
  output <- getOutput leftVal rightVal
  return (output, afterRightInterp)
 where
  getOutput :: SomeValue -> SomeValue -> InterpreterOutput SomeValue
  getOutput leftVal rightVal
    | tokenType operator `elem` [BANG_EQUAL, EQUAL_EQUAL] =
        return $
          SomeValue $
            VBoolean
              ( if tokenType operator == EQUAL_EQUAL
                  then leftVal == rightVal
                  else leftVal /= rightVal
              )
    | tokenType operator == PLUS = case toNumberPair leftVal rightVal operator of
        Right (leftn, rightn) -> return $ SomeValue $ VNumber $ leftn + rightn
        Left _ -> case (leftVal, rightVal) of
          (SomeValue (VStr lefts), SomeValue (VStr rights)) -> return $ SomeValue $ VStr $ lefts ++ rights
          _ -> throwRuntimeError $ runtimeError operator "Operands must be two numbers or two strings."
    | Map.member (tokenType operator) numericBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ SomeValue $ VNumber $ (numericBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> throwRuntimeError err
    | Map.member (tokenType operator) booleanBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ SomeValue $ VBoolean $ (booleanBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> throwRuntimeError err
    | otherwise = error "Unexpected opType when interpreting binary"
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
interpretExpr interp (Unary operator expr) = do
  (val, interp') <- interpretExpr interp expr
  case tokenType operator of
    BANG -> return (SomeValue $ VBoolean $ not $ isTruthy val, interp')
    MINUS -> case toNumber val operator of
      Right n -> return (SomeValue $ VNumber $ -n, interp')
      Left err -> throwRuntimeError err
    _ -> error "unexpected opType when interpreting unary"
interpretExpr interp (Grouping expr) = interpretExpr interp expr
interpretExpr interp expr@(Variable tok) =
  case lookupVariable interp tok expr of
    Right val -> return (val, interp)
    Left err -> throwRuntimeError err
interpretExpr interp (AndExpr left _ right) = do
  (val, interp') <- interpretExpr interp left
  if not $ isTruthy val
    then return (val, interp')
    else interpretExpr interp' right
interpretExpr interp (OrExpr left _ right) = do
  (val, interp') <- interpretExpr interp left
  if isTruthy val
    then return (val, interp')
    else interpretExpr interp' right
interpretExpr interp (Primary lit) = return (fromLiteral lit, interp)
interpretExpr interp (Get object name) = do
  (interpretedObject, interp') <- interpretExpr interp object
  getFieldOrMethod interp' name interpretedObject
interpretExpr interp (Set object name value) = do
  (interpretedObject, interp') <- interpretExpr interp object
  case interpretedObject of
    SomeValue (VInstance _ _ _ iid) -> do
      (interpretedValue, interp'') <- interpretExpr interp' value
      return (interpretedValue, setProperty iid name interpretedValue interp'')
    _ -> throwRuntimeError $ runtimeError name "Only instances have fields."
interpretExpr interp expr@(This keyword) =
  case lookupVariable interp keyword expr of
    Right value -> return (value, interp)
    Left err -> throwRuntimeError err

getFieldOrMethod :: Interpreter -> Token -> SomeValue -> InterpreterOutput (SomeValue, Interpreter)
getFieldOrMethod interp name (SomeValue inst@(VInstance _ properties methods _)) =
  case properties Map.!? lexeme name of
    Just value -> return (value, interp)
    Nothing -> case methods Map.!? lexeme name of
      Just (VFunction params leftParen fname isInitializer definingInterp func _) ->
        let interpWithThis = assignThis inst definingInterp interp
            (method, interpWithMethod) = addFunction params leftParen fname isInitializer definingInterp func interpWithThis
         in return (SomeValue method, restoreRunningEnv interp interpWithMethod)
      Nothing -> throwRuntimeError $ runtimeError name $ "Undefined property '" ++ lexeme name ++ "'."
getFieldOrMethod _ name _ = throwRuntimeError $ runtimeError name "Only instances have properties."

toNumberPair :: SomeValue -> SomeValue -> Token -> Either String (Double, Double)
toNumberPair left right op = case (toNumber left op, toNumber right op) of
  (Right l, Right r) -> Right (l, r)
  _ -> Left $ runtimeError op "Operands must be numbers."

toNumber :: SomeValue -> Token -> Either String Double
toNumber (SomeValue (VNumber n)) _ = Right n
toNumber _ token = Left $ runtimeError token "Operand must be a number."

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
addFunction params leftParen fname isInitializer definingInterp func interpreter =
  let fc = (getFuncCounter . funcCounter $ interpreter)
   in case fc Map.!? (params, leftParen, fname, isInitializer) of
        Nothing ->
          let newFc = FuncCounter $ Map.insert (params, leftParen, fname, isInitializer) (FunctionID 0) fc
           in ( VFunction params leftParen fname isInitializer definingInterp func $ FunctionID 0
              , interpreter{funcCounter = newFc}
              )
        Just (FunctionID count) ->
          let newFc = FuncCounter $ Map.insert (params, leftParen, fname, isInitializer) (FunctionID $ count + 1) fc
           in ( VFunction params leftParen fname isInitializer definingInterp func $ FunctionID $ count + 1
              , interpreter{funcCounter = newFc}
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
      clockFunc interp _ = do
        time <- liftIO getPOSIXTime
        return (SomeValue $ VNumber (realToFrac time :: Double), interp)
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

assignTok :: Interpreter -> Token -> SomeValue -> Either String Interpreter
assignTok interpreter token value =
  case environmentTable interpreter Map.! currentEnvironment interpreter of
    (Environment envTable parent)
      | Map.member (lexeme token) envTable ->
          let assignedTable = Map.insert (lexeme token) value envTable
              assignedEnv = Environment assignedTable parent
           in Right
                interpreter
                  { environmentTable =
                      Map.insert
                        (currentEnvironment interpreter)
                        assignedEnv
                        (environmentTable interpreter)
                  }
      | Just pEnv <- parent -> do
          assignedInParent <- assignTok interpreter{currentEnvironment = pEnv} token value
          return $ interpreter{environmentTable = environmentTable assignedInParent}
      | Nothing <- parent ->
          Left $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

assignThis :: Value 'ValueInstance -> Interpreter -> Interpreter -> Interpreter
assignThis value (Interpreter _ _ thisInterp _ _ _) interpreter =
  let Environment variables parent = case environmentTable interpreter Map.!? thisInterp of
        Nothing -> error "here"
        Just e -> e
      env = Environment (Map.insert "this" (SomeValue value) variables) parent
   in interpreter{environmentTable = Map.insert thisInterp env $ environmentTable interpreter}

assign :: Interpreter -> Expr -> Token -> SomeValue -> Either String Interpreter
assign interpreter expr name value =
  case locals interpreter Map.!? expr of
    Nothing -> assignGlobal interpreter name value
    Just distance -> assignAt interpreter distance name value

assignAt :: Interpreter -> Int -> Token -> SomeValue -> Either String Interpreter
assignAt interpreter distance name val =
  let ancestorId = ancestor distance interpreter $ currentEnvironment interpreter
      Environment envTable parent = fromMaybe (error "in assignAt") (environmentTable interpreter Map.!? ancestorId)
      newTable =
        Map.insert
          ancestorId
          (Environment (Map.insert (lexeme name) val envTable) parent)
          (environmentTable interpreter)
   in Right interpreter{environmentTable = newTable}

assignGlobal :: Interpreter -> Token -> SomeValue -> Either String Interpreter
assignGlobal interpreter token value =
  let Environment globalTable pEnv = environmentTable interpreter Map.! EnvID 0
      newGlobalEnv = Environment (Map.insert (lexeme token) value globalTable) pEnv
   in case globalTable Map.!? lexeme token of
        Just _ ->
          Right
            interpreter
              { environmentTable = Map.insert (EnvID 0) newGlobalEnv $ environmentTable interpreter
              }
        Nothing -> Left $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

lookupVariable :: Interpreter -> Token -> Expr -> Either String SomeValue
lookupVariable interpreter name expr =
  case locals interpreter Map.!? expr of
    Nothing ->
      let Environment envTable _ = environmentTable interpreter Map.! EnvID 0
       in case envTable Map.!? lexeme name of
            Nothing -> Left (runtimeError name $ "Undefined variable '" ++ lexeme name ++ "'.")
            Just val -> Right val
    Just distance -> Right $ getAt interpreter distance $ lexeme name

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
        Just val -> val

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
setProperty iid name val interpreter =
  let mapFunc (SomeValue (VInstance className properties methods otherIid)) =
        if iid == otherIid
          then SomeValue $ VInstance className (Map.insert (lexeme name) val properties) methods otherIid
          else SomeValue $ VInstance className (Map.map mapFunc properties) methods otherIid
      mapFunc other = other
      envMap (Environment vars pID) = Environment (Map.map mapFunc vars) pID
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
