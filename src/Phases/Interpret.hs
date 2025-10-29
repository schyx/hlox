{-# LANGUAGE GADTs #-}

module Phases.Interpret (interpret, InterpretOutput, interpretExpr) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Error
import Phases.Expr
import Phases.Interpreter
import Phases.Stmt
import Tokens

type InterpretOutput a = ExceptT (Value, Interpreter) (ExceptT String IO) a

throwRuntimeError :: String -> InterpretOutput a
throwRuntimeError = lift . throwError

interpret :: Interpreter -> SomeStmt -> InterpretOutput Interpreter
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
interpret interp func@(SomeStmt (Function fname _ _)) =
  let (funcObj, interp') = functionToValue interp False func
   in return $ define interp' fname funcObj
interpret interp (SomeStmt (Return _ returnValue)) =
  case returnValue of
    Nothing -> throwError (VNil, interp)
    Just value -> do
      (val, interp') <- interpretExpr interp value
      throwError (val, interp')
interpret interp (SomeStmt (Class name classMethods)) =
  let interp' = define interp name VNil
      foldFunc func@(Function fname _ _) =
        Map.insert (lexeme fname) (fst $ functionToValue (createChildEnv interp) (lexeme fname == "init") (SomeStmt func))
      methods = foldr foldFunc Map.empty classMethods
      arity = case methods Map.!? "init" of
        Nothing -> 0
        Just (VFunction args _ _ _ _ _ _) -> length args
        Just _ -> error "bruh"
      klass = VClass (lexeme name) arity methods
   in case assignTok interp' name klass of
        Right interp'' -> return (restoreRunningEnv interp'' $ createChildEnv interp'')
        Left err -> throwRuntimeError err

functionToValue :: Interpreter -> Bool -> SomeStmt -> (Value, Interpreter)
functionToValue interp isInitializer (SomeStmt (Function fname params body)) =
  let functionF interpreter args = do
        let enclosingInterp = restoreRunningEnv interp interpreter
        let fInterpInitial = createChildEnv enclosingInterp
        let fInterp = foldr (\(tok, val) prevInterp -> define prevInterp tok val) fInterpInitial (zip params args) -- TODO: maybe this needs to be foldl?
        run <- runExceptT $ execBlock fInterp body
        case run of
          Left (val, outputInterp) -> ExceptT $ return $ Right (val, outputInterp)
          Right outputInterp -> ExceptT $ return $ Right (VNil, outputInterp)
   in addFunction params fname ("<fn " ++ lexeme fname ++ ">") isInitializer interp functionF interp
functionToValue _ _ _ = error "Called this in the wrong place"

execBlock :: Interpreter -> [SomeStmt] -> InterpretOutput Interpreter
execBlock = foldM interpret

interpretExpr :: Interpreter -> Expr -> InterpretOutput (Value, Interpreter)
interpretExpr interp (Call callee paren argExprs) = do
  (val, callerInterp) <- interpretExpr interp callee
  (args, argsInterp) <- interpretExprs callerInterp argExprs
  case val of
    function@VFunction{} -> callFunction argsInterp args function
    (VClass className _ classMethods) ->
      let (inst, instInterp) = newInstance className classMethods argsInterp
       in case classMethods Map.!? "init" of
            Nothing ->
              if null args
                then return (inst, instInterp)
                else throwRuntimeError $ runtimeError paren $ "Expected 0 arguments but got " ++ show (length args) ++ "."
            Just initializer@(VFunction _ _ _ _ funcInterp _ _) ->
              callFunction (assignThis inst funcInterp instInterp) args initializer
            Just _ -> error "buhhhhh"
    _ -> throwRuntimeError $ runtimeError paren "Can only call functions and classes."
 where
  callFunction :: Interpreter -> [Value] -> Value -> InterpretOutput (Value, Interpreter)
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
  callFunction _ _ _ = error "calling wrong"
  interpretExprs :: Interpreter -> [Expr] -> InterpretOutput ([Value], Interpreter)
  interpretExprs argsInterp [] = return ([], argsInterp)
  interpretExprs argsInterp (expr : exprs) = do
    (val, interp') <- interpretExpr argsInterp expr
    (params, afterParamsInterp) <- interpretExprs interp' exprs
    return (val : params, afterParamsInterp)
interpretExpr interp expr@(Assign _ value) = do
  (val, interp') <- interpretExpr interp value
  case assign interp' expr val of
    Right assignedInterp -> return (val, assignedInterp)
    Left err -> throwRuntimeError err
interpretExpr interp (Binary left operator right) = do
  (leftVal, afterLeftInterp) <- interpretExpr interp left
  (rightVal, afterRightInterp) <- interpretExpr afterLeftInterp right
  output <- getOutput leftVal rightVal
  return (output, afterRightInterp)
 where
  getOutput :: Value -> Value -> InterpretOutput Value
  getOutput leftVal rightVal
    | tokenType operator `elem` [BANG_EQUAL, EQUAL_EQUAL] =
        return $
          VBoolean
            ( if tokenType operator == EQUAL_EQUAL
                then leftVal == rightVal
                else leftVal /= rightVal
            )
    | tokenType operator == PLUS = case toNumberPair leftVal rightVal operator of
        Right (leftn, rightn) -> return $ VNumber $ leftn + rightn
        Left _ -> case (leftVal, rightVal) of
          (VStr lefts, VStr rights) -> return $ VStr $ lefts ++ rights
          _ -> throwRuntimeError $ runtimeError operator "Operands must be two numbers or two strings."
    | Map.member (tokenType operator) numericBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ VNumber $ (numericBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> throwRuntimeError err
    | Map.member (tokenType operator) booleanBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ VBoolean $ (booleanBinaryTable Map.! tokenType operator) leftn rightn
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
    BANG -> return (VBoolean $ not $ isTruthy val, interp')
    MINUS -> case toNumber val operator of
      Right n -> return (VNumber $ -n, interp')
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
    VInstance _ _ _ iid -> do
      (interpretedValue, interp'') <- interpretExpr interp' value
      return (interpretedValue, setProperty iid name interpretedValue interp'')
    _ -> throwRuntimeError $ runtimeError name "Only instances have fields."
interpretExpr interp expr@(This keyword) =
  case lookupVariable interp keyword expr of
    Right value -> return (value, interp)
    Left err -> throwRuntimeError err

getFieldOrMethod :: Interpreter -> Token -> Value -> InterpretOutput (Value, Interpreter)
getFieldOrMethod interp name inst@(VInstance _ properties methods _) =
  case properties Map.!? lexeme name of
    Just value -> return (value, interp)
    Nothing -> case methods Map.!? lexeme name of
      Just (VFunction params leftParen fname isInitializer definingInterp func _) ->
        let interpWithThis = assignThis inst definingInterp interp
            (method, interpWithMethod) = addFunction params leftParen fname isInitializer definingInterp func interpWithThis
         in return (method, restoreRunningEnv interp interpWithMethod)
      Just _ -> error "Should not happen"
      Nothing -> throwRuntimeError $ runtimeError name $ "Undefined property '" ++ lexeme name ++ "'."
getFieldOrMethod _ name _ = throwRuntimeError $ runtimeError name "Only instances have properties."

toNumberPair :: Value -> Value -> Token -> Either String (Double, Double)
toNumberPair left right op = case (toNumber left op, toNumber right op) of
  (Right l, Right r) -> Right (l, r)
  _ -> Left $ runtimeError op "Operands must be numbers."

toNumber :: Value -> Token -> Either String Double
toNumber (VNumber n) _ = Right n
toNumber _ token = Left $ runtimeError token "Operand must be a number."

isTruthy :: Value -> Bool
isTruthy VNil = False
isTruthy (VBoolean b) = b
isTruthy _ = True
