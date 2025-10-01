module Phases.Interpreter (interpret, InterpreterOutput, interpretExpr) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Error
import Phases.Environment
import Phases.Expr
import Phases.Stmt
import Tokens

type InterpretExprResult = ExceptT String IO (Value, Environment)

type InterpreterOutput = ExceptT String IO Environment

interpret :: Environment -> Stmt -> InterpreterOutput
interpret env (Print expr) = do
  (val, newEnv) <- interpretExpr env expr
  liftIO $ print val
  return newEnv
interpret env (Expression expr) = do
  (_, newEnv) <- interpretExpr env expr
  return newEnv
interpret env (Var name initializer) = do
  (val, newEnv) <- interpretExpr env initializer
  return $ define newEnv name val
interpret env (Block stmts) = do
  let blockEnv = envWithParent env
  newBlockEnv <- execBlock blockEnv stmts
  return $ getParent newBlockEnv
interpret env (If condition ifBranch (Just elseBranch)) = do
  (val, newEnv) <- interpretExpr env condition
  interpret newEnv (if isTruthy val then ifBranch else elseBranch)
interpret env (If condition ifBranch Nothing) = do
  (val, newEnv) <- interpretExpr env condition
  if isTruthy val then interpret newEnv ifBranch else return newEnv
interpret env (While condition whileBlock) = do
  (val, newEnv) <- interpretExpr env condition
  if isTruthy val
    then do
      afterStmtEnv <- interpret newEnv whileBlock
      interpret afterStmtEnv (While condition whileBlock)
    else return newEnv
interpret env (Function fname params body) = do
  let functionVal = VFunction (length params) fname ("<fn " ++ lexeme fname ++ ">")
  let functionF enclosing args = do
        let fEnvInitial = envWithParent enclosing
        let fEnv = foldr (\(tok, val) prevEnv -> define prevEnv tok val) fEnvInitial (zip params args)
        outputEnv <- execBlock fEnv body
        return (VNil, getParent outputEnv)
  return $ assignFunc env fname functionVal functionF

execBlock :: Environment -> [Stmt] -> ExceptT String IO Environment
execBlock = foldM interpret

interpretExpr :: Environment -> Expr -> InterpretExprResult
interpretExpr env (Call callee paren args) = do
  (val, newEnv) <- interpretExpr env callee
  (params, afterParamsEnv) <- interpretExprs newEnv args
  case val of
    c@(VFunction arity _ _) ->
      if arity == length params
        then call afterParamsEnv c params
        else
          throwError
            $ runtimeError
              paren
            $ "Expected " ++ show arity ++ " arguments but got " ++ show (length params) ++ "."
    _ -> throwError $ runtimeError paren "Can only call functions and classes."
 where
  interpretExprs :: Environment -> [Expr] -> ExceptT String IO ([Value], Environment)
  interpretExprs argsEnv [] = return ([], argsEnv)
  interpretExprs argsEnv (expr : exprs) = do
    (val, newEnv) <- interpretExpr argsEnv expr
    (params, afterParamsEnv) <- interpretExprs newEnv exprs
    return (val : params, afterParamsEnv)
interpretExpr env (Assign name value) = do
  (val, newEnv) <- interpretExpr env value
  case assign newEnv name val of
    (Right _, assignedEnv) -> return (val, assignedEnv)
    (Left err, _) -> throwError err
interpretExpr env (Binary left operator right) = do
  (leftVal, afterLeftEnv) <- interpretExpr env left
  (rightVal, afterRightEnv) <- interpretExpr afterLeftEnv right
  output <- getOutput leftVal rightVal
  return (output, afterRightEnv)
 where
  getOutput :: Value -> Value -> ExceptT String IO Value
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
          _ -> throwError $ runtimeError operator "Operands must be two numbers or two strings."
    | Map.member (tokenType operator) numericBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ VNumber $ (numericBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> throwError err
    | Map.member (tokenType operator) booleanBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ VBoolean $ (booleanBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> throwError err
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
interpretExpr env (Unary operator expr) = do
  (val, newEnv) <- interpretExpr env expr
  case tokenType operator of
    BANG -> return (VBoolean $ not $ isTruthy val, newEnv)
    MINUS -> case toNumber val operator of
      Right n -> return (VNumber $ -n, newEnv)
      Left err -> throwError err
    _ -> error "unexpected opType when interpreting unary"
interpretExpr env (Grouping expr) = interpretExpr env expr
interpretExpr env (Variable tok) =
  case get env tok of
    Right val -> return (val, env)
    Left err -> throwError err
interpretExpr env (AndExpr left _ right) = do
  (val, newEnv) <- interpretExpr env left
  if not $ isTruthy val
    then return (val, newEnv)
    else interpretExpr newEnv right
interpretExpr env (OrExpr left _ right) = do
  (val, newEnv) <- interpretExpr env left
  if isTruthy val
    then return (val, newEnv)
    else interpretExpr newEnv right
interpretExpr env (Primary lit) = return (fromLiteral lit, env)

call :: Environment -> Value -> [Value] -> InterpretExprResult
call env callee args =
  let func = getFunc env callee
   in func env args

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
