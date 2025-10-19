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

interpret :: Interpreter -> Stmt -> InterpretOutput Interpreter
interpret interp (Print expr) = do
  (val, interp') <- interpretExpr interp expr
  liftIO $ print val
  return interp'
interpret interp (Expression expr) = do
  (_, interp') <- interpretExpr interp expr
  return interp'
interpret interp (Var name initializer) = do
  (val, interp') <- interpretExpr interp initializer
  return $ define interp' name val
interpret interp (Block stmts) = do
  let interp' = createChildEnv interp
  interp'' <- execBlock interp' stmts
  return $ changeToParent interp''
interpret interp (If condition ifBranch (Just elseBranch)) = do
  (val, interp') <- interpretExpr interp condition
  interpret interp' (if isTruthy val then ifBranch else elseBranch)
interpret interp (If condition ifBranch Nothing) = do
  (val, interp') <- interpretExpr interp condition
  if isTruthy val then interpret interp' ifBranch else return interp'
interpret interp (While condition whileBlock) = do
  (val, interp') <- interpretExpr interp condition
  if isTruthy val
    then do
      interp'' <- interpret interp' whileBlock
      interpret interp'' (While condition whileBlock)
    else return interp'
interpret interp (Function fname params body) = do
  let functionF interpreter args = do
        let enclosingInterp = restoreRunningEnv interp interpreter
        let fInterpInitial = createChildEnv enclosingInterp
        let fInterp = foldr (\(tok, val) prevInterp -> define prevInterp tok val) fInterpInitial (zip params args)
        run <- runExceptT $ execBlock fInterp body
        case run of
          Left (val, outputInterp) -> ExceptT $ return $ Right (val, outputInterp)
          Right outputInterp -> ExceptT $ return $ Right (VNil, outputInterp)
  let functionVal = VFunction params fname ("<fn " ++ lexeme fname ++ ">") functionF
  return $ define interp fname functionVal
interpret interp (Return _ expr) = do
  (val, interp') <- interpretExpr interp expr
  throwError (val, interp')

execBlock :: Interpreter -> [Stmt] -> InterpretOutput Interpreter
execBlock = foldM interpret

interpretExpr :: Interpreter -> Expr -> InterpretOutput (Value, Interpreter)
interpretExpr interp (Call callee paren argExprs) = do
  (val, callerInterp) <- interpretExpr interp callee
  (args, argsInterp) <- interpretExprs callerInterp argExprs
  case val of
    (VFunction params _ _ func) ->
      if length params == length args
        then do
          (outputVal, outputInterp) <- lift $ func argsInterp args
          return (outputVal, restoreRunningEnv interp outputInterp)
        else
          lift . throwError
            $ runtimeError
              paren
            $ "Expected " ++ show (length params) ++ " arguments but got " ++ show (length args) ++ "."
    _ -> lift . throwError $ runtimeError paren "Can only call functions and classes."
 where
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
    Left err -> lift $ throwError err
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
          _ -> lift $ throwError $ runtimeError operator "Operands must be two numbers or two strings."
    | Map.member (tokenType operator) numericBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ VNumber $ (numericBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> lift $ throwError err
    | Map.member (tokenType operator) booleanBinaryTable =
        case toNumberPair leftVal rightVal operator of
          Right (leftn, rightn) ->
            return $ VBoolean $ (booleanBinaryTable Map.! tokenType operator) leftn rightn
          Left err -> lift $ throwError err
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
      Left err -> lift $ throwError err
    _ -> error "unexpected opType when interpreting unary"
interpretExpr interp (Grouping expr) = interpretExpr interp expr
interpretExpr interp expr@(Variable tok) =
  case lookupVariable interp tok expr of
    Right val -> return (val, interp)
    Left err -> lift $ throwError err
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
