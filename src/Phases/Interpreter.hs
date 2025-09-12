module Phases.Interpreter (interpret, mkInterpreter, InterpreterOutput, ToPrint(..), Interpreter) where

import Error
import Phases.Expr
import Phases.Stmt
import Tokens

data ToPrint = Zilch | Literal Literal

type InterpreterOutput = Either String ToPrint

type InterpretExprResult = Either String Literal

data Interpreter = MkInterpreter

mkInterpreter :: Interpreter
mkInterpreter = MkInterpreter

interpret :: Interpreter -> Stmt -> (Interpreter, InterpreterOutput)
interpret interp (Print expr) = case interpretExpr expr of
  Left err -> (interp, Left err)
  Right lit -> (interp, Right $ Literal lit)
interpret interp (Expression expr) = case interpretExpr expr of
  Left err -> (interp, Left err)
  Right _ -> (interp, Right Zilch)

interpretExpr :: Expr -> InterpretExprResult
interpretExpr (Binary left operator right) = do
  leftLiteral <- interpretExpr left
  rightLiteral <- interpretExpr right
  let opType = tokenType operator
  case opType of
    EQUAL_EQUAL -> Right $ Boolean $ leftLiteral == rightLiteral
    BANG_EQUAL -> Right $ Boolean $ leftLiteral /= rightLiteral
    LESS -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      Right $ Boolean $ leftn < rightn
    LESS_EQUAL -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      Right $ Boolean $ leftn <= rightn
    GREATER -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      Right $ Boolean $ leftn > rightn
    GREATER_EQUAL -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      Right $ Boolean $ leftn >= rightn
    STAR -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      Right $ Number $ leftn * rightn
    SLASH -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      if rightn == 0
        then Left $ runtimeError operator "Dividing by 0."
        else Right $ Number $ leftn / rightn
    MINUS -> do
      (leftn, rightn) <- toNumberPair leftLiteral rightLiteral operator
      Right $ Number $ leftn - rightn
    PLUS -> case toNumberPair leftLiteral rightLiteral operator of
      Right (leftn, rightn) -> Right $ Number $ leftn + rightn
      Left _ -> case (leftLiteral, rightLiteral) of
        (Str lefts, Str rights) -> Right $ Str $ lefts ++ rights
        _ -> Left $ runtimeError operator "Operands must be two numbers or two strings."
    _ -> error "unexpected opType when interpreting Binary"
interpretExpr (Unary operator expr) = do
  lit <- interpretExpr expr
  let opType = tokenType operator
  case opType of
    BANG -> Right $ Boolean $ not $ isTruthy lit
    MINUS -> do
      n <- toNumber lit operator
      Right $ Number $ -n
    _ -> error "unexpected opType when interpreting unary"
interpretExpr (Grouping expr) = interpretExpr expr
interpretExpr (Primary lit) = Right lit

toNumberPair :: Literal -> Literal -> Token -> Either String (Double, Double)
toNumberPair left right op = do
  leftNum <- toNumber left op
  rightNum <- toNumber right op
  Right (leftNum, rightNum)

toNumber :: Literal -> Token -> Either String Double
toNumber (Number n) _ = Right n
toNumber _ token = Left $ runtimeError token "Operand must be a number."

isTruthy :: Literal -> Bool
isTruthy Nil = False
isTruthy (Boolean b) = b
isTruthy _ = True
