module Phases.Interpreter (interpret, InterpreterOutput, interpretExpr) where

import qualified Data.Map as Map
import Error
import Phases.Environment
import Phases.Expr
import Phases.Stmt
import Tokens

type InterpreterOutput = Either String ()

type InterpretExprResult = (Either String Literal, Environment)

interpret :: Environment -> Stmt -> IO (Environment, InterpreterOutput)
interpret env (Print expr) = case interpretExpr env expr of
  (Left err, newInterp) -> return (newInterp, Left err)
  (Right lit, newInterp) -> print lit >> return (newInterp, Right ())
interpret env (Expression expr) = case interpretExpr env expr of
  (Left err, newInterp) -> return (newInterp, Left err)
  (Right _, newInterp) -> return (newInterp, Right ())
interpret env (Var name initializer) =
  case interpretExpr env initializer of
    (Left err, newInterp) -> return (newInterp, Left err)
    (Right val, newEnv) -> return (define newEnv name val, Right ())
interpret env (Block stmts) = do
  let blockEnv = envWithParent env
  (newBlockEnv, output) <- execBlock blockEnv stmts
  let newEnv = getParent newBlockEnv
  case output of
    Right () -> return (newEnv, Right ())
    Left err -> return (newEnv, Left err)
  where
    execBlock :: Environment -> [Stmt] -> IO (Environment, InterpreterOutput)
    execBlock blockEnv (s : sOther) = do
      (newBlockEnv, output) <- interpret blockEnv s
      case output of
        Right () -> execBlock newBlockEnv sOther
        Left err -> return (newBlockEnv, Left err)
    execBlock blockEnv [] = return (blockEnv, Right ())

interpretExpr :: Environment -> Expr -> InterpretExprResult
interpretExpr interp (Assign name value) =
  case interpretExpr interp value of
    (Right lit, newEnv) -> case assign newEnv name lit of
      (Right _, assignedEnv) -> (Right lit, assignedEnv)
      (Left err, assignedEnv) -> (Left err, assignedEnv)
    err -> err
interpretExpr interp (Binary left operator right) =
  case interpretExpr interp left of
    (Left err, afterLeftInterp) -> (Left err, afterLeftInterp)
    (Right leftLiteral, afterLeftInterp) -> case interpretExpr afterLeftInterp right of
      (Left err, afterRightInterp) -> (Left err, afterRightInterp)
      (Right rightLiteral, afterRightInterp) -> (first, afterRightInterp)
        where
          first
            | tokenType operator `elem` [BANG_EQUAL, EQUAL_EQUAL] =
                Right $
                  Boolean
                    ( if tokenType operator == EQUAL_EQUAL
                        then leftLiteral == rightLiteral
                        else leftLiteral /= rightLiteral
                    )
            | tokenType operator == PLUS = case toNumberPair leftLiteral rightLiteral operator of
                Right (leftn, rightn) -> Right $ Number $ leftn + rightn
                Left _ -> case (leftLiteral, rightLiteral) of
                  (Str lefts, Str rights) -> Right $ Str $ lefts ++ rights
                  _ -> Left $ runtimeError operator "Operands must be two numbers or two strings."
            | Map.member (tokenType operator) numericBinaryTable =
                case toNumberPair leftLiteral rightLiteral operator of
                  Right (leftn, rightn) ->
                    Right $ Number $ (numericBinaryTable Map.! tokenType operator) leftn rightn
                  Left err -> Left err
            | Map.member (tokenType operator) booleanBinaryTable =
                case toNumberPair leftLiteral rightLiteral operator of
                  Right (leftn, rightn) ->
                    Right $ Boolean $ (booleanBinaryTable Map.! tokenType operator) leftn rightn
                  Left err -> Left err
            | otherwise = error "Unexpected opType when interpreting binary"
          booleanBinaryTable =
            Map.fromList
              [ (LESS, (<)),
                (LESS_EQUAL, (<=)),
                (GREATER, (>)),
                (GREATER_EQUAL, (>=))
              ]
          numericBinaryTable =
            Map.fromList
              [ (STAR, (*)),
                (SLASH, (/)),
                (MINUS, (-))
              ]
interpretExpr interp (Unary operator expr) = case interpretExpr interp expr of
  (Right lit, newInterp) -> case tokenType operator of
    BANG -> (Right $ Boolean $ not $ isTruthy lit, newInterp)
    MINUS -> case toNumber lit operator of
      Right n -> (Right $ Number $ -n, newInterp)
      Left err -> (Left err, newInterp)
    _ -> error "unexpected opType when interpreting unary"
  (Left err, newInterp) -> (Left err, newInterp)
interpretExpr interp (Grouping expr) = interpretExpr interp expr
interpretExpr env (Variable tok) =
  ( case get env tok of
      Right lit -> Right lit
      Left err -> Left err,
    env
  )
interpretExpr interp (Primary lit) = (Right lit, interp)

toNumberPair :: Literal -> Literal -> Token -> Either String (Double, Double)
toNumberPair left right op = case (toNumber left op, toNumber right op) of
  (Right l, Right r) -> Right (l, r)
  _ ->Left $ runtimeError op "Operands must be numbers."

toNumber :: Literal -> Token -> Either String Double
toNumber (Number n) _ = Right n
toNumber _ token = Left $ runtimeError token "Operand must be a number."

isTruthy :: Literal -> Bool
isTruthy Nil = False
isTruthy (Boolean b) = b
isTruthy _ = True
