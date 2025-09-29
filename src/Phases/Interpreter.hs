module Phases.Interpreter (interpret, InterpreterOutput, interpretExpr) where

import qualified Data.Map           as Map
import           Error
import           Phases.Environment
import           Phases.Expr
import           Phases.Stmt
import           Tokens

type InterpreterOutput = Either String ()

type InterpretExprResult = (Either String Value, Environment)

interpret :: Environment -> Stmt -> IO (Environment, InterpreterOutput)
interpret env (Print expr) = case interpretExpr env expr of
  (Left err, newInterp)  -> return (newInterp, Left err)
  (Right lit, newInterp) -> print lit >> return (newInterp, Right ())
interpret env (Expression expr) = case interpretExpr env expr of
  (Left err, newInterp) -> return (newInterp, Left err)
  (Right _, newInterp)  -> return (newInterp, Right ())
interpret env (Var name initializer) =
  case interpretExpr env initializer of
    (Left err, newInterp) -> return (newInterp, Left err)
    (Right val, newEnv)   -> return (define newEnv name val, Right ())
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
interpret env (If condition ifBranch (Just elseBranch)) =
  case interpretExpr env condition of
    (Right lit, newEnv) -> interpret newEnv (if isTruthy lit then ifBranch else elseBranch)
    (Left err, newEnv) -> return (newEnv, Left err)
interpret env (If condition ifBranch Nothing) =
  case interpretExpr env condition of
    (Right lit, newEnv) -> if isTruthy lit
      then interpret newEnv ifBranch
      else return (newEnv, Right ())
    (Left err, newEnv) -> return (newEnv, Left err)
interpret env (While condition whileBlock) =
  case interpretExpr env condition of
    (Right lit, newEnv) -> if isTruthy lit
      then do
        (afterStmtEnv, Right ()) <- interpret newEnv whileBlock
        interpret afterStmtEnv (While condition whileBlock)
      else return (newEnv, Right ())
    (Left err, newEnv) -> return (newEnv, Left err)

interpretExpr :: Environment -> Expr -> InterpretExprResult
interpretExpr env (Call callee paren args) =
  case interpretExpr env callee of
    (Right lit, newEnv) -> case interpretExprs newEnv args of
      (Right interpArgs, afterArgsEnv) -> case lit of
        c@(VCall arity _) -> if arity == length interpArgs
          then call env c interpArgs
          else
          (
            Left
              $ runtimeError
              paren
              $ "Expected " ++ show arity ++ " arguments but got " ++ show (length interpArgs) ++ ".",
            afterArgsEnv
          )
        _ -> (Left $ runtimeError paren "Can only call functions and classes.", afterArgsEnv)
      (Left err, errEnv)               -> (Left err, errEnv)
    err -> err
  where
    interpretExprs :: Environment -> [Expr] -> (Either String [Value], Environment)
    interpretExprs argsEnv []             = (Right [], argsEnv)
    interpretExprs argsEnv (expr : exprs) =
      case interpretExpr argsEnv expr of
        (Right lit, exprEnv) -> case interpretExprs exprEnv exprs of
          (Right lits, exprsEnv) -> (Right $ lit : lits, exprsEnv)
          (Left err, errEnv)     -> (Left err, errEnv)
        (Left err, errEnv) -> (Left err, errEnv)
interpretExpr interp (Assign name value) =
  case interpretExpr interp value of
    (Right lit, newEnv) -> case assign newEnv name lit of
      (Right _, assignedEnv)  -> (Right lit, assignedEnv)
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
                  VBoolean
                    ( if tokenType operator == EQUAL_EQUAL
                        then leftLiteral == rightLiteral
                        else leftLiteral /= rightLiteral
                    )
            | tokenType operator == PLUS = case toNumberPair leftLiteral rightLiteral operator of
                Right (leftn, rightn) -> Right $ VNumber $ leftn + rightn
                Left _ -> case (leftLiteral, rightLiteral) of
                  (VStr lefts, VStr rights) -> Right $ VStr $ lefts ++ rights
                  _ -> Left $ runtimeError operator "Operands must be two numbers or two strings."
            | Map.member (tokenType operator) numericBinaryTable =
                case toNumberPair leftLiteral rightLiteral operator of
                  Right (leftn, rightn) ->
                    Right $ VNumber $ (numericBinaryTable Map.! tokenType operator) leftn rightn
                  Left err -> Left err
            | Map.member (tokenType operator) booleanBinaryTable =
                case toNumberPair leftLiteral rightLiteral operator of
                  Right (leftn, rightn) ->
                    Right $ VBoolean $ (booleanBinaryTable Map.! tokenType operator) leftn rightn
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
    BANG -> (Right $ VBoolean $ not $ isTruthy lit, newInterp)
    MINUS -> case toNumber lit operator of
      Right n  -> (Right $ VNumber $ -n, newInterp)
      Left err -> (Left err, newInterp)
    _ -> error "unexpected opType when interpreting unary"
  (Left err, newInterp) -> (Left err, newInterp)
interpretExpr interp (Grouping expr) = interpretExpr interp expr
interpretExpr env (Variable tok) =
  ( case get env tok of
      Right lit -> Right lit
      Left err  -> Left err,
    env
  )
interpretExpr interp (AndExpr left _ right) =
  case interpretExpr interp left of
    (Right lit, newEnv) -> if not $ isTruthy lit
      then (Right lit, newEnv)
      else interpretExpr newEnv right
    err -> err
interpretExpr interp (OrExpr left _ right) =
  case interpretExpr interp left of
    (Right lit, newEnv) -> if isTruthy lit
      then (Right lit, newEnv)
      else interpretExpr newEnv right
    err -> err
interpretExpr interp (Primary lit) = (Right $ fromLiteral lit, interp)

toNumberPair :: Value -> Value -> Token -> Either String (Double, Double)
toNumberPair left right op = case (toNumber left op, toNumber right op) of
  (Right l, Right r) -> Right (l, r)
  _                  -> Left $ runtimeError op "Operands must be numbers."

toNumber :: Value -> Token -> Either String Double
toNumber (VNumber n) _ = Right n
toNumber _ token       = Left $ runtimeError token "Operand must be a number."

isTruthy :: Value -> Bool
isTruthy VNil         = False
isTruthy (VBoolean b) = b
isTruthy _            = True
