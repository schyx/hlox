{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Phases.Stmt (Stmt (..), StmtKind (..), SomeStmt (..)) where

import Phases.Expr
import Tokens

data StmtKind
  = KExpression
  | KPrint
  | KVar
  | KBlock
  | KIf
  | KWhile
  | KFunction
  | KReturn
  | KClass

data SomeStmt where
  SomeStmt :: Stmt k -> SomeStmt

data Stmt (k :: StmtKind) where
  Expression :: SomeExpr -> Stmt 'KExpression
  Print :: SomeExpr -> Stmt 'KPrint
  Var :: Token -> SomeExpr -> Stmt 'KVar
  Block :: [SomeStmt] -> Stmt 'KBlock
  If :: SomeExpr -> SomeStmt -> Maybe SomeStmt -> Stmt 'KIf
  While :: SomeExpr -> SomeStmt -> Stmt 'KWhile
  Function :: Token -> [Token] -> [SomeStmt] -> Stmt 'KFunction
  Return :: Token -> Maybe SomeExpr -> Stmt 'KReturn
  Class :: Token -> Maybe (Expr 'ExprVariable) -> [Stmt 'KFunction] -> Stmt 'KClass
