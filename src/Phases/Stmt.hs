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

data Stmt (k :: StmtKind)
  = Expression Expr
  | Print Expr
  | Var Token Expr
  | Block [SomeStmt]
  | If Expr SomeStmt (Maybe SomeStmt)
  | While Expr SomeStmt
  | Function Token [Token] [SomeStmt]
  | Return Token Expr
  | Class Token [Stmt 'KFunction]
