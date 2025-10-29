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
  Expression :: Expr -> Stmt 'KExpression
  Print :: Expr -> Stmt 'KPrint
  Var :: Token -> Expr -> Stmt 'KVar
  Block :: [SomeStmt] -> Stmt 'KBlock
  If :: Expr -> SomeStmt -> Maybe SomeStmt -> Stmt 'KIf
  While :: Expr -> SomeStmt -> Stmt 'KWhile
  Function :: Token -> [Token] -> [SomeStmt] -> Stmt 'KFunction
  Return :: Token -> Maybe Expr -> Stmt 'KReturn
  Class :: Token -> [Stmt 'KFunction] -> Stmt 'KClass
