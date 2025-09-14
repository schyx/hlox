module Phases.Stmt (Stmt(..)) where

import Phases.Expr
import Tokens

data Stmt
  = Expression Expr
  | Print Expr
  | Var Token Expr
  | Block [Stmt]
