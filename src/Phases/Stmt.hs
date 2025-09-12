module Phases.Stmt (Stmt(..)) where

import Phases.Expr

data Stmt
  = Expression Expr
  | Print Expr
