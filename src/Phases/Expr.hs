module Phases.Expr (Expr(..)) where

import Tokens

data Expr
  = Assign Token Expr
  | Binary Expr Token Expr -- ==, >, *, +
  | Grouping Expr
  | Unary Token Expr -- ! or -
  | Primary Literal -- number or string or bool or nil
  | Variable Token
  deriving (Show, Eq)
