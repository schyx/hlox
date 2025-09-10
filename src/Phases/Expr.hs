module Phases.Expr (Expr(..)) where

import Tokens

data Expr
  = Binary Expr Token Expr -- ==, >, *, +
  | Grouping Expr
  | Unary Token Expr -- ! or -
  | Primary Literal -- number or string or bool or nil
  deriving (Show, Eq)
