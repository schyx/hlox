module Phases.Expr (Expr (..)) where

import Tokens

data Expr
  = Assign Token Expr
  | Binary Expr Token Expr -- ==, >, *, +
  | Grouping Expr
  | Unary Token Expr -- ! or -
  | Primary Literal -- number or string or bool or nil
  | Variable Token
  | OrExpr Expr Token Expr
  | AndExpr Expr Token Expr
  | Call Expr Token [Expr] -- caller, paren, args
  | Get Expr Token
  | Set Expr Token Expr
  | This Token
  deriving (Show, Eq, Ord)
