{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Phases.Expr (Expr (..), ExprKind (..), SomeExpr (..)) where

import Tokens

data ExprKind
  = ExprAssign
  | ExprBinary
  | ExprGrouping
  | ExprUnary
  | ExprPrimary
  | ExprVariable
  | ExprOr
  | ExprAnd
  | ExprCall
  | ExprGet
  | ExprSet
  | ExprSuper
  | ExprThis

deriving instance Eq ExprKind
deriving instance Ord ExprKind

data SomeExpr where
  SomeExpr :: Expr e -> SomeExpr

instance Eq SomeExpr where
  (SomeExpr a@Assign{}) == (SomeExpr b@Assign{}) = a == b
  (SomeExpr a@Binary{}) == (SomeExpr b@Binary{}) = a == b
  (SomeExpr a@Grouping{}) == (SomeExpr b@Grouping{}) = a == b
  (SomeExpr a@Unary{}) == (SomeExpr b@Unary{}) = a == b
  (SomeExpr a@Primary{}) == (SomeExpr b@Primary{}) = a == b
  (SomeExpr a@Variable{}) == (SomeExpr b@Variable{}) = a == b
  (SomeExpr a@OrExpr{}) == (SomeExpr b@OrExpr{}) = a == b
  (SomeExpr a@AndExpr{}) == (SomeExpr b@AndExpr{}) = a == b
  (SomeExpr a@Call{}) == (SomeExpr b@Call{}) = a == b
  (SomeExpr a@Get{}) == (SomeExpr b@Get{}) = a == b
  (SomeExpr a@Set{}) == (SomeExpr b@Set{}) = a == b
  (SomeExpr a@Super{}) == (SomeExpr b@Super{}) = a == b
  (SomeExpr a@This{}) == (SomeExpr b@This{}) = a == b
  _ == _ = False

instance Ord SomeExpr where
  compare (SomeExpr a@AndExpr{}) (SomeExpr b@AndExpr{}) = compare a b
  compare (SomeExpr AndExpr{}) _ = LT
  compare _ (SomeExpr AndExpr{}) = GT
  compare (SomeExpr a@Assign{}) (SomeExpr b@Assign{}) = compare a b
  compare (SomeExpr Assign{}) _ = LT
  compare _ (SomeExpr Assign{}) = GT
  compare (SomeExpr a@Binary{}) (SomeExpr b@Binary{}) = compare a b
  compare (SomeExpr Binary{}) _ = LT
  compare _ (SomeExpr Binary{}) = GT
  compare (SomeExpr a@Call{}) (SomeExpr b@Call{}) = compare a b
  compare (SomeExpr Call{}) _ = LT
  compare _ (SomeExpr Call{}) = GT
  compare (SomeExpr a@Get{}) (SomeExpr b@Get{}) = compare a b
  compare (SomeExpr Get{}) _ = LT
  compare _ (SomeExpr Get{}) = GT
  compare (SomeExpr a@Grouping{}) (SomeExpr b@Grouping{}) = compare a b
  compare (SomeExpr Grouping{}) _ = LT
  compare _ (SomeExpr Grouping{}) = GT
  compare (SomeExpr a@OrExpr{}) (SomeExpr b@OrExpr{}) = compare a b
  compare (SomeExpr OrExpr{}) _ = LT
  compare _ (SomeExpr OrExpr{}) = GT
  compare (SomeExpr a@Primary{}) (SomeExpr b@Primary{}) = compare a b
  compare (SomeExpr Primary{}) _ = LT
  compare _ (SomeExpr Primary{}) = GT
  compare (SomeExpr a@Set{}) (SomeExpr b@Set{}) = compare a b
  compare (SomeExpr Set{}) _ = LT
  compare _ (SomeExpr Set{}) = GT
  compare (SomeExpr a@Super{}) (SomeExpr b@Super{}) = compare a b
  compare (SomeExpr Super{}) _ = LT
  compare _ (SomeExpr Super{}) = GT
  compare (SomeExpr a@This{}) (SomeExpr b@This{}) = compare a b
  compare (SomeExpr This{}) _ = LT
  compare _ (SomeExpr This{}) = GT
  compare (SomeExpr a@Unary{}) (SomeExpr b@Unary{}) = compare a b
  compare (SomeExpr Unary{}) _ = LT
  compare _ (SomeExpr Unary{}) = GT
  compare (SomeExpr a@Variable{}) (SomeExpr b@Variable{}) = compare a b

data Expr (e :: ExprKind) where
  AndExpr :: SomeExpr -> Token -> SomeExpr -> Expr 'ExprAnd
  Assign :: Token -> SomeExpr -> Expr 'ExprAssign
  Binary :: SomeExpr -> Token -> SomeExpr -> Expr 'ExprBinary
  Call :: SomeExpr -> Token -> [SomeExpr] -> Expr 'ExprCall
  Get :: SomeExpr -> Token -> Expr 'ExprGet
  Grouping :: SomeExpr -> Expr 'ExprGrouping
  OrExpr :: SomeExpr -> Token -> SomeExpr -> Expr 'ExprOr
  Primary :: Literal -> Expr 'ExprPrimary
  Set :: SomeExpr -> Token -> SomeExpr -> Expr 'ExprSet
  Super :: Token -> Token -> Expr 'ExprSuper
  This :: Token -> Expr 'ExprThis
  Unary :: Token -> SomeExpr -> Expr 'ExprUnary
  Variable :: Token -> Expr 'ExprVariable

instance Ord (Expr e) where
  compare (AndExpr l1 t1 r1) (AndExpr l2 t2 r2) = compare (l1, t1, r1) (l2, t2, r2)
  compare (Assign t1 se1) (Assign t2 se2) = compare (t1, se1) (t2, se2)
  compare (Binary l1 t1 r1) (Binary l2 t2 r2) = compare (l1, t1, r1) (l2, t2, r2)
  compare (Call c1 p1 args1) (Call c2 p2 args2) = compare (c1, p1, args1) (c2, p2, args2)
  compare (Get obj1 name1) (Get obj2 name2) = compare (obj1, name1) (obj2, name2)
  compare (Grouping se1) (Grouping se2) = compare se1 se2
  compare (OrExpr l1 t1 r1) (OrExpr l2 t2 r2) = compare (l1, t1, r1) (l2, t2, r2)
  compare (Primary lit1) (Primary lit2) = compare lit1 lit2
  compare (Set obj1 name1 val1) (Set obj2 name2 val2) = compare (obj1, name1, val1) (obj2, name2, val2)
  compare (Super t1 m1) (Super t2 m2) = compare (t1, m1) (t2, m2)
  compare (This t1) (This t2) = compare t1 t2
  compare (Unary t1 se1) (Unary t2 se2) = compare (t1, se1) (t2, se2)
  compare (Variable t1) (Variable t2) = compare t1 t2

instance Eq (Expr e) where
  (Assign t1 se1) == (Assign t2 se2) = (t1, se1) == (t2, se2)
  (Binary l1 t1 r1) == (Binary l2 t2 r2) = (l1, t1, r1) == (l2, t2, r2)
  (Grouping se1) == (Grouping se2) = se1 == se2
  (Unary t1 se1) == (Unary t2 se2) = (t1, se1) == (t2, se2)
  (Primary lit1) == (Primary lit2) = lit1 == lit2
  (Variable t1) == (Variable t2) = t1 == t2
  (OrExpr l1 t1 r1) == (OrExpr l2 t2 r2) = (l1, t1, r1) == (l2, t2, r2)
  (AndExpr l1 t1 r1) == (AndExpr l2 t2 r2) = (l1, t1, r1) == (l2, t2, r2)
  (Call callee1 paren1 args1) == (Call callee2 paren2 args2) = (callee1, paren1, args1) == (callee2, paren2, args2)
  (Get obj1 name1) == (Get obj2 name2) = (obj1, name1) == (obj2, name2)
  (Set obj1 name1 val1) == (Set obj2 name2 val2) = (obj1, name1, val1) == (obj2, name2, val2)
  (Super keyword1 method1) == (Super keyword2 method2) = (keyword1, method1) == (keyword2, method2)
  (This keyword1) == (This keyword2) = keyword1 == keyword2
