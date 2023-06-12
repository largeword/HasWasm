{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module HasWasm.Internal  (
  I32(..), F32(..),
  StackT(..),
  Stack,
  (:+),
  Instr(..),
  BinOp(..),
  UnOp(..),
  LabelId,
  TypedInstr(..),
  TypedLabel(..),
  (#),
  untype
) where

import Data.Kind (Constraint, Type)

data I32 = I32
data F32 = F32

type family BaseType (a :: Type) :: Constraint where
  BaseType I32 = ()
  BaseType F32 = ()

data StackT s t where
  Empty :: StackT () ()
  Cons :: (Stack s, BaseType t) => s -> t -> StackT s t

type s :+ t = StackT s t
infixl 2 :+

type family Stack (st :: Type) :: Constraint where
  Stack (StackT s t) = ()

{- Types -}

data BinOp = ADD | SUB | MUL | DIV
data UnOp = NEG

data Instr =
  Sequence [Instr] |
  I32Const Int |
  I32Binary BinOp |
  I32Unary UnOp |
  F32Const Float |
  F32Binary BinOp |
  F32Unary UnOp |
  Block (LabelId -> Instr) |
  Branch LabelId |
  BranchIf LabelId |
  Call

newtype (Stack a, Stack b) => TypedInstr a b = TypedInstr Instr

type LabelId = Int
newtype (Stack s) => TypedLabel s = TypedLabel LabelId

join :: Instr -> Instr -> Instr
join (Sequence s1) (Sequence s2) = Sequence (s1 ++ s2)
join (Sequence s) i = Sequence (s ++ [i])
join i (Sequence s) = Sequence (i : s)
join i1 i2 = Sequence [i1, i2]

(#) :: (Stack a, Stack b, Stack c) => TypedInstr a b -> TypedInstr b c -> TypedInstr a c
(TypedInstr i1) # (TypedInstr i2) = TypedInstr (join i1 i2)

infixl 0 #

untype :: TypedInstr a b -> Instr
untype (TypedInstr i) = i

-- data (StackType a, StackType b) => FuncType a b = FuncType (TypedInstr a b)

-- newtype (BaseType t) => Param t = Param Int
-- newtype (BaseType t) => Var t = Var Int
-- newtype (BaseType t) => Return t = Return Int

-- fibo :: Param I32 -> Var I32 -> Return I32 -> TypedInstr s (s :+ I32)
-- fibo = undefined
