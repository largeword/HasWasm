{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HasWasm.Internal  (
  I32, F32,
  BaseType(..),
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
  untype,
  Var(..),
  VarTypes(..),
  WasmFunc(..),
  createFunction,
  ReturnInstr,
  FuncBody,
  FuncCallType,
) where

import Data.Kind

data I32 = I32
data F32 = F32

data TypeTag = I32T | F32T

class BaseType t where
  tag :: t -> TypeTag
  val :: t

instance BaseType I32 where
  tag _ = I32T
  val = I32

instance BaseType F32 where
  tag _ = F32T
  val = F32

data StackT s t where
  Empty :: StackT () ()
  Cons :: (Stack s, BaseType t) => s -> t -> StackT s t

type s :+ t = StackT s t
infixl 2 :+

type family Stack (s :: Type) :: Constraint where
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
  Call WasmFuncObj |
  Return |
  LocalGet Int |
  LocalSet Int

newtype (Stack a, Stack b) => TypedInstr a b = TypedInstr Instr

type LabelId = Int
newtype (Stack s) => TypedLabel s = TypedLabel LabelId

join :: Instr -> Instr -> Instr
join (Sequence s1) (Sequence s2) = Sequence (s1 ++ s2)
join (Sequence s) i = Sequence (s ++ [i])
join i (Sequence s) = Sequence (i : s)
join i1 i2 = Sequence [i1, i2]

(#) :: TypedInstr a b -> TypedInstr b c -> TypedInstr a c
(TypedInstr i1) # (TypedInstr i2) = TypedInstr (join i1 i2)

infixl 0 #

untype :: TypedInstr a b -> Instr
untype (TypedInstr i) = i

{- Function Types -}

newtype (BaseType t) => Var t = Var Int

data WasmFunc p v r where
  WasmFunc :: (VarTypes p, VarTypes v, VarTypes r) => (p, v, r) -> WasmFuncObj -> WasmFunc p v r

data WasmFuncObj = WasmFuncObj String [TypeTag] [TypeTag] [TypeTag] Instr

type FuncBody r s = TypedInstr s (StackType r s)
type ReturnInstr r s = forall s2. TypedInstr (StackType r s) s2
type FuncCallType p r s = TypedInstr (StackType p s) (StackType r s)

createFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (VarSet p -> VarSet v -> ReturnInstr r (StackT s t) -> FuncBody r (StackT s t)) -> WasmFunc p v r

createFunction name body =
  WasmFunc proxy $ WasmFuncObj name (typetags p) (typetags v) (typetags r) (untype $ funcbody)
  where
    proxy@(p, v, r) = (value, value, value)
    (params, n1) = (genvar p 0)
    (vars, _) = (genvar v n1)
    returnInstr = TypedInstr Return
    funcbody = body params vars returnInstr

{- Param & Var Types -}

class VarTypes v where
  value :: v
  genvar :: v -> Int -> (VarSet v, Int)
  typetags :: v -> [TypeTag]

instance VarTypes () where
  value = ()
  genvar _ i = ((), i)
  typetags _ = []

instance VarTypes I32 where
  value = I32
  genvar _ i = (Var i, i+1)
  typetags t = [tag t]

instance VarTypes F32 where
  value = F32
  genvar _ i = (Var i, i+1)
  typetags t = [tag t]

instance (BaseType t1, BaseType t2) => VarTypes (t1, t2) where
  value = (val, val)
  genvar _ i = ((Var i, Var $ i + 1), i + 2)
  typetags (t1, t2) = [tag t1, tag t2]

instance (BaseType t1, BaseType t2, BaseType t3) => VarTypes (t1, t2, t3) where
  value = (val, val, val)
  genvar _ i = ((Var i, Var $ i + 1, Var $ i + 2), i + 3)
  typetags (t1, t2, t3) = [tag t1, tag t2, tag t3]

instance (BaseType t1, BaseType t2, BaseType t3, BaseType t4) => VarTypes (t1, t2, t3, t4) where
  value = (val, val, val, val)
  genvar _ i = ((Var i, Var $ i + 1, Var $ i + 2, Var $ i + 3), i + 4)
  typetags (t1, t2, t3, t4) = [tag t1, tag t2, tag t3, tag t4]

instance (BaseType t1, BaseType t2, BaseType t3, BaseType t4, BaseType t5) => VarTypes (t1, t2, t3, t4, t5) where
  value = (val, val, val, val, val)
  genvar _ i = ((Var i, Var $ i + 1, Var $ i + 2, Var $ i + 3, Var $ i + 4), i + 5)
  typetags (t1, t2, t3, t4, t5) = [tag t1, tag t2, tag t3, tag t4, tag t5]

type family VarSet v where
  VarSet () = ()
  VarSet I32 = Var I32
  VarSet F32 = Var F32
  VarSet (t1, t2) = (Var t1, Var t2)
  VarSet (t1, t2, t3) = (Var t1, Var t2, Var t3)
  VarSet (t1, t2, t3, t4) = (Var t1, Var t2, Var t3, Var t4)
  VarSet (t1, t2, t3, t4, t5) = (Var t1, Var t2, Var t3, Var t4, Var t5)

type family StackType t s where
  StackType () s = s
  StackType I32 s = s :+ I32
  StackType F32 s = s :+ F32
  StackType (t1, t2) s = s :+ t1 :+ t2
  StackType (t1, t2, t3) s = s :+ t1 :+ t2 :+ t3
  StackType (t1, t2, t3, t4) s = s :+ t1 :+ t2 :+ t3 :+ t4
  StackType (t1, t2, t3, t4, t5) s = s :+ t1 :+ t2 :+ t3 :+ t4 :+ t5
