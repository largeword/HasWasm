{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module HasWasm.Internal  (
  I32(..), F32(..), TypeTag(..),
  BaseType(..),
  StackT,
  Stack,
  (:+),
  Instr(..),
  BinOpI(..),
  UnOpI(..),
  BinOpF(..),
  UnOpF(..),
  LabelId,
  TypedInstr(..),
  TypedLabel(..),
  (#),
  untype,
  Var(..),
  VarTypes(..),
  InitValue(..),
  Mut(..), Imm(..),
  Mutability,
  isMutable,
  GlobalVar(..),
  WasmFunc(..),
  WasmFuncT(..),
  createFunction,
  createExpFunction,
  createLocalFunction,
  createGlobalI32,
  createGlobalF32,
  ReturnInstr,
  FuncBody,
  FuncCallType,
  StackType,
) where

import Data.Kind
import Data.Sequence

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

data (Stack s, BaseType t) => StackT s t

type s :+ t = StackT s t
infixl 2 :+

type family Stack (s :: Type) :: Constraint where
  Stack (StackT s t) = ()

{- Types -}

data BinOpI = ADDI | SUBI | MULI | DIVS
data UnOpI = EQZ

data BinOpF = ADDF | SUBF | MULF | DIV
data UnOpF = NEG

data Instr =
  Sequence (Seq Instr) |
  I32Const Int |
  I32Binary BinOpI |
  I32Unary UnOpI |
  F32Const Float |
  F32Binary BinOpF |
  F32Unary UnOpF |
  Block Bool [TypeTag] [TypeTag] (LabelId -> Instr) |
  Branch LabelId |
  BranchIf LabelId |
  Call WasmFuncT |
  Return |
  LocalGet Int |
  LocalSet Int |
  GlobalGet String |
  GlobalSet String

newtype (Stack a, Stack b) => TypedInstr a b = TypedInstr Instr

type LabelId = Int
newtype (Stack s) => TypedLabel s = TypedLabel LabelId

join :: Instr -> Instr -> Instr
join (Sequence s1) (Sequence s2) = Sequence (s1 >< s2)
join (Sequence s) i = Sequence (s |> i)
join i (Sequence s) = Sequence (i <| s)
join i1 i2 = Sequence (i1 <| i2 <| empty)

(#) :: (Stack a, Stack b, Stack c) => TypedInstr a b -> TypedInstr b c -> TypedInstr a c
(TypedInstr i1) # (TypedInstr i2) = TypedInstr (join i1 i2)

infixl 0 #

untype :: TypedInstr a b -> Instr
untype (TypedInstr i) = i

{- Var and Function Types -}

newtype (BaseType t) => Var t = Var Int

data (Mutability m, BaseType t) => GlobalVar m t = GlobalVar m String (Maybe String) InitValue

createGlobalI32 :: (Mutability m) => String -> Maybe String -> Int -> GlobalVar m I32
createGlobalI32 name expname val = GlobalVar mval name expname (InitI val)

createGlobalF32 :: (Mutability m) => String -> Maybe String -> Float -> GlobalVar m F32
createGlobalF32 name expname val = GlobalVar mval name expname (InitF val)

data InitValue = InitI Int | InitF Float

data Mut = Mut
data Imm = Imm

class Mutability m where
  isMutable :: m -> Bool
  mval :: m

instance Mutability Mut where
  isMutable _ = True
  mval = Mut

instance Mutability Imm where
  isMutable _ = False
  mval = Imm

data WasmFunc p v r where
  WasmFunc :: (VarTypes p, VarTypes v, VarTypes r) => (p, v, r) -> WasmFuncT -> WasmFunc p v r

data WasmFuncT = WasmFuncT String (Maybe String) [TypeTag] [TypeTag] [TypeTag] (Instr)

type FuncBody s r = TypedInstr s (StackType r s)
type ReturnInstr s r = forall s2. TypedInstr (StackType r s) s2
type FuncCallType s p r = TypedInstr (StackType p s) (StackType r s)

createFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (Maybe String) -> (VarSet p -> VarSet v -> ReturnInstr (StackT s t) r -> FuncBody (StackT s t) r) -> WasmFunc p v r

createFunction name expname body =
  WasmFunc proxy $ WasmFuncT name expname (typetags p) (typetags v) (typetags r) (untype funcbody)
  where
    proxy@(p, v, r) = (value, value, value)
    (params, n1) = (genvar p 0)
    (vars, _) = (genvar v n1)
    returnInstr = TypedInstr Return
    funcbody = body params vars returnInstr

createExpFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (VarSet p -> VarSet v -> ReturnInstr (StackT s t) r -> FuncBody (StackT s t) r) -> WasmFunc p v r
createExpFunction name body = createFunction name (Just name) body

createLocalFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (VarSet p -> VarSet v -> ReturnInstr (StackT s t) r -> FuncBody (StackT s t) r) -> WasmFunc p v r
createLocalFunction name body = createFunction name Nothing body

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

{- Show & Eq Instances -}

deriving instance Eq TypeTag
deriving instance Eq BinOpI
deriving instance Eq UnOpI
deriving instance Eq BinOpF
deriving instance Eq UnOpF

instance Show TypeTag where
  show I32T = "i32"
  show F32T = "f32"

instance Show BinOpI where
  show ADDI = "add"
  show SUBI = "sub"
  show MULI = "mul"
  show DIVS = "div_s"

instance Show UnOpI where
  show EQZ = "eqz"

instance Show BinOpF where
  show ADDF = "add"
  show SUBF = "sub"
  show MULF = "mul"
  show DIV  = "div"

instance Show UnOpF where
  show NEG = "neg"
