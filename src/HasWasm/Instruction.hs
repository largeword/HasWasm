{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module HasWasm.Instruction (
  -- instructions
  (#),

  -- i32
  i32_const,
  i32_add,
  i32_sub,
  i32_mul,
  i32_div_s,
  i32_div_u,
  i32_rem_s,
  i32_rem_u,
  i32_and,
  i32_or,
  i32_xor,
  i32_shl,
  i32_shr_s,
  i32_shr_u,
  i32_rotl,
  i32_rotr,
  i32_eqz,
  i32_eq,
  i32_ne,
  i32_lt_s,
  i32_lt_u,
  i32_gt_s,
  i32_gt_u,
  i32_le_s,
  i32_le_u,
  i32_ge_s,
  i32_ge_u,


  -- f32
  f32_const,
  f32_add,
  f32_sub,
  f32_mul,
  f32_div,
  f32_min,
  f32_max,
  f32_copysign,
  f32_neg,
  f32_abs,
  f32_ceil,
  f32_floor,
  f32_trunc,
  f32_nearest,
  f32_sqrt,
  f32_eq,
  f32_ne,
  f32_lt,
  f32_gt,
  f32_le,
  f32_ge,

  -- flow control
  block,
  loop,
  br,
  br_if,
  call,

  -- variables
  local_get,
  local_set,
  global_get,
  global_set
) where

import HasWasm.Internal

{- WASM Instructions -}

i32_const :: (Stack s) => Int -> TypedInstr s (s :+ I32)
i32_const i = TypedInstr (I32Const i)

-- i binop
i32_add :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_add = i32_binary ADDI

i32_sub :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_sub = i32_binary SUBI

i32_mul :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_mul = i32_binary MULI

i32_div_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_div_s = i32_binary DIVS

i32_div_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_div_u = i32_binary DIVU

i32_rem_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_rem_s = i32_binary REMS

i32_rem_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_rem_u = i32_binary REMU

i32_and :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_and = i32_binary ANDI

i32_or :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_or = i32_binary ORI

i32_xor :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_xor = i32_binary XORI

i32_shl :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_shl = i32_binary SHLI

i32_shr_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_shr_s = i32_binary SHR_S

i32_shr_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_shr_u = i32_binary SHR_U

i32_rotl :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_rotl = i32_binary ROTLI

i32_rotr :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_rotr = i32_binary ROTRI


-- i testop
i32_eqz:: (Stack s) => TypedInstr (s :+ I32) (s :+ I32)
i32_eqz = i32_unary EQZ

-- i relop
i32_eq :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_eq = i32_compare EQI

i32_ne :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_ne = i32_compare NEI

i32_lt_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_lt_s = i32_compare LTI_S

i32_lt_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_lt_u = i32_compare LTI_U

i32_gt_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_gt_s = i32_compare GTI_S

i32_gt_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_gt_u = i32_compare GTI_U

i32_le_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_le_s = i32_compare LEI_S

i32_le_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_le_u = i32_compare LEI_U

i32_ge_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_ge_s = i32_compare GEI_S

i32_ge_u :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_ge_u = i32_compare GEI_U


f32_const :: (Stack s) => Float -> TypedInstr s (s :+ F32)
f32_const n = TypedInstr (F32Const n)

-- f binop
f32_add :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_add = f32_binary ADDF

f32_sub :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_sub = f32_binary SUBF

f32_mul :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_mul = f32_binary MULF

f32_div :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_div = f32_binary DIVF

f32_min :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_min = f32_binary MINF

f32_max :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_max = f32_binary MAXF

f32_copysign :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_copysign = f32_binary COPYSIGNF

-- f unop
f32_neg :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_neg = f32_unary NEGF

f32_abs :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_abs = f32_unary ABSF

f32_ceil :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_ceil = f32_unary CEILF

f32_floor :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_floor = f32_unary FLOORF

f32_trunc :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_trunc = f32_unary TRUNCF

f32_nearest :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_nearest = f32_unary NEARESTF

f32_sqrt :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_sqrt = f32_unary SQRTF

-- f relop
f32_eq :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_eq = f32_compare EQF

f32_ne :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_ne = f32_compare NEF

f32_lt :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_lt = f32_compare LTF

f32_gt :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_gt = f32_compare GTF

f32_le :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_le = f32_compare LEF

f32_ge :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_ge = f32_compare GEF


br :: (Stack s1, Stack s2) => TypedLabel s1 -> TypedInstr s1 s2
br (TypedLabel l) = TypedInstr (Branch l)

br_if :: (Stack s) => TypedLabel s -> TypedInstr (s :+ I32) s
br_if (TypedLabel l) = TypedInstr (BranchIf l)

block :: (VarTypes p, VarTypes r, Stack s) => p -> r -> (TypedLabel (StackType r s) -> FuncCallType s p r) -> FuncCallType s p r
block p r body = TypedInstr $ Block False (typetags p) (typetags r) (untype . body . TypedLabel)

loop :: (VarTypes p, VarTypes r, Stack s) => p -> r -> (TypedLabel (StackType p s) -> FuncCallType s p r) -> FuncCallType s p r
loop p r body = TypedInstr $ Block True  (typetags p) (typetags r) (untype . body . TypedLabel)

call :: (Stack s, VarTypes p, VarTypes v, VarTypes r) => WasmFunc p v r -> FuncCallType s p r
call (WasmFunc _ func) = TypedInstr $ Call func

local_get :: (Stack s) => Var t -> TypedInstr s (s :+ t)
local_get (Var i) = TypedInstr $ LocalGet i

local_set :: (Stack s) => Var t -> TypedInstr (s :+ t) s
local_set (Var i) = TypedInstr $ LocalSet i

global_get :: (Mutability m, Stack s) => GlobalVar m t -> TypedInstr s (s :+ t)
global_get (GlobalVar _ name _ _) = TypedInstr $ GlobalGet name

global_set :: (Stack s) => GlobalVar Mut t -> TypedInstr (s :+ t) s
global_set (GlobalVar _ name _ _) = TypedInstr $ GlobalSet name

{- Helper Instructions -}

i32_binary :: (Stack s) => BinOpI -> TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_binary op = TypedInstr (I32Binary op)

i32_unary :: (Stack s) => UnOpI -> TypedInstr (s :+ I32) (s :+ I32)
i32_unary op = TypedInstr (I32Unary op)

i32_compare :: (Stack s) => RelOpI -> TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_compare op = TypedInstr (I32Compare op)

f32_binary :: (Stack s) => BinOpF -> TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_binary op = TypedInstr (F32Binary op)

f32_unary :: (Stack s) => UnOpF -> TypedInstr (s :+ F32) (s :+ F32)
f32_unary op = TypedInstr (F32Unary op)

f32_compare :: (Stack s) => RelOpF -> TypedInstr (s :+ F32 :+ F32) (s :+ I32)
f32_compare op = TypedInstr (F32Compare op)
