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
  i32_eqz,

  -- f32
  f32_const,
  f32_add,
  f32_sub,
  f32_mul,
  f32_div,
  f32_neg,

  -- flow control
  block,
  loop,
  br,
  br_if,
  call,

  -- variables
  local_get,
  local_set,
) where

import HasWasm.Internal

{- WASM Instructions -}

i32_const :: (Stack s) => Int -> TypedInstr s (s :+ I32)
i32_const i = TypedInstr (I32Const i)

i32_add :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_add = i32_binary ADDI

i32_sub :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_sub = i32_binary SUBI

i32_mul :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_mul = i32_binary MULI

i32_div_s :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_div_s = i32_binary DIVS

i32_eqz:: (Stack s) => TypedInstr (s :+ I32) (s :+ I32)
i32_eqz = i32_unary EQZ

f32_const :: (Stack s) => Float -> TypedInstr s (s :+ F32)
f32_const n = TypedInstr (F32Const n)

f32_add :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_add = f32_binary ADDF

f32_sub :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_sub = f32_binary SUBF

f32_mul :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_mul = f32_binary MULF

f32_div :: (Stack s) => TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_div = f32_binary DIV

f32_neg :: (Stack s) => TypedInstr (s :+ F32) (s :+ F32)
f32_neg = f32_unary NEG

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

{- Helper Instructions -}

i32_binary :: (Stack s) => BinOpI -> TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_binary op = TypedInstr (I32Binary op)

i32_unary :: (Stack s) => UnOpI -> TypedInstr (s :+ I32) (s :+ I32)
i32_unary op = TypedInstr (I32Unary op)

f32_binary :: (Stack s) => BinOpF -> TypedInstr (s :+ F32 :+ F32) (s :+ F32)
f32_binary op = TypedInstr (F32Binary op)

f32_unary :: (Stack s) => UnOpF -> TypedInstr (s :+ F32) (s :+ F32)
f32_unary op = TypedInstr (F32Unary op)
