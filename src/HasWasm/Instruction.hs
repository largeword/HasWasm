{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module HasWasm.Instruction (
  -- instructions
  (#),
  i32_const,
  i32_add,
  i32_sub,
  i32_mul,
  block,
  loop,
  br,
  br_if,
  call,
  local_get,
  local_set,
) where

import HasWasm.Internal

{- WASM Instructions -}

i32_const :: (Stack s) => Int -> TypedInstr s (s :+ I32)
i32_const i = TypedInstr (I32Const i)

i32_add :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_add = i32_binary ADD

i32_sub :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_sub = i32_binary SUB

i32_mul :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_mul = i32_binary MUL

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

i32_binary :: (Stack s) => BinOp -> TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_binary op = TypedInstr (I32Binary op)

i32_unary :: (Stack s) => UnOp -> TypedInstr (s :+ I32) (s :+ I32)
i32_unary op = TypedInstr (I32Unary op)
