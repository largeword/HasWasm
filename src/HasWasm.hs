{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- most of the constructors from internal should NOT be exported!
module HasWasm (
  -- exposed types and helper functions
  I32, F32,
  Stack, (:+),
  TypedInstr,

  -- instructions
  (#),
  i32_const,
  i32_add,
  i32_sub,
  i32_neg,
  block,
  loop,
  br,
  br_if,
  call
) where

import HasWasm.Internal

{- WASM Module -}

{- WASM Instructions -}

i32_const :: (Stack s) => Int -> TypedInstr s (s :+ I32)
i32_const i = TypedInstr (I32Const i)

i32_add :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_add = i32_binary ADD

i32_sub :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_sub = i32_binary SUB

i32_neg :: (Stack s) => TypedInstr (s :+ I32) (s :+ I32)
i32_neg = i32_unary NEG

br :: (Stack s1, Stack s2) => TypedLabel s1 -> TypedInstr s1 s2
br (TypedLabel l) = TypedInstr (Branch l)

br_if :: (Stack s) => TypedLabel s -> TypedInstr (s :+ I32) s
br_if (TypedLabel l) = TypedInstr (BranchIf l)

block :: (Stack s1, Stack s2) => (TypedLabel s2 -> TypedInstr s1 s2) -> TypedInstr s1 s2
block body = TypedInstr $ Block (untype . body . TypedLabel)

loop :: (Stack s1, Stack s2) => (TypedLabel s1 -> TypedInstr s1 s2) -> TypedInstr s1 s2
loop body = TypedInstr $ Block (untype . body . TypedLabel)

call :: (VarTypes p, VarTypes v, VarTypes r) => WasmFunc p v r -> FuncCallType p r s
call (WasmFunc _ obj) = TypedInstr $ Call obj

{- Helper Instructions -}

i32_binary :: (Stack s) => BinOp -> TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_binary op = TypedInstr (I32Binary op)

i32_unary :: (Stack s) => UnOp -> TypedInstr (s :+ I32) (s :+ I32)
i32_unary op = TypedInstr (I32Unary op)

-- test :: WasmFunc () () (I32, I32)
-- test = createFunction "test" $ \_ _ ret -> (
--     i32_const 1 #
--     i32_const 2 #
--     i32_const 3 #
--     i32_add #
--     block (\lbl ->
--       i32_const 0 #
--       br_if lbl #
--       block (\lbl2 ->
--         i32_const 1 #
--         i32_neg #
--         br_if lbl2 #
--         br lbl #
--         ret
--       )
--     )
--   )

-- test :: WasmFunc (I32, I32) () I32
-- test = createFunction "test" func
--   where
--     func :: (Stack s) => (Var I32, Var I32) -> () -> ReturnInstr I32 s -> FuncBody I32 s
--     func (i, j) v ret = i32_const 1 # i32_const 1 # i32_add
