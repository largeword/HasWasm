{-# LANGUAGE TypeOperators #-}

-- most of the constructors from internal should NOT be exported!
module HasWasm (
  -- exposed types and helper functions
  I32, F32,
  StackType, (:+),
  TypedInstr,

  -- instructions
  (#),
  i32_const,
  i32_add,
  i32_sub,
  i32_neg,
  block,
  br,
  br_if,

) where

import HasWasm.Internal

{- WASM Module -}

{- WASM Instructions -}

i32_const :: (StackType s) => Int -> TypedInstr s (s :+ I32)
i32_const i = TypedInstr (I32Const i)

i32_add :: (StackType s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_add = i32_binary ADD

i32_sub :: (StackType s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_sub = i32_binary SUB

i32_neg :: (StackType s) => TypedInstr (s :+ I32) (s :+ I32)
i32_neg = i32_unary NEG

br :: (StackType s1, StackType s2) => TypedLabel s1 -> TypedInstr s1 s2
br (TypedLabel l) = TypedInstr (Branch l)

br_if :: (StackType s) => TypedLabel s -> TypedInstr (s :+ I32) s
br_if (TypedLabel l) = TypedInstr (BranchIf l)

block :: (StackType s1, StackType s2) => (TypedLabel s2 -> TypedInstr s1 s2) -> TypedInstr s1 s2
block body = TypedInstr $ Block (untype . body . TypedLabel)

{- Helper Instructions -}

i32_binary :: (StackType s) => BinOp -> TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_binary op = TypedInstr (I32Binary op)

i32_unary :: (StackType s) => UnOp -> TypedInstr (s :+ I32) (s :+ I32)
i32_unary op = TypedInstr (I32Unary op)

-- test :: (StackType s) => TypedInstr s (s :+ I32 :+ I32)
-- test =
--   i32_const 1 #
--   i32_const 2 #
--   i32_const 3 #
--   i32_add #
--   block (\lbl ->
--     i32_const 0 #
--     br_if lbl #
--     block (\lbl2 ->
--       i32_const 1 #
--       i32_neg #
--       br_if lbl2 #
--       br lbl
--     )
--   )
