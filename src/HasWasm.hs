{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- most of the constructors from internal should NOT be exported!
module HasWasm (
  -- exposed types and helper functions
  I32, F32,
  Stack, (:+),
  TypedInstr, WasmFunc,
  Var, ReturnInstr, FuncBody,

  -- instructions
  (#),
  createFunction,
  i32_const,
  i32_add,
  i32_sub,
  i32_neg,
  block,
  loop,
  br,
  br_if,
  call,
  local_get,
  local_set,

  printFunc
) where

import HasWasm.Internal
import Control.Monad.State
import Data.Map ( Map )

{- WASM Module -}

data WasmModule = WasmModule (Map String Declaration)

data Declaration = FuncDecl Bool WasmFuncT | GlobalVar

data BuilderContext = BuilderContext {mod :: WasmModule}

type ModuleBuilder = State BuilderContext

addFunc :: Bool -> WasmFunc p v r -> Declaration
addFunc exported (WasmFunc _ func) = FuncDecl exported func

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


printFunc :: WasmFunc p v r -> String
printFunc (WasmFunc _ (WasmFuncT name _ _ _ f)) =
  let body = f () in
    "func " ++ name ++ " " ++ evalState (printInstr body) 0

printInstr :: Instr -> State Int String
printInstr (Sequence instrs) =
  foldl go (return "") $ map printInstr instrs
  where
    go acc m = do s1 <- acc; s2 <- m; return (s1 ++ s2 ++ " ")
printInstr (I32Const i) = return $ "i32_const " ++ show i
printInstr (I32Binary b) = return $ "i32_binary " ++ show b
printInstr (I32Unary u) = return $ "i32_unary " ++ show u
printInstr (F32Const f ) = return $ "f32_const " ++ show f
printInstr (F32Binary b ) = return $ "f32_binary " ++ show b
printInstr (F32Unary u ) = return $ "f32_unary " ++ show u
printInstr (Block f ) =
  do
    n <- get
    put (n+1)
    s <- printInstr $ f n
    return $ "block " ++ show n ++ " " ++ s ++ "end"
printInstr (Branch l ) = return $ "br " ++ show l
printInstr (BranchIf l) = return $ "br_if " ++ show l
printInstr (Call (WasmFuncT name _ _ _ _) ) = return $ "call " ++ name
printInstr (Return ) = return "return"
printInstr (LocalGet i ) = return $ "local_get " ++ show i
printInstr (LocalSet i) = return $ "local_set " ++ show i
