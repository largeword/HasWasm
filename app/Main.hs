{-# LANGUAGE RankNTypes #-}
module Main where

import HasWasm
import HasWasm.Instruction

main :: IO ()
main = do
  case buildModule myModule1 of
    Right result -> putStrLn $ result
    Left err -> putStrLn $ "Error: " ++ err

myModule :: WasmModule
myModule = createModule $ do
  addFunc addCounter
  addGlobal counter
  addGlobal immvar
  addFunc mathsqrt

myModule0 :: WasmModule
myModule0 = createModule $ do
  addFunc addCounter
  addFunc mathsqrt
  addGlobal counter

myModule1 :: WasmModule
myModule1 = createModule $ do
  addFunc addCounter
  addFunc mathsqrt
  addFunc add3

myModule2 :: WasmModule
myModule2 = createModule $ do
  addFunc rgb
  addFunc add3

myModule3 :: WasmModule
myModule3 = createModule $ do
  addFunc add3
  addFunc rgb

-- it should be that myModule1 ~~ myModule2 ~~ myModule3

mathsqrt :: WasmFunc F32 () F32
mathsqrt = createImportFunction "Math" "sqrt" "sqrt"

counter :: GlobalVar Mut I32
counter = createGlobalI32 "counter" (Just "counter") 0

immvar :: GlobalVar Imm I32
immvar = createGlobalI32 "immvar" Nothing 0

addCounter :: WasmFunc (I32) () ()
addCounter = createExpFunction "addCounter" func
  where
  func i _ _ =
    global_get counter #
    global_get immvar #
    local_get i #
    i32_add #
    i32_add #
    global_set counter

add3 :: WasmFunc (I32, I32, I32) () I32
add3 = createExpFunction "add3" funcbody
  where
  funcbody (a, b, c) _ ret =
    local_get a #
    local_get b #
    local_get c #
    call rgb

rgb :: WasmFunc (I32, I32, I32) (I32) I32
rgb = createExpFunction "rgb" func
  where
  func :: (Stack s) => (Var I32, Var I32, Var I32) -> Var I32 -> ReturnInstr s I32 -> FuncBody s I32
  func (r, g, b) l ret =
    local_get r #
    i32_const 256 #
    i32_mul #
    local_get g #
    i32_add #
    i32_const 256 #
    i32_mul #
    local_get b #
    i32_add

fact :: WasmFunc I32 () I32
fact = createExpFunction "factorial" func
  where
  func n _ ret =
    block () () (\lbl ->
      local_get n #
      br_if lbl #
      i32_const 1 #
      ret
    ) #
    local_get n #
    local_get n #
    i32_const 1 #
    i32_sub #
    call fact #
    i32_mul

test1 :: WasmFunc () () (I32, I32)
test1 = createExpFunction "test1" $ \_ _ ret -> (
    i32_const 1 #
    i32_const 2 #
    i32_const 3 #
    i32_add #
    block () () (\lbl ->
      i32_const 0 #
      br_if lbl #
      block () () (\lbl2 ->
        i32_const 1 #
        br_if lbl2 #
        br lbl #
        ret
      )
    )
  )
