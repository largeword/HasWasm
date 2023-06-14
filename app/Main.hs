{-# LANGUAGE RankNTypes #-}
module Main where

import HasWasm
import HasWasm.Instruction

main :: IO ()
main = do
  case buildModule myModule of
    Right result -> putStrLn $ result
    Left err -> putStrLn $ "Error: " ++ err

myModule :: WasmModule
myModule = createModule $ do
  addFunc add3
  addFunc rgb

myModule1 :: WasmModule
myModule1 = createModule $ do
  addFunc add3

myModule2 :: WasmModule
myModule2 = createModule $ do
  addFunc rgb
  addFunc add3

-- it should be that myModule ~~ myModule1 ~~ myModule2

add3 :: WasmFunc (I32, I32, I32) () I32
add3 = createExpFunction "add3" funcbody
  where
  funcbody (a, b, c) _ ret =
    local_get a #
    local_get b #
    local_get c #
    call rgb

rgb :: WasmFunc (I32, I32, I32) (I32) I32
rgb = createLocalFunction "rgb" func
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
