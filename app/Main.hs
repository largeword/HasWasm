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
  addFunc rgb False
  addFunc fact True

rgb :: WasmFunc (I32, I32, I32) (I32) I32
rgb = createFunction "rgb" func
  where
  func :: (Stack s) => (Var I32, Var I32, Var I32) -> Var I32 -> ReturnInstr s I32 -> FuncBody s I32
  func (r, g, b) _ _ =
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
fact = createFunction "factorial" func
  where
  func n _ ret =
    local_get n #
    block I32 () (\lbl ->
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
test1 = createFunction "test1" $ \_ _ ret -> (
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

-- test :: WasmFunc (I32, I32) () I32
-- test = createFunction "test" func
--   where
--     func :: (Stack s) => (Var I32, Var I32) -> () -> ReturnInstr s I32 -> FuncBody s I32
--     func (i, j) v ret = i32_const 1 # i32_const 1 # call test
