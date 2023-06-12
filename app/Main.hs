module Main where

import HasWasm

main :: IO ()
main = do
  putStrLn $ printFunc fact
  putStrLn $ printFunc test1

fact :: WasmFunc I32 () I32
fact = createFunction "factorial" func
  where
  func n _ ret =
    local_get n #
    block (\lbl ->
      br_if lbl #
      i32_const 1 #
      ret
    ) #
    local_get n #
    call fact

test1 :: WasmFunc () () (I32, I32)
test1 = createFunction "test1" $ \_ _ ret -> (
    i32_const 1 #
    i32_const 2 #
    i32_const 3 #
    i32_add #
    block (\lbl ->
      i32_const 0 #
      br_if lbl #
      block (\lbl2 ->
        i32_const 1 #
        i32_neg #
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
