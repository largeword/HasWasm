-- This main module is an example of how HasWasm can be used

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
  addFunc factorial
  addFunc factorialRec
  addFunc incCounter

factorial :: WasmFunc I32 () I32
factorial = createExpFunction "factorial" func
  where
  func n () ret =
    i32_const 1 #
    block (I32) (I32) (\end->
      loop (I32) (I32) (\start ->
        local_get n #
        i32_const 1 #
        i32_le_s #
        br_if end #    -- if n <= 1, jump to end
        local_get n #
        i32_mul #      -- total = total * n
        local_get n #
        i32_const 1 #
        i32_sub #
        local_set n #  -- set n = n - 1
        br start       -- loop back
      )
    )

factorialRec :: WasmFunc I32 () I32
factorialRec = createExpFunction "factorial_rec" func
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
    call factorialRec #
    i32_mul

incCounter :: WasmFunc () () ()
incCounter = createExpFunction "inc_counter" func
  where
  func _ _ _ =
    global_get counter #
    call addByConst #
    global_set counter #
    global_get counter #
    call consoleLog

addByConst :: WasmFunc I32 () I32
addByConst = createLocalFunction "addByConst" func
  where
  func i _ _ =
    local_get i #
    global_get constant #
    i32_add

counter :: GlobalVar Mut I32
counter = createGlobalI32 "counter" (Just "counter") 0

constant :: GlobalVar Imm I32
constant = createGlobalI32 "constant" Nothing 1

consoleLog :: WasmFunc I32 () ()
consoleLog = createImportFunction "console" "log" "console_log"

-- other examples

myModule0 :: WasmModule
myModule0 = createModule $ do
  addFunc addCounter
  addFunc mathsqrt
  addGlobal counter

myModule1 :: WasmModule
myModule1 = createModule $ do
  addFunc addCounter
  -- addFunc mathsqrt
  addFunc add3

myModule2 :: WasmModule
myModule2 = createModule $ do
  addFunc rgb
  addFunc add3

myModule3 :: WasmModule
myModule3 = createModule $ do
  addFunc add3
  -- addFunc rgb

moduleIndirect :: WasmModule
moduleIndirect = createModule $ do
  addFunc indirect1 -- should also includes indirect2, add3, rgb, sqrt, addCounter, counter and immvar

moduleMutualRec :: WasmModule
moduleMutualRec = createModule $ do
  addFunc rec1 -- should also includes rec1, rec2, rec3, and not looping forever

-- it should be that myModule1 ~~ myModule2 ~~ myModule3

mathsqrt :: WasmFunc I32 () I32
mathsqrt = createImportFunction "Math" "sqrt" "sqrt"

addCounter :: WasmFunc (I32) () ()
addCounter = createExpFunction "addCounter" func
  where
  func i _ _ =
    global_get counter #
    global_get constant #
    local_get i #
    i32_add #
    i32_add #
    global_set counter

indirect1 :: WasmFunc () () I32
indirect1 = createExpFunction "indirect1" func
  where
    func _ _ _ =
      call indirect2

indirect2 :: WasmFunc () () I32
indirect2 = createExpFunction "indirect2" func
  where
    func _ _ _ =
      i32_const 1 #
      call addCounter #
      i32_const 0 #
      i32_const 0 #
      i32_const 0 #
      call add3

add3 :: WasmFunc (I32, I32, I32) () I32
add3 = createExpFunction "add3" funcbody
  where
  funcbody (a, b, c) _ ret =
    local_get a #
    local_get b #
    local_get c #
    call rgb #
    call mathsqrt

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

rec1 :: WasmFunc I32 () I32
rec1 = createExpFunction "rec1" func where
  func n _ ret =
    block () I32 (\lbl ->
      local_get n #
      local_get n #
      br_if lbl #
      i32_const 1 #
      i32_sub #
      call rec2
    )

rec2 :: WasmFunc I32 () I32
rec2 = createExpFunction "rec2" func where
  func n _ ret =
    block () I32 (\lbl ->
      local_get n #
      local_get n #
      br_if lbl #
      i32_const 1 #
      i32_sub #
      call rec3
    )

rec3 :: WasmFunc I32 () I32
rec3 = createExpFunction "rec3" func where
  func n _ ret =
    block () I32 (\lbl ->
      global_get counter #
      global_get constant #
      br_if lbl #
      i32_const 1 #
      i32_sub #
      call rec1
    )

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
