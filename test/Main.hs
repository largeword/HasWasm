module Main (main) where

import HasWasm
import HasWasm.Instruction
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Build Module Test" [
    testUnaryI32,
    testIndirectAdd,
    testIndirectAdd2,
    testMutualRec
  ]

{- Instruction building test -}

testUnaryI32 :: TestTree
testUnaryI32 = testCase "unary instructions" $ do
  assertModule (singleFunc unaryI32) $
    singleFuncFormat "unary" "(param i32)" "(result i32)" "" "local.get 0 i32.eqz return"
  where
    unaryI32 :: WasmFunc I32 () I32
    unaryI32 = createLocalFunction "unary" $
      \n _ ret ->
        local_get n #
        i32_eqz #
        ret

testIndirectAdd :: TestTree
testIndirectAdd = testCase "indirect add 1" $ do
  assertEqualModule moduleIndirect moduleDirect
  where
    moduleIndirect :: WasmModule
    moduleIndirect = createModule $ do
      addFunc indirect1 -- should also includes indirect2, add3, rgb, sqrt, addCounter, counter and constant

    moduleDirect :: WasmModule
    moduleDirect = createModule $ do
       -- should also includes indirect2, add3, rgb, sqrt, addCounter, counter and constant
      addFunc indirect1
      addFunc indirect2
      addFunc add3
      addFunc rgb
      addFunc mathsqrt
      addFunc addCounter
      addGlobal counter
      addGlobal constant

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
    add3 = createExpFunction "add3" func
      where
      func (a, b, c) _ ret =
        local_get a #
        local_get b #
        local_get c #
        call rgb #
        call mathsqrt

    rgb :: WasmFunc (I32, I32, I32) (I32) I32
    rgb = createLocalFunction "rgb" func
      where
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

    counter :: GlobalVar Mut I32
    counter = createGlobalI32 "counter" (Just "counter") 0

    constant :: GlobalVar Imm I32
    constant = createGlobalI32 "constant" Nothing 1

    mathsqrt :: WasmFunc I32 () I32
    mathsqrt = createImportFunction "Math" "sqrt" "sqrt"

testIndirectAdd2 :: TestTree
testIndirectAdd2 = testCase "indirect add 2" $
  assertEqualModule moduleIndirect moduleDirect
  where
  moduleIndirect :: WasmModule
  moduleIndirect = createModule $ do
    addFunc factorial
    addFunc factorialRec
    addFunc incCounter

  moduleDirect :: WasmModule
  moduleDirect = createModule $ do
      -- should also includes indirect2, add3, rgb, sqrt, addCounter, counter and constant
    addFunc factorial
    addFunc factorialRec
    addFunc addByConst
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

testMutualRec :: TestTree
testMutualRec = testCase "mutual recursion" $ do
  assertEqualModule moduleIndirect moduleDirect
  where
  moduleIndirect :: WasmModule
  moduleIndirect = createModule $ do
    addFunc rec1 -- should also includes rec1, rec2, rec3, and not looping forever

  moduleDirect :: WasmModule
  moduleDirect = createModule $ do
    addFunc rec1
    addFunc rec2
    addFunc rec3
    addGlobal counter
    addGlobal constant

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

  counter :: GlobalVar Mut I32
  counter = createGlobalI32 "counter" (Just "counter") 0

  constant :: GlobalVar Imm I32
  constant = createGlobalI32 "constant" Nothing 1


{- Helper functions -}

assertEqualModule :: WasmModule -> WasmModule -> Assertion
assertEqualModule m1 m2 =
  assertEqual "" (fmap reformatOutput $ buildModule m1) (fmap reformatOutput $ buildModule m2)

assertModule :: WasmModule -> String -> Assertion
assertModule mod str =
  assertEqual "" (Right $ reformatOutput str) (fmap reformatOutput $ buildModule mod)

singleFunc :: WasmFunc p v r -> WasmModule
singleFunc f = createModule $ do
  addFunc f

singleFuncFormat :: String -> String -> String -> String -> String -> String
singleFuncFormat name param result local instrs =
  "(module (func $" ++ name ++ " " ++ param ++ " "
  ++ result ++ " " ++ local ++ " " ++ instrs ++ " ))"

reformatOutput :: String -> String
reformatOutput = unwords . words . filter (/= '\n')
