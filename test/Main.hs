module Main (main) where

import HasWasm
import HasWasm.Instruction
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Build Module Test" [
    testUnaryI32
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

{- Helper functions -}

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
