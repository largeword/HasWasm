{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- most of the constructors from internal should NOT be exported!
module HasWasm (
  -- exposed types and helper functions
  I32(..), F32(..),
  Stack, (:+),
  TypedInstr, WasmFunc,
  Var, ReturnInstr, FuncBody,

  -- module building
  createModule,
  addFunc,

  -- instructions
  (#),
  createFunction,
  i32_const,
  i32_add,
  i32_sub,
  i32_mul,
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
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Map ( Map )
import qualified Data.Map as Map

{- WASM Module -}

data WasmModule = WasmModule {declarations :: Map String Declaration, exports :: Map String String}

findIn :: (WasmModule -> Map String a) -> String -> WasmModule -> Maybe a
findIn getf key mod = Map.lookup key (getf mod)

addDeclaration :: String -> Declaration -> WasmModule -> WasmModule
addDeclaration key val mod = mod {declarations = Map.insert key val (declarations mod)}

addExport :: String -> String -> WasmModule -> WasmModule
addExport key val mod = mod {exports = Map.insert key val (exports mod)}

data Declaration = FuncDecl WasmFuncT | GlobalVar

data BuilderContext = BuilderContext {wasmmod :: WasmModule}

newWasmModule :: WasmModule
newWasmModule = WasmModule {declarations = Map.empty, exports = Map.empty}

newCtx :: BuilderContext
newCtx = BuilderContext { wasmmod = newWasmModule}

type ModuleBuilder = ExceptT String (State BuilderContext)

createModule :: ModuleBuilder () -> Either String WasmModule
createModule b =
  let (result, ctx) = runState (runExceptT b) newCtx in
    case result of
      Right _ -> Right (wasmmod ctx)
      Left e -> Left e

updateMod :: WasmModule -> ModuleBuilder ()
updateMod mod = do
  ctx <- get
  put ctx {wasmmod = mod}

addFunc :: WasmFunc p v r -> Bool -> ModuleBuilder ()
addFunc (WasmFunc _ func) exported = do
  let decl = FuncDecl func
  let name = funcName func
  mod <- gets wasmmod
  case findIn declarations name mod of
    Just _ -> throwE $ "Name is already declared: " ++ name
    Nothing -> updateMod (addDeclaration name decl mod) -- TODO: recurse to find implicitly called functions?

  if exported then do
    mod <- gets wasmmod
    updateMod (addExport name name mod)
  else return ()

funcName :: WasmFuncT -> String
funcName (WasmFuncT name _ _ _ _) = name

{- WASM Instructions -}

i32_const :: (Stack s) => Int -> TypedInstr s (s :+ I32)
i32_const i = TypedInstr (I32Const i)

i32_add :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_add = i32_binary ADD

i32_sub :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_sub = i32_binary SUB

i32_mul :: (Stack s) => TypedInstr (s :+ I32 :+ I32) (s :+ I32)
i32_mul = i32_binary MUL

br :: (Stack s1, Stack s2) => TypedLabel s1 -> TypedInstr s1 s2
br (TypedLabel l) = TypedInstr (Branch l)

br_if :: (Stack s) => TypedLabel s -> TypedInstr (s :+ I32) s
br_if (TypedLabel l) = TypedInstr (BranchIf l)

-- block :: (Stack s1, Stack s2) => (TypedLabel s2 -> TypedInstr s1 s2) -> TypedInstr s1 s2
-- block body = TypedInstr $ Block False (untype . body . TypedLabel)

block :: (VarTypes p, VarTypes r, Stack s) => p -> r -> (TypedLabel (StackType r s) -> FuncCallType s p r) -> FuncCallType s p r
block p r body = TypedInstr $ Block False (typetags p) (typetags r) (untype . body . TypedLabel)

loop :: (VarTypes p, VarTypes r, Stack s) => p -> r -> (TypedLabel (StackType p s) -> FuncCallType s p r) -> FuncCallType s p r
loop p r body = TypedInstr $ Block True  (typetags p) (typetags r) (untype . body . TypedLabel)

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
printFunc (WasmFunc _ (WasmFuncT name params locals results body)) =
  "(func " ++ printFuncName name ++ " " ++
  (printVars "param" params) ++
  (printVars "result" results) ++
  (printVars "local" locals) ++ "\n" ++
  evalState (printInstr body) newPrintState ++ ")"

printFuncName :: String -> String
printFuncName name = "$" ++ name

printVars :: String -> [TypeTag] -> String
printVars _ [] = ""
printVars prefix tags = "(" ++ prefix ++ (concat $ map showtag tags) ++ ") "
  where
    showtag tag =  " " ++ show tag

data PrintState = PrintState {labelid :: Int, tabs :: Int}

printTabs :: Int -> String
printTabs 0 = ""
printTabs n = "  " ++ printTabs (n-1)

newPrintState :: PrintState
newPrintState = PrintState {labelid = 0, tabs = 1}

printInstr :: Instr -> State PrintState String
printInstr (Sequence instrs) =
  foldl go (return "") $ map printInstr instrs
  where
    go :: State PrintState String -> State PrintState String -> State PrintState String
    go acc m = do
      s1 <- acc
      s2 <- m
      tab <- gets tabs
      return (s1 ++ (printTabs tab) ++ s2 ++ "\n")

printInstr (I32Const i) = return $ "i32.const " ++ show i
printInstr (I32Binary b) = return $ "i32." ++ show b
printInstr (I32Unary u) = return $ "i32." ++ show u
printInstr (F32Const f ) = return $ "f32.const " ++ show f
printInstr (F32Binary b ) = return $ "f32." ++ show b
printInstr (F32Unary u ) = return $ "f32." ++ show u
printInstr (Block isLoop params results f ) =
  do
    ctx <- get
    let n = labelid ctx
    let t = tabs ctx
    put ctx {labelid = n+1, tabs = t+1}
    s <- printInstr $ f n
    ctx <- get
    put ctx {tabs = t}
    let prefix = if isLoop then "(loop " else "(block "

    return $
      prefix ++ printLabel n ++ " " ++
      (printVars "param" params) ++
      (printVars "result" results) ++ "\n" ++ s ++  (printTabs t) ++ ")"
printInstr (Branch l ) = return $ "br " ++ printLabel l
printInstr (BranchIf l) = return $ "br_if " ++ printLabel l
printInstr (Call (WasmFuncT name _ _ _ _) ) = return $ "call " ++ printFuncName name
printInstr (Return ) = return "return"
printInstr (LocalGet i ) = return $ "local.get " ++ show i
printInstr (LocalSet i) = return $ "local.set " ++ show i

printLabel :: Int -> String
printLabel n = "$l" ++ show n
