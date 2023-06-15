-- most of the constructors from internal should NOT be exported!
module HasWasm (
  -- exposed types and helper functions
  I32(..), F32(..),
  Stack, (:+),
  TypedInstr, WasmFunc,
  Var, GlobalVar, Mut(..), Imm(..),
  ReturnInstr, FuncBody,
  WasmModule, WasmModuleT,

  -- module building
  createModule,
  addFunc, addGlobal,
  buildModule,
  createGlobalI32,
  createGlobalF32,
  createFunction,
  createExpFunction,
  createLocalFunction,
) where

import HasWasm.Internal
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Map ( Map )
import qualified Data.Map as Map

{- WASM Module -}

type WasmModule = Either String WasmModuleT
data WasmModuleT = WasmModuleT {declarations :: Map String Declaration, exports :: Map String ExportDecl}

data Declaration = FuncDecl WasmFuncT | GlobalDecl String (Maybe String) Bool InitValue
type ExportDecl = (ExportType, String)
data ExportType = ExpFunc | ExpGlobal

findIn :: (WasmModuleT -> Map String a) -> String -> WasmModuleT -> Maybe a
findIn getf key mod = Map.lookup key (getf mod)

addDeclaration :: String -> Declaration -> WasmModuleT -> WasmModuleT
addDeclaration key val mod = mod {declarations = Map.insert key val (declarations mod)}

addExport :: String -> ExportDecl -> WasmModuleT -> WasmModuleT
addExport key val mod = mod {exports = Map.insert key val (exports mod)}

data BuilderContext = BuilderContext {wasmmod :: WasmModuleT}

newWasmModule :: WasmModuleT
newWasmModule = WasmModuleT {declarations = Map.empty, exports = Map.empty}

newCtx :: BuilderContext
newCtx = BuilderContext { wasmmod = newWasmModule}

type ModuleBuilder = ExceptT String (State BuilderContext)

createModule :: ModuleBuilder () -> Either String WasmModuleT
createModule b =
  let (result, ctx) = runState (runExceptT b) newCtx in
    case result of
      Right _ -> Right (wasmmod ctx)
      Left e -> Left e

updateMod :: WasmModuleT -> ModuleBuilder ()
updateMod mod = do
  ctx <- get
  put ctx {wasmmod = mod}

addFunc :: WasmFunc p v r -> ModuleBuilder ()
addFunc (WasmFunc _ func@(WasmFuncT name expname _ _ _ _)) = do
  let decl = FuncDecl func
  mod <- gets wasmmod
  case findIn declarations name mod of
    Just _ -> throwE $ "Name is already declared: " ++ name
    Nothing -> updateMod (addDeclaration name decl mod) -- TODO: recurse to find implicitly called functions?

  case expname of
    Nothing -> return ()
    Just ename -> do
      mod <- gets wasmmod
      updateMod (addExport name (ExpFunc, ename) mod)

addGlobal :: (Mutability m) => GlobalVar m t -> ModuleBuilder ()
addGlobal (GlobalVar mut name expname init) = do
  let decl = GlobalDecl name expname (isMutable mut) init
  mod <- gets wasmmod
  case findIn declarations name mod of
    Just _ -> throwE $ "Name is already declared: " ++ name
    Nothing -> updateMod (addDeclaration name decl mod)

  case expname of
    Nothing -> return ()
    Just ename -> do
      mod <- gets wasmmod
      updateMod (addExport name (ExpGlobal, ename) mod)

{- Print to WAT Functions -}

buildModule :: WasmModule -> Either String String
buildModule mod = fmap (runShows . go) mod
  where
    go mod =
      ("(module \n" ++) .
      printExports (exports mod) .
      printDeclarations (declarations mod) .
      (")\n" ++)

runShows :: (String -> String) -> String
runShows f = f ""

printExports :: Map String ExportDecl -> ShowS
printExports = Map.foldrWithKey go id
  where
    go k v acc = printModuleTab . printExport k v . acc

printExport :: String -> ExportDecl -> ShowS
printExport local (exptype, expname) =
  ("(export \"" ++) . (expname ++) . ("\" (" ++) . prefix exptype . printName local . ("))\n" ++)
  where
    prefix ExpFunc = ("func " ++)
    prefix ExpGlobal = ("global " ++)

printDeclarations :: Map String Declaration -> ShowS
printDeclarations = Map.foldr go id
  where
    go v acc = printDeclaration v . acc

printDeclaration :: Declaration -> ShowS
printDeclaration (FuncDecl f) = printFunc f
printDeclaration (GlobalDecl name _  mut init) =
  printModuleTab . ("(global " ++) . printName name . showinit init . ("))\n" ++)
  where
    showinit (InitI i) = ismut mut "i32" . (" (i32.const " ++) . shows i
    showinit (InitF f) = ismut mut "f32" . (" (f32.const " ++) . shows f
    ismut True t = (" (mut " ++) . (t ++) . (")" ++)
    ismut False t = (" " ++) . (t ++)

printFunc :: WasmFuncT -> ShowS
printFunc (WasmFuncT name _ params locals results body) =
  printModuleTab . ("(func " ++) . printName name . (" " ++) .
  (printVars "param" params) .
  (printVars "result" results) .
  (printVars "local" locals) . ("\n" ++) .
  evalState (printInstr body) newPrintState .
  printModuleTab . (")\n" ++)

printName :: String -> ShowS
printName name = ("$" ++) . (name ++)

printVars :: String -> [TypeTag] -> ShowS
printVars _ [] = id
printVars prefix tags = ("(" ++) . (prefix ++) . (foldr (.) id $ map showtag tags) . (") " ++)
  where
    showtag tag =  (" " ++) . shows tag

data PrintState = PrintState {labelid :: Int, tabs :: Int}

moduleTab :: Int
moduleTab = 1

printTabs :: Int -> ShowS
printTabs 0 = id
printTabs n = ("  " ++) . printTabs (n-1)

printModuleTab :: ShowS
printModuleTab = printTabs moduleTab

newPrintState :: PrintState
newPrintState = PrintState {labelid = 0, tabs = moduleTab + 1}

printInstr :: Instr -> State PrintState ShowS
printInstr (Sequence instrs) =
  foldl go (pure id) $ fmap printInstr instrs
  where
    go :: State PrintState ShowS -> State PrintState ShowS -> State PrintState ShowS
    go acc m = do
      s1 <- acc
      s2 <- m
      tab <- gets tabs
      return $ s1 . (printTabs tab) . s2 . ("\n" ++)

printInstr (I32Const i) = return $ ("i32.const " ++) . shows i
printInstr (I32Binary b) = return $ ("i32." ++) . shows b
printInstr (I32Unary u) = return $ ("i32." ++) . shows u
printInstr (I32Compare r) = return $ ("i32." ++) . shows r
printInstr (F32Const f) = return $ ("f32.const " ++) . shows f
printInstr (F32Binary b) = return $ ("f32." ++) . shows b
printInstr (F32Compare r) = return $ ("f32." ++) . shows r
printInstr (F32Unary u ) = return $ ("f32." ++) . shows u
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
      (prefix ++) . printLabel n . (" " ++) .
      (printVars "param" params) .
      (printVars "result" results) . ("\n" ++) . s . (printTabs t) . (")" ++)

printInstr (Branch l ) = return $ ("br " ++) . printLabel l
printInstr (BranchIf l) = return $ ("br_if " ++) . printLabel l
printInstr (Call (WasmFuncT name _ _ _ _ _) ) = return $ ("call " ++) . printName name
printInstr (Return ) = return ("return" ++)
printInstr (LocalGet i) = return $ ("local.get " ++) . shows i
printInstr (LocalSet i) = return $ ("local.set " ++) . shows i
printInstr (GlobalGet name) = return $ ("global.get " ++) . printName name
printInstr (GlobalSet name) = return $ ("global.set " ++) . printName name

printLabel :: Int -> ShowS
printLabel n = ("$l" ++) . shows n
