{-# LANGUAGE GADTs #-}
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
  createImportFunction
) where

import HasWasm.Internal
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Map ( Map )
import Data.Sequence
import qualified Data.Map as Map

{- WASM Module -}

type WasmModule = Either String WasmModuleT
data WasmModuleT = WasmModuleT {
  declarations :: Map String Declaration,
  exports :: Map String ExportDecl,
  imports :: Map String ImportFuncT}

data Declaration = FuncDecl WasmFuncT Bool | GlobalDecl String (Maybe String) Bool InitValue Bool | ImportFuncDecl Bool
type ExportDecl = (ExportType, String)
data ExportType = ExpFunc | ExpGlobal

findIn :: (WasmModuleT -> Map String a) -> String -> WasmModuleT -> Maybe a
findIn getf key mod = Map.lookup key (getf mod)

addDeclaration :: String -> Declaration -> WasmModuleT -> WasmModuleT
addDeclaration key val mod = mod {declarations = Map.insert key val (declarations mod)}

addExport :: String -> ExportDecl -> WasmModuleT -> WasmModuleT
addExport key val mod = mod {exports = Map.insert key val (exports mod)}

addImport :: String -> ImportFuncT -> WasmModuleT -> WasmModuleT
addImport key val mod = mod {imports = Map.insert key val (imports mod)}

data BuilderContext = BuilderContext {wasmmod :: WasmModuleT}

newWasmModule :: WasmModuleT
newWasmModule = WasmModuleT {declarations = Map.empty, exports = Map.empty, imports = Map.empty}

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
addFunc (WasmFunc _ func@(WasmFuncT name expname p v r funcBody)) = do
  let decl = FuncDecl func True
  mod <- gets wasmmod
  case findIn declarations name mod of
    -- If find a declaration, check if it is added explicitly
    Just (FuncDecl (WasmFuncT _ _ p' v' r' _) isAddedExplicitly) -> do
      if isAddedExplicitly then throwE $ "Function " ++ name ++ " is already defined"
      else
        if (p' == p) && (v' == v) && (r' == r) then do
          -- If the signature is the same, update the declaration into explicitly added one
          updateMod (addDeclaration name decl mod)
        else throwE $ "Function " ++ name ++ " is already declared and has different signature"

    Nothing -> do
      updateMod (addDeclaration name decl mod)
      implicitlyCalledAdd funcBody
    _ -> throwE $ "Name is already declared but not a function: " ++ name

  case expname of
    Nothing -> return ()
    Just ename -> do
      mod <- gets wasmmod
      updateMod (addExport name (ExpFunc, ename) mod)

addFunc (ImportFunc _ func@(ImportFuncT _ _ name _ _)) = do
  mod <- gets wasmmod
  case findIn declarations name mod of
    Just _ -> throwE $ "Imported name is already declared: " ++ name
    Nothing -> updateMod (addDeclaration name (ImportFuncDecl True) mod) -- only for marking the name usage

  -- the actual definition is stored in imports
  mod <- gets wasmmod
  updateMod (addImport name func mod)


{- Handle implicitly called function & global var -}

-- Extract all the functions called in the body of a declaring function
lookupCalledFunc :: Instr -> [WasmFuncT] -> [WasmFuncT]
lookupCalledFunc funcBody funcList =
  case funcBody of
    Call wasmFuncT -> funcList ++ [wasmFuncT]
    Sequence instrSeq -> do
      lookupCall instrSeq funcList
    _ -> funcList

-- Extract all the functions called in a sequence of instructions
lookupCall :: Seq Instr -> [WasmFuncT] -> [WasmFuncT]
lookupCall funcBody funcList =
  case funcBody of
    Empty -> funcList
    x :<| xs -> case x of
      Call wasmFuncT -> lookupCall xs (funcList ++ [wasmFuncT])
      _ -> lookupCall xs funcList

-- Extract all the functions called in the body of a declaring function
lookupCalledGVar :: Instr -> [GlobalVarData] -> [GlobalVarData]
lookupCalledGVar funcBody funcList =
  case funcBody of
    GlobalGet globalVar -> funcList ++ [globalVar]
    GlobalSet globalVar -> funcList ++ [globalVar]
    Sequence instrSeq -> do
      lookupGVar instrSeq funcList
    _ -> funcList

-- Extract all the global variables called in a sequence of instructions
lookupGVar :: Seq Instr -> [GlobalVarData] -> [GlobalVarData]
lookupGVar funcBody gVarList =
  case funcBody of
    Empty -> gVarList
    x :<| xs -> case x of
      GlobalGet globalVar -> lookupGVar xs (gVarList ++ [globalVar])
      GlobalSet globalVar -> lookupGVar xs (gVarList ++ [globalVar])
      _ -> lookupGVar xs gVarList

-- Implicitly add called functions
implicitlyCalledAdd :: Instr -> ModuleBuilder ()
implicitlyCalledAdd funcBody = do
  let funcList = lookupCalledFunc funcBody []
  let gVarList = lookupCalledGVar funcBody []
  implicitlyCalledFuncAdd funcList
  implicitlyCalledGVarAdd gVarList

implicitlyCalledFuncAdd :: [WasmFuncT] -> ModuleBuilder ()
implicitlyCalledFuncAdd funcs = do
  case funcs of
    f : fs -> do
      let (WasmFuncT name' expname' p' v' r' _) = f
      let decl' = FuncDecl f False
      mod <- gets wasmmod
      case findIn declarations name' mod of
        Nothing -> do
          updateMod (addDeclaration name' decl' mod)
          implicitlyCalledFuncAdd fs
        Just (FuncDecl (WasmFuncT _ _ p v r _) _) -> do
          if (p' == p) && (v' == v) && (r' == r) then implicitlyCalledFuncAdd fs
          else throwE $ "Called function " ++ name' ++ " is already declared but has different signature"
        _ -> throwE $ "implicitlyCalledFuncAdd PANIC! Name is already declared but the called function is not a function: " ++ name'

      case expname' of
        Nothing -> return ()
        Just ename' -> do
          mod <- gets wasmmod
          updateMod (addExport name' (ExpFunc, ename') mod)
    [] -> return ()

-- Implicitly add called global variables
implicitlyCalledGVarAdd :: [GlobalVarData] -> ModuleBuilder ()
implicitlyCalledGVarAdd gVars = do
  case gVars of
    v : vs -> case v of
      MutI32 (GlobalVar mut name expname init) -> do
        implicitlyCalledGVarAddHelper (GlobalVar mut name expname init)
        implicitlyCalledGVarAdd vs
      MutF32 (GlobalVar mut name expname init) -> do
        implicitlyCalledGVarAddHelper (GlobalVar mut name expname init)
        implicitlyCalledGVarAdd vs
      ImmI32 (GlobalVar mut name expname init) -> do
        implicitlyCalledGVarAddHelper (GlobalVar mut name expname init)
        implicitlyCalledGVarAdd vs
      ImmF32 (GlobalVar mut name expname init) -> do
        implicitlyCalledGVarAddHelper (GlobalVar mut name expname init)
        implicitlyCalledGVarAdd vs
    [] -> return ()

-- Add each global var implicitly called in the body of a declaring function
implicitlyCalledGVarAddHelper :: (Mutability m) => GlobalVar m t -> ModuleBuilder ()
implicitlyCalledGVarAddHelper gVar = do
  let (GlobalVar mut name expname initVar) = gVar
  let decl = GlobalDecl name expname (isMutable mut) initVar False
  mod <- gets wasmmod
  case findIn declarations name mod of
    Nothing -> do
      updateMod (addDeclaration name decl mod)
    Just (GlobalDecl name' expname' mutable' initVar' _) -> do
      if (expname == expname') && (isMutable mut == mutable') && (initVar == initVar') then return ()
      else throwE $ "Called global var " ++ name' ++ " is already declared but has different specs"
    _ -> throwE $ "implicitlyCalledGVarAddHelper PANIC! Name is already declared but the called global var is not a global var: " ++ name
  case expname of
    Nothing -> return ()
    Just ename -> do
      mod <- gets wasmmod
      updateMod (addExport name (ExpGlobal, ename) mod)


addGlobal :: (Mutability m) => GlobalVar m t -> ModuleBuilder ()
addGlobal (GlobalVar mut name expname init) = do
  let decl = GlobalDecl name expname (isMutable mut) init True
  mod <- gets wasmmod
  case findIn declarations name mod of
    Just (GlobalDecl _ _ _ _ isAddedExplicitly) -> do
      if isAddedExplicitly then throwE $ "Global variable " ++ name ++ " is already defined"
      else updateMod (addDeclaration name decl mod)
    Nothing -> updateMod (addDeclaration name decl mod)
    _ -> throwE $ "Name is already declared but not a global var: " ++ name

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
      printImports (imports mod) .
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

printImports :: Map String ImportFuncT -> ShowS
printImports = Map.foldr go id
  where
    go v acc = printModuleTab . printImportFunc v . acc

printImportFunc :: ImportFuncT -> ShowS
printImportFunc (ImportFuncT immod imname locname params results) =
  ("(import " ++) . printQuot immod . (" " ++) .
  printQuot imname . (" (func " ++) . printName locname . (" " ++) .
  (printVars "param" params) .
  (printVars "result" results) . ("))\n" ++)

printDeclarations :: Map String Declaration -> ShowS
printDeclarations = Map.foldr go id
  where
    go v acc = printDeclaration v . acc

printDeclaration :: Declaration -> ShowS
printDeclaration (FuncDecl f _) = printFunc f
printDeclaration (GlobalDecl name _  mut init _) =
  printModuleTab . ("(global " ++) . printName name . showinit init . ("))\n" ++)
  where
    showinit (InitI i) = ismut mut "i32" . (" (i32.const " ++) . shows i
    showinit (InitF f) = ismut mut "f32" . (" (f32.const " ++) . shows f
    ismut True t = (" (mut " ++) . (t ++) . (")" ++)
    ismut False t = (" " ++) . (t ++)
printDeclaration (ImportFuncDecl _) = id

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

printQuot :: String -> ShowS
printQuot name = ("\"" ++) . (name ++) . ("\"" ++)

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
printInstr (CallImport (ImportFuncT _ _ name _ _) ) = return $ ("call " ++) . printName name
printInstr (Return ) = return ("return" ++)
printInstr (LocalGet i) = return $ ("local.get " ++) . shows i
printInstr (LocalSet i) = return $ ("local.set " ++) . shows i

printInstr (GlobalGet (MutI32 (GlobalVar _ name _ _))) = return $ ("global.get " ++) . printName name
printInstr (GlobalGet (ImmI32 (GlobalVar _ name _ _))) = return $ ("global.get " ++) . printName name
printInstr (GlobalGet (MutF32 (GlobalVar _ name _ _))) = return $ ("global.get " ++) . printName name
printInstr (GlobalGet (ImmF32 (GlobalVar _ name _ _))) = return $ ("global.get " ++) . printName name

printInstr (GlobalSet (MutI32 (GlobalVar _ name _ _))) = return $ ("global.set " ++) . printName name
printInstr (GlobalSet (ImmI32 (GlobalVar _ name _ _))) = return $ ("global.set " ++) . printName name
printInstr (GlobalSet (MutF32 (GlobalVar _ name _ _))) = return $ ("global.set " ++) . printName name
printInstr (GlobalSet (ImmF32 (GlobalVar _ name _ _))) = return $ ("global.set " ++) . printName name

printLabel :: Int -> ShowS
printLabel n = ("$l" ++) . shows n
