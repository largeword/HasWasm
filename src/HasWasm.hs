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
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import qualified Data.Map as Map
import HasWasm.Instruction (call)

{- WASM Module -}

type WasmModule = Either String WasmModuleT
data WasmModuleT = WasmModuleT {
  declarations :: Map String Declaration,
  exports :: Map String ExportDecl,
  imports :: Map String ImportFuncT, 
  isAddedExplicitly :: Map String Bool}

data Declaration = FuncDecl WasmFuncT | GlobalDecl String (Maybe String) Bool InitValue | ImportFuncDecl
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

-- To store whether a function is added explicitly (by user) or implicitly (by calling)
addExplicitly :: String -> Bool -> WasmModuleT -> WasmModuleT
addExplicitly key val mod = mod {isAddedExplicitly = Map.insert key val (isAddedExplicitly mod)}

data BuilderContext = BuilderContext {wasmmod :: WasmModuleT}

newWasmModule :: WasmModuleT
newWasmModule = WasmModuleT {declarations = Map.empty, exports = Map.empty, imports = Map.empty, isAddedExplicitly = Map.empty}

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
  let decl = FuncDecl func
  mod <- gets wasmmod
  case findIn declarations name mod of
    -- If find a declaration, check if it is added explicitly
    Just (FuncDecl (WasmFuncT _ _ p' v' r' _)) -> case findIn isAddedExplicitly name mod of
      -- If added not explicitly, check if the signature is the same
      Just False -> do
        if (p' == p) && (v' == v) && (r' == r) then do
          -- If the signature is the same, update the declaration into explicitly added one
          updateMod (addExplicitly name True mod)
        else throwE $ "Function " ++ name ++ " is already declared and has different signature"
      -- If added explicitly, throw error, since it is already defined
      Just True -> throwE $ "Function " ++ name ++ " is already defined"
      -- If not known how it was added, but it is already there, throw error, something goes wrong!
      Nothing -> throwE $ "addFunc PANIC! Name is already declared but not known how it was added: " ++ name
    
    Nothing -> do
      updateMod (addDeclaration name decl mod) -- TODO: recurse to find implicitly called functions?
      updateMod (addExplicitly name True mod)
      implicitlyCalledFuncAdd funcBody
    _ -> throwE $ "Name is already declared but not a function: " ++ name

  case expname of
    Nothing -> return ()
    Just ename -> do
      mod <- gets wasmmod
      updateMod (addExport name (ExpFunc, ename) mod)

addFunc (ImportFunc _ func@(ImportFuncT _ _ name _ _)) = do
  mod <- gets wasmmod
  case findIn declarations name mod of
    Just _ -> throwE $ "Name is already declared: " ++ name
    Nothing -> updateMod (addDeclaration name ImportFuncDecl mod) -- only for marking the name usage

  -- the actual definition is stored in imports
  mod <- gets wasmmod
  updateMod (addImport name func mod)


{- Handle implicitly called function -}

-- Extract all the functions called in the body of a declaring function
lookupCalledFunc :: Instr -> [WasmFuncT] -> [WasmFuncT]
lookupCalledFunc funcBody funcList = 
  case funcBody of
    Call wasmFuncT -> funcList ++ [wasmFuncT]
    Sequence instrSeq -> do
      lookupInstrSeq instrSeq funcList
    _ -> funcList

-- Extract all the functions called in a sequence of instructions
lookupInstrSeq :: Seq Instr -> [WasmFuncT] -> [WasmFuncT]
lookupInstrSeq funcBody funcList = 
  case funcBody of
    Empty -> funcList
    x :<| xs -> case x of
      Call wasmFuncT -> lookupInstrSeq xs (funcList ++ [wasmFuncT])
      _ -> lookupInstrSeq xs funcList

-- Implicitly add called functions 
implicitlyCalledFuncAdd :: Instr -> ModuleBuilder ()
implicitlyCalledFuncAdd funcBody = do
  let funcList = lookupCalledFunc funcBody []
  implicitlyCalledFuncAddHelper funcList

implicitlyCalledFuncAddHelper :: [WasmFuncT] -> ModuleBuilder ()
implicitlyCalledFuncAddHelper funcs = do
  case funcs of
    f : fs -> do
      let (WasmFuncT name' expname' p' v' r' _) = f
      let decl' = FuncDecl f
      mod <- gets wasmmod
      case findIn declarations name' mod of
        Nothing -> do
          updateMod (addDeclaration name' decl' mod)
          updateMod (addExplicitly name' False mod)
          implicitlyCalledFuncAddHelper fs
        Just (FuncDecl (WasmFuncT _ _ p v r _)) -> do
          -- No need to change the isAddedExplicitly flag
          if (p' == p) && (v' == v) && (r' == r) then implicitlyCalledFuncAddHelper fs
          else throwE $ "Called function " ++ name' ++ " is already declared but has different signature"
        _ -> throwE $ "implicitlyCalledFuncAddHelper PANIC! Name is already declared but the called function is not a function: " ++ name'

      case expname' of
        Nothing -> return ()
        Just ename' -> do
          mod <- gets wasmmod
          updateMod (addExport name' (ExpFunc, ename') mod)
    [] -> return ()


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
printDeclaration (FuncDecl f) = printFunc f
printDeclaration (GlobalDecl name _  mut init) =
  printModuleTab . ("(global " ++) . printName name . showinit init . ("))\n" ++)
  where
    showinit (InitI i) = ismut mut "i32" . (" (i32.const " ++) . shows i
    showinit (InitF f) = ismut mut "f32" . (" (f32.const " ++) . shows f
    ismut True t = (" (mut " ++) . (t ++) . (")" ++)
    ismut False t = (" " ++) . (t ++)
printDeclaration (ImportFuncDecl) = id

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
printInstr (Return ) = return ("return" ++)
printInstr (LocalGet i) = return $ ("local.get " ++) . shows i
printInstr (LocalSet i) = return $ ("local.set " ++) . shows i
printInstr (GlobalGet name) = return $ ("global.get " ++) . printName name
printInstr (GlobalSet name) = return $ ("global.set " ++) . printName name

printLabel :: Int -> ShowS
printLabel n = ("$l" ++) . shows n
