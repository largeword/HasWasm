{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}

module HasWasm.Internal  (
  I32(..), F32(..), TypeTag(..),
  BaseType(..),
  StackT,
  Stack,
  (:+),
  Instr(..),
  GlobalVarData(..),
  BinOpI(..),
  UnOpI(..),
  RelOpI(..),
  BinOpF(..),
  UnOpF(..),
  RelOpF(..),
  LabelId,
  TypedInstr(..),
  TypedLabel(..),
  (#),
  untype,
  Var(..),
  VarTypes(..),
  InitValue(..),
  Mut(..), Imm(..),
  Mutability,
  isMutable,
  GlobalVar(..),
  WasmFunc(..),
  WasmFuncT(..),
  ImportFuncT(..),
  createFunction,
  createExpFunction,
  createLocalFunction,
  createImportFunction,
  createGlobalI32,
  createGlobalF32,
  ReturnInstr,
  FuncBody,
  FuncCallType,
  StackType
) where

import Data.Kind
import Data.Sequence

data I32 = I32
data F32 = F32

data TypeTag = I32T | F32T

class BaseType t where
  tag :: t -> TypeTag
  val :: t

instance BaseType I32 where
  tag _ = I32T
  val = I32

instance BaseType F32 where
  tag _ = F32T
  val = F32

data (Stack s, BaseType t) => StackT s t

type s :+ t = StackT s t
infixl 2 :+

type family Stack (s :: Type) :: Constraint where
  Stack (StackT s t) = ()

{- Types -}

data BinOpI = ADDI | SUBI | MULI | DIVS | DIVU | REMS | REMU | ANDI | ORI | XORI | SHLI | SHR_S | SHR_U | ROTLI | ROTRI
data UnOpI = EQZ
data RelOpI = EQI | NEI | LTI_S | LTI_U | GTI_S | GTI_U | LEI_S | LEI_U | GEI_S | GEI_U

data BinOpF = ADDF | SUBF | MULF | DIVF | MINF | MAXF | COPYSIGNF
data UnOpF = NEGF | ABSF | CEILF | FLOORF | TRUNCF | NEARESTF | SQRTF
data RelOpF = EQF | NEF | LTF | GTF | LEF | GEF

data Instr =
  Sequence (Seq Instr) |
  I32Const Int |
  I32Binary BinOpI |
  I32Unary UnOpI |
  I32Compare RelOpI |
  F32Const Float |
  F32Binary BinOpF |
  F32Unary UnOpF |
  F32Compare RelOpF |
  Block Bool [TypeTag] [TypeTag] (LabelId -> Instr) |
  Branch LabelId |
  BranchIf LabelId |
  Call WasmFuncT |
  CallImport ImportFuncT |
  Return |
  LocalGet Int |
  LocalSet Int |
  GlobalGet GlobalVarData |
  GlobalSet GlobalVarData

data GlobalVarData =
  MutI32 (GlobalVar Mut I32) |
  MutF32 (GlobalVar Mut F32) |
  ImmI32 (GlobalVar Imm I32) |
  ImmF32 (GlobalVar Imm F32)

newtype (Stack a, Stack b) => TypedInstr a b = TypedInstr Instr

type LabelId = Int
newtype (Stack s) => TypedLabel s = TypedLabel LabelId

join :: Instr -> Instr -> Instr
join (Sequence s1) (Sequence s2) = Sequence (s1 >< s2)
join (Sequence s) i = Sequence (s |> i)
join i (Sequence s) = Sequence (i <| s)
join i1 i2 = Sequence (i1 <| i2 <| empty)

(#) :: (Stack a, Stack b, Stack c) => TypedInstr a b -> TypedInstr b c -> TypedInstr a c
(TypedInstr i1) # (TypedInstr i2) = TypedInstr (join i1 i2)

infixl 0 #

untype :: TypedInstr a b -> Instr
untype (TypedInstr i) = i

{- Var and Function Types -}

newtype (BaseType t) => Var t = Var Int

data (Mutability m, BaseType t) => GlobalVar m t = GlobalVar m String (Maybe String) InitValue

createGlobalI32 :: (Mutability m) => String -> Maybe String -> Int -> GlobalVar m I32
createGlobalI32 name expname val' = GlobalVar mval name expname (InitI val')

createGlobalF32 :: (Mutability m) => String -> Maybe String -> Float -> GlobalVar m F32
createGlobalF32 name expname val' = GlobalVar mval name expname (InitF val')

data InitValue = InitI Int | InitF Float
  deriving Eq

data Mut = Mut
data Imm = Imm

class Mutability m where
  isMutable :: m -> Bool
  mval :: m

instance Mutability Mut where
  isMutable _ = True
  mval = Mut

instance Mutability Imm where
  isMutable _ = False
  mval = Imm

data WasmFunc p v r where
  WasmFunc :: (VarTypes p, VarTypes v, VarTypes r) => (p, v, r) -> WasmFuncT -> WasmFunc p v r
  ImportFunc :: (VarTypes p, VarTypes r) => (p, r) -> ImportFuncT -> WasmFunc p () r

instance Show (WasmFunc p v r) where
  show (WasmFunc _ wasmFuncT) = show wasmFuncT
  show (ImportFunc _ importFuncT) = show importFuncT

data WasmFuncT = WasmFuncT String (Maybe String) [TypeTag] [TypeTag] [TypeTag] (Instr)

instance Show WasmFuncT where
  show (WasmFuncT name expname p v r _) = "(WasmFuncT " ++ name ++ " " ++ show expname ++ " (param " ++ show p ++ ") (var " ++ show v ++") (result " ++ show r ++ "))"
  -- does not show the body to prevent infinite loop

instance Eq WasmFuncT where
  (WasmFuncT name1 expname1 p1 v1 r1 _) == (WasmFuncT name2 expname2 p2 v2 r2 _) =
    name1 == name2 && expname1 == expname2 && p1 == p2 && v1 == v2 && r1 == r2
    -- does not compare the body to prevent infinite loop

data ImportFuncT = ImportFuncT String String String [TypeTag] [TypeTag]

instance Show ImportFuncT where
  show (ImportFuncT immod imname locname p r) = "(ImportFuncT " ++ immod ++ " " ++ imname ++ " " ++ locname ++ " (param " ++ show p ++ ") (result " ++ show r ++ "))"

instance Eq ImportFuncT where
  (ImportFuncT immod1 imname1 locname1 p1 r1) == (ImportFuncT immod2 imname2 locname2 p2 r2) =
    immod1 == immod2 && imname1 == imname2 && locname1 == locname2 && p1 == p2 && r1 == r2

type FuncBody s r = TypedInstr s (StackType r s)
type ReturnInstr s r = forall s2. TypedInstr (StackType r s) s2
type FuncCallType s p r = TypedInstr (StackType p s) (StackType r s)

createFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (Maybe String) -> (VarSet p -> VarSet v -> ReturnInstr (StackT s t) r -> FuncBody (StackT s t) r) -> WasmFunc p v r

createFunction name expname body =
  WasmFunc proxy $ WasmFuncT name expname (typetags p) (typetags v) (typetags r) (untype funcbody)
  where
    proxy@(p, v, r) = (value, value, value)
    (params, n1) = (genvar p 0)
    (vars, _) = (genvar v n1)
    returnInstr = TypedInstr Return
    funcbody = body params vars returnInstr

createExpFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (VarSet p -> VarSet v -> ReturnInstr (StackT s t) r -> FuncBody (StackT s t) r) -> WasmFunc p v r
createExpFunction name body = createFunction name (Just name) body

createLocalFunction :: (VarTypes p, VarTypes v, VarTypes r) =>
  String -> (VarSet p -> VarSet v -> ReturnInstr (StackT s t) r -> FuncBody (StackT s t) r) -> WasmFunc p v r
createLocalFunction name body = createFunction name Nothing body

createImportFunction :: (VarTypes p, VarTypes r) => String -> String -> String -> WasmFunc p () r
createImportFunction mod imname locname =
  ImportFunc proxy $ ImportFuncT mod imname locname (typetags p) (typetags r)
  where
    proxy@(p, r) = (value, value)

{- Param & Var Types -}

class VarTypes v where
  value :: v
  genvar :: v -> Int -> (VarSet v, Int)
  typetags :: v -> [TypeTag]

instance VarTypes () where
  value = ()
  genvar _ i = ((), i)
  typetags _ = []

instance VarTypes I32 where
  value = I32
  genvar _ i = (Var i, i+1)
  typetags t = [tag t]

instance VarTypes F32 where
  value = F32
  genvar :: F32 -> Int -> (VarSet F32, Int)
  genvar _ i = (Var i, i+1)
  typetags t = [tag t]

instance (BaseType t1, BaseType t2) => VarTypes (t1, t2) where
  value = (val, val)
  genvar _ i = ((Var i, Var $ i + 1), i + 2)
  typetags :: (BaseType t1, BaseType t2) => (t1, t2) -> [TypeTag]
  typetags (t1, t2) = [tag t1, tag t2]

instance (BaseType t1, BaseType t2, BaseType t3) => VarTypes (t1, t2, t3) where
  value = (val, val, val)
  genvar _ i = ((Var i, Var $ i + 1, Var $ i + 2), i + 3)
  typetags (t1, t2, t3) = [tag t1, tag t2, tag t3]

instance (BaseType t1, BaseType t2, BaseType t3, BaseType t4) => VarTypes (t1, t2, t3, t4) where
  value = (val, val, val, val)
  genvar _ i = ((Var i, Var $ i + 1, Var $ i + 2, Var $ i + 3), i + 4)
  typetags (t1, t2, t3, t4) = [tag t1, tag t2, tag t3, tag t4]

instance (BaseType t1, BaseType t2, BaseType t3, BaseType t4, BaseType t5) => VarTypes (t1, t2, t3, t4, t5) where
  value = (val, val, val, val, val)
  genvar _ i = ((Var i, Var $ i + 1, Var $ i + 2, Var $ i + 3, Var $ i + 4), i + 5)
  typetags (t1, t2, t3, t4, t5) = [tag t1, tag t2, tag t3, tag t4, tag t5]

type family VarSet v where
  VarSet () = ()
  VarSet I32 = Var I32
  VarSet F32 = Var F32
  VarSet (t1, t2) = (Var t1, Var t2)
  VarSet (t1, t2, t3) = (Var t1, Var t2, Var t3)
  VarSet (t1, t2, t3, t4) = (Var t1, Var t2, Var t3, Var t4)
  VarSet (t1, t2, t3, t4, t5) = (Var t1, Var t2, Var t3, Var t4, Var t5)

type family StackType t s where
  StackType () s = s
  StackType I32 s = s :+ I32
  StackType F32 s = s :+ F32
  StackType (t1, t2) s = s :+ t1 :+ t2
  StackType (t1, t2, t3) s = s :+ t1 :+ t2 :+ t3
  StackType (t1, t2, t3, t4) s = s :+ t1 :+ t2 :+ t3 :+ t4
  StackType (t1, t2, t3, t4, t5) s = s :+ t1 :+ t2 :+ t3 :+ t4 :+ t5

{- Show & Eq Instances -}

deriving instance Eq TypeTag
deriving instance Eq BinOpI
deriving instance Eq UnOpI
deriving instance Eq RelOpI
deriving instance Eq BinOpF
deriving instance Eq UnOpF
deriving instance Eq RelOpF

instance Show TypeTag where
  show I32T = "i32"
  show F32T = "f32"

instance Show BinOpI where
  show ADDI = "add"
  show SUBI = "sub"
  show MULI = "mul"
  show DIVS = "div_s"
  show DIVU = "div_u"
  show REMS = "rem_s"
  show REMU = "rem_u"
  show ANDI = "and"
  show ORI = "or"
  show XORI = "xor"
  show SHLI = "shl"
  show SHR_S = "shr_s"
  show SHR_U = "shr_u"
  show ROTLI = "rotl"
  show ROTRI = "rotr"

instance Show UnOpI where
  show EQZ = "eqz"

instance Show RelOpI where
  show EQI = "eq"
  show NEI = "ne"
  show LTI_S = "lt_s"
  show LTI_U = "lt_u"
  show GTI_S = "gt_s"
  show GTI_U = "gt_u"
  show LEI_S = "le_s"
  show LEI_U = "le_u"
  show GEI_S = "ge_s"
  show GEI_U = "ge_u"

instance Show BinOpF where
  show ADDF = "add"
  show SUBF = "sub"
  show MULF = "mul"
  show DIVF  = "div"
  show MINF = "min"
  show MAXF = "max"
  show COPYSIGNF = "copysign"

instance Show UnOpF where
  show NEGF = "neg"
  show ABSF = "abs"
  show CEILF = "ceil"
  show FLOORF = "floor"
  show TRUNCF = "trunc"
  show NEARESTF = "nearest"
  show SQRTF = "sqrt"

instance Show RelOpF where
  show EQF = "eq"
  show NEF = "ne"
  show LTF = "lt"
  show GTF = "gt"
  show LEF = "le"
  show GEF = "ge"

instance Show Instr where
  show (Sequence s) = show s
  show (I32Const i) = "i32.const " ++ show i
  show (I32Binary op) = "i32." ++ show op
  show (I32Unary op) = "i32." ++ show op
  show (I32Compare op) = "i32." ++ show op
  show (F32Const f) = "f32.const " ++ show f
  show (F32Binary op) = "f32." ++ show op
  show (F32Unary op) = "f32." ++ show op
  show (F32Compare op) = "f32." ++ show op
  show (Block b typeP typeV _) = "block " ++ show b ++ " (" ++ show typeP ++ ") (" ++ show typeV ++ ")"
  show (Branch i) = "br " ++ show i
  show (BranchIf i) = "br_if " ++ show i
  show (Call wasmFuncT) = "call " ++ getFuncTName wasmFuncT
  show (CallImport f) = "call " ++ getImportTName f
  show Return = "return"
  show (LocalGet i) = "local.get " ++ show i
  show (LocalSet i) = "local.set " ++ show i
  show (GlobalGet _) = "global.get "
  show (GlobalSet _) = "global.set "

getFuncTName :: WasmFuncT -> String
getFuncTName (WasmFuncT n _ _ _ _ _) = n

getImportTName :: ImportFuncT -> String
getImportTName (ImportFuncT _ _ n _ _ ) = n

instance Eq Instr where
  (Sequence s1) == (Sequence s2) = s1 == s2
  (I32Const i1) == (I32Const i2) = i1 == i2
  (I32Binary op1) == (I32Binary op2) = op1 == op2
  (I32Unary op1) == (I32Unary op2) = op1 == op2
  (I32Compare op1) == (I32Compare op2) = op1 == op2
  (F32Const f1) == (F32Const f2) = f1 == f2
  (F32Binary op1) == (F32Binary op2) = op1 == op2
  (F32Unary op1) == (F32Unary op2) = op1 == op2
  (F32Compare op1) == (F32Compare op2) = op1 == op2
  (Block b1 typeP1 typeV1 _) == (Block b2 typeP2 typeV2 _) = b1 == b2 && typeP1 == typeP2 && typeV1 == typeV2
  (Branch i1) == (Branch i2) = i1 == i2
  (BranchIf i1) == (BranchIf i2) = i1 == i2
  (Call wasmFuncT1) == (Call wasmFuncT2) = wasmFuncT1 == wasmFuncT2
  (CallImport f1) == (CallImport f2) = f1 == f2
  Return == Return = True
  (LocalGet i1) == (LocalGet i2) = i1 == i2
  (LocalSet i1) == (LocalSet i2) = i1 == i2
  (GlobalGet _) == (GlobalGet _) = True
  (GlobalSet _) == (GlobalSet _) = True
  _ == _ = False
