{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell API for defining Objective-C classes backed by
-- Haskell method implementations.
--
-- = Usage
--
-- At the splice site, you must have these imports:
--
-- @
-- import ObjC.Runtime
-- import ObjC.Runtime.DynClass
-- import ObjC.Runtime.TH
-- import Foreign.Ptr
-- import Foreign.Marshal.Array (mallocArray, peekElemOff, pokeElemOff)
-- import Foreign.C.Types
-- import System.IO.Unsafe (unsafePerformIO)
-- @
--
-- Then at top level:
--
-- @
-- \$(defineClass \"CounterTarget\" \"NSObject\" $ do
--   instanceMethod \"increment:\" [t| RawId -> IO () |]
--   instanceMethod \"decrement:\" [t| RawId -> IO () |]
-- )
-- @
--
-- This generates a record type @CounterTargetImpl@, a class CAF
-- @counterTargetClass@, and a constructor @newCounterTarget@.
module ObjC.Runtime.TH
  ( defineClass
  , classMethod
  , instanceMethod
  , ClassDef
  ) where

import Control.Monad (forM)
import Data.Char (toLower, toUpper)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Language.Haskell.TH

import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr, castPtrToFunPtr)
import Foreign.C.Types (CBool, CLong, CDouble)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.C.String (withCString)
import System.IO.Unsafe (unsafePerformIO)

import ObjC.Runtime.Types (ObjCObject, ObjCSel, RawId(..), Class, IMP)
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.DynClass (addVtableIvar, addDeallocHandler, readVtable, writeVtable, addObjCMethod, castIMP)

-- ---------------------------------------------------------------------------
-- Builder monad
-- ---------------------------------------------------------------------------

data MethodKind = ClassMethodKind | InstanceMethodKind
  deriving (Eq, Show)

data MethodDecl = MethodDecl
  { mdKind     :: !MethodKind
  , mdSelector :: !String
  , mdType     :: Q Type
  }

-- | Monadic builder for describing the methods of a class.
newtype ClassDef a = ClassDef { runClassDef :: IORef [MethodDecl] -> Q a }

instance Functor ClassDef where
  fmap f (ClassDef g) = ClassDef $ \ref -> f <$> g ref

instance Applicative ClassDef where
  pure a = ClassDef $ \_ -> pure a
  ClassDef f <*> ClassDef a = ClassDef $ \ref -> f ref <*> a ref

instance Monad ClassDef where
  ClassDef a >>= f = ClassDef $ \ref -> do
    x <- a ref
    runClassDef (f x) ref

-- | Declare a class method (shared across all instances).
classMethod :: String -> Q Type -> ClassDef ()
classMethod sel ty = ClassDef $ \ref ->
  runIO $ modifyIORef' ref (++ [MethodDecl ClassMethodKind sel ty])

-- | Declare an instance method (per-instance implementation).
instanceMethod :: String -> Q Type -> ClassDef ()
instanceMethod sel ty = ClassDef $ \ref ->
  runIO $ modifyIORef' ref (++ [MethodDecl InstanceMethodKind sel ty])

-- ---------------------------------------------------------------------------
-- Type analysis
-- ---------------------------------------------------------------------------

-- | Peel @A -> B -> IO C@ into @([A, B], C)@.
peelMethodType :: Type -> ([Type], Type)
peelMethodType (AppT (AppT ArrowT argTy) rest) =
  let (args, ret) = peelMethodType rest in (argTy : args, ret)
peelMethodType (AppT (ConT io) retTy)
  | io == ''IO = ([], retTy)
peelMethodType other = ([], other)

-- | ObjC type encoding character for a Haskell type.
typeEnc :: Type -> String
typeEnc (ConT n)
  | n == ''()     = "v"
  | n == ''Bool   = "B"
  | n == ''Int    = "q"
  | n == ''Double = "d"
  | n == ''RawId  = "@"
typeEnc (TupleT 0) = "v"
typeEnc _           = "@"

-- | Full ObjC type encoding: @retEnc \@ : argEnc...@
fullTypeEncoding :: [Type] -> Type -> String
fullTypeEncoding args ret = typeEnc ret ++ "@:" ++ concatMap typeEnc args

-- | Map Haskell type to C FFI type (as a TH Type).
toCType :: Type -> Q Type
toCType (ConT n)
  | n == ''()     = tupleT 0
  | n == ''Bool   = conT ''CBool
  | n == ''Int    = conT ''CLong
  | n == ''Double = conT ''CDouble
  | n == ''RawId  = [t| Ptr ObjCObject |]
toCType (TupleT 0)  = tupleT 0
toCType _            = [t| Ptr ObjCObject |]

-- | Build the C function type for an IMP:
--   @Ptr ObjCObject -> Ptr ObjCSel -> cArg0 -> ... -> IO cRet@
impCType :: [Type] -> Type -> Q Type
impCType hsArgs hsRet = do
  cArgs <- mapM toCType hsArgs
  cRet  <- toCType hsRet
  let result = appT (conT ''IO) (pure cRet)
      self   = [t| Ptr ObjCObject |]
      sel    = [t| Ptr ObjCSel |]
      all'   = [self, sel] ++ fmap pure cArgs
  foldr (\a b -> [t| $a -> $b |]) result all'

-- | A short fingerprint used to generate unique names per signature.
sigFP :: [Type] -> Type -> String
sigFP args ret = concatMap tc args ++ "_" ++ tc ret
  where
    tc (ConT n)
      | n == ''()     = "v"
      | n == ''Bool   = "B"
      | n == ''Int    = "q"
      | n == ''Double = "d"
      | n == ''RawId  = "at"
    tc (TupleT 0) = "v"
    tc _           = "at"

-- ---------------------------------------------------------------------------
-- Name helpers
-- ---------------------------------------------------------------------------

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

upperFirst :: String -> String
upperFirst []     = []
upperFirst (c:cs) = toUpper c : cs

-- | Remove colons from a selector to make a valid Haskell identifier.
sanitizeSel :: String -> String
sanitizeSel = filter (/= ':')

-- | Remove duplicates by fingerprint.
nubBySigFP :: [([Type], Type)] -> [([Type], Type)]
nubBySigFP = go []
  where
    go _ [] = []
    go seen ((a,r):rest)
      | fp `elem` seen = go seen rest
      | otherwise       = (a,r) : go (fp:seen) rest
      where fp = sigFP a r

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

-- | Generate all declarations for a Haskell-backed Objective-C class.
defineClass :: String -> String -> ClassDef () -> Q [Dec]
defineClass clsName superName builder = do
  ref <- runIO (newIORef [])
  runClassDef builder ref
  methods <- runIO (readIORef ref)

  let instMs  = filter ((== InstanceMethodKind) . mdKind) methods
      classMs = filter ((== ClassMethodKind)     . mdKind) methods

  -- Resolve all types
  rInst  <- forM instMs  $ \m -> do { ty <- mdType m; pure (m, ty, peelMethodType ty) }
  rClass <- forM classMs $ \m -> do { ty <- mdType m; pure (m, ty, peelMethodType ty) }

  let allSigs   = fmap (\(_,_,(a,r)) -> (a,r)) rInst
                ++ fmap (\(_,_,(a,r)) -> (a,r)) rClass
      uniqSigs  = nubBySigFP allSigs

  d1 <- genImplRecord clsName rInst
  d2 <- concat <$> mapM (genWrapper clsName) uniqSigs
  d3 <- concat <$> mapM (genDynamic clsName) uniqSigs
  d4 <- genCAF clsName superName rInst rClass
  d5 <- genCtor clsName rInst rClass

  pure (d1 ++ d2 ++ d3 ++ d4 ++ d5)

-- ---------------------------------------------------------------------------
-- 1. Impl record type
-- ---------------------------------------------------------------------------

genImplRecord :: String -> [(MethodDecl, Type, ([Type], Type))] -> Q [Dec]
genImplRecord clsName ms = do
  let recName = mkName (clsName ++ "Impl")
      fields  = fmap mkField ms
  pure [DataD [] recName [] Nothing [RecC recName fields] []]
  where
    mkField (m, ty, _) =
      ( mkName ("_" ++ sanitizeSel (mdSelector m))
      , Bang NoSourceUnpackedness NoSourceStrictness
      , ty
      )

-- ---------------------------------------------------------------------------
-- 2. foreign import ccall "wrapper"
-- ---------------------------------------------------------------------------

genWrapper :: String -> ([Type], Type) -> Q [Dec]
genWrapper clsName (args, ret) = do
  cTy <- impCType args ret
  let nm = mkName ("hs_wrap_" ++ clsName ++ "_" ++ sigFP args ret)
      fpTy = AppT (ConT ''FunPtr) cTy
      fullTy = AppT (AppT ArrowT cTy) (AppT (ConT ''IO) fpTy)
  pure [ForeignD (ImportF CCall Safe "wrapper" nm fullTy)]

-- ---------------------------------------------------------------------------
-- 3. foreign import ccall "dynamic"
-- ---------------------------------------------------------------------------

genDynamic :: String -> ([Type], Type) -> Q [Dec]
genDynamic clsName (args, ret) = do
  cTy <- impCType args ret
  let nm = mkName ("hs_call_" ++ clsName ++ "_" ++ sigFP args ret)
      fpTy = AppT (ConT ''FunPtr) cTy
      fullTy = AppT (AppT ArrowT fpTy) cTy
  pure [ForeignD (ImportF CCall Safe "dynamic" nm fullTy)]

-- ---------------------------------------------------------------------------
-- 4. Class CAF
-- ---------------------------------------------------------------------------

genCAF :: String -> String
       -> [(MethodDecl, Type, ([Type], Type))]
       -> [(MethodDecl, Type, ([Type], Type))]
       -> Q [Dec]
genCAF clsName superName instMs _classMs = do
  let cafN = mkName (lowerFirst clsName ++ "Class")

  -- Build the IO action that creates and registers the class
  clsN   <- newName "cls"
  superN <- newName "super_"

  -- Statements: get superclass, allocate pair, add vtable ivar
  let getSuperStmt = BindS (VarP superN)
        (VarE 'getRequiredClass `AppE` LitE (StringL superName))

  allocExp <- [| withCString $(litE (stringL clsName)) $ \n ->
                   objc_allocateClassPair $(varE superN) n 0 |]
  let allocStmt = BindS (VarP clsN) allocExp

  let addIvarStmt = NoBindS (VarE 'addVtableIvar `AppE` VarE clsN)

  -- Stub registration statements (one per instance method)
  stubStmts <- concat <$> mapM (\(i, m) -> genStubReg clsName clsN i m) (zip [0..] instMs)

  -- Register a dealloc handler that frees the vtable on instance destruction
  let nSlots = length instMs
      deallocStmt = NoBindS
        (VarE 'addDeallocHandler `AppE` VarE clsN `AppE` LitE (IntegerL (fromIntegral nSlots)))

  let regStmt = NoBindS (VarE 'objc_registerClassPair `AppE` VarE clsN)
      retStmt = NoBindS (VarE 'pure `AppE` VarE clsN)

  let allStmts = [getSuperStmt, allocStmt, addIvarStmt] ++ stubStmts ++ [deallocStmt, regStmt, retStmt]
      ioAction = DoE Nothing allStmts
      cafBody  = VarE 'unsafePerformIO `AppE` ioAction

  let sig     = SigD cafN (ConT ''Class)
      def     = ValD (VarP cafN) (NormalB cafBody) []
      pragma  = PragmaD (InlineP cafN NoInline FunLike AllPhases)

  pure [sig, def, pragma]

-- | Generate the statements that create a dispatch stub IMP and register
-- it as a method on the class being built.
genStubReg :: String -> Name -> Int -> (MethodDecl, Type, ([Type], Type)) -> Q [Stmt]
genStubReg clsName clsN slotIdx (m, _ty, (args, ret)) = do
  let fp       = sigFP args ret
      wrapN    = mkName ("hs_wrap_" ++ clsName ++ "_" ++ fp)
      callN    = mkName ("hs_call_" ++ clsName ++ "_" ++ fp)
      sel      = mdSelector m
      tEnc     = fullTypeEncoding args ret
      nArgs    = length args

  impN  <- newName ("stub_" ++ sanitizeSel sel)
  selfN <- newName "self"
  selN  <- newName "_cmd"
  argNs <- mapM (\i -> newName ("x" ++ show i)) [0 .. nArgs - 1]

  -- Build dispatch body:
  --   do vt <- readVtable self
  --      p  <- peekElemOff vt slotIdx
  --      hs_call_Xxx (castPtrToFunPtr p) self _cmd x0 x1 ...
  vtN <- newName "vt"
  pN  <- newName "p"

  let readVtStmt = BindS (VarP vtN) (VarE 'readVtable `AppE` VarE selfN)
      peekStmt   = BindS (VarP pN)
        (VarE 'peekElemOff `AppE` VarE vtN `AppE` LitE (IntegerL (fromIntegral slotIdx)))
      castExp    = VarE 'castPtrToFunPtr `AppE` VarE pN
      callExp    = foldl AppE (VarE callN) ([castExp, VarE selfN, VarE selN] ++ fmap VarE argNs)
      callStmt   = NoBindS callExp
      stubBody   = DoE Nothing [readVtStmt, peekStmt, callStmt]
      stubLam    = LamE (fmap VarP ([selfN, selN] ++ argNs)) stubBody

  -- Bind the wrapped stub
  let wrapStmt = BindS (VarP impN) (VarE wrapN `AppE` stubLam)

  -- Register it with addObjCMethod
  let addExp = foldl AppE (VarE 'addObjCMethod)
        [ VarE clsN
        , LitE (StringL sel)
        , LitE (StringL tEnc)
        , VarE impN
        ]
      addStmt = NoBindS addExp

  pure [wrapStmt, addStmt]

-- ---------------------------------------------------------------------------
-- 5. Constructor
-- ---------------------------------------------------------------------------

genCtor :: String
        -> [(MethodDecl, Type, ([Type], Type))]
        -> [(MethodDecl, Type, ([Type], Type))]
        -> Q [Dec]
genCtor clsName instMs classMs = do
  let ctorN   = mkName ("new" ++ upperFirst clsName)
      cafN    = mkName (lowerFirst clsName ++ "Class")
      implTyN = mkName (clsName ++ "Impl")
      nSlots  = length instMs

  -- Parameters: class method impls, then the IO builder
  cmArgNs  <- mapM (\(m,_,_) -> newName ("cm_" ++ sanitizeSel (mdSelector m))) classMs
  builderN <- newName "mkImpl"

  -- Build type signature
  let cmArgTys   = fmap (\(_,ty,_) -> ty) classMs
      builderTy  = AppT (ConT ''IO) (ConT implTyN)
      retTy      = AppT (ConT ''IO) (ConT ''RawId)
      fullTy     = foldr (\a b -> AppT (AppT ArrowT a) b)
                         (AppT (AppT ArrowT builderTy) retTy)
                         cmArgTys

  -- Build body
  instN  <- newName "inst"
  implN  <- newName "impl"
  vtblN  <- newName "vtbl"

  let createStmt = BindS (VarP instN)
        (VarE 'class_createInstance `AppE` VarE cafN `AppE` LitE (IntegerL 0))
      runBuilderStmt = BindS (VarP implN)
        (VarE builderN)
      allocVtblStmt = BindS (VarP vtblN)
        (VarE 'mallocArray `AppE` LitE (IntegerL (fromIntegral nSlots)))

  -- Generate slot fill statements
  fillStmts <- concat <$> mapM (\(i, m) -> genFill clsName implTyN implN vtblN i m) (zip [0..] instMs)

  let writeVtStmt = NoBindS
        (VarE 'writeVtable `AppE` VarE instN `AppE` VarE vtblN)
      retStmt = NoBindS
        (VarE 'pure `AppE` VarE instN)

  let bodyStmts = [createStmt, runBuilderStmt, allocVtblStmt]
                  ++ fillStmts
                  ++ [writeVtStmt, retStmt]
      body = DoE Nothing bodyStmts

  let sig = SigD ctorN fullTy
      def = FunD ctorN [Clause (fmap VarP cmArgNs ++ [VarP builderN]) (NormalB body) []]

  pure [sig, def]

-- | Generate statements that wrap a record field into a FunPtr and poke it
-- into the vtable.
genFill :: String -> Name -> Name -> Name
        -> Int -> (MethodDecl, Type, ([Type], Type))
        -> Q [Stmt]
genFill clsName _implTyN implN vtblN slotIdx (m, _ty, (args, ret)) = do
  let fp     = sigFP args ret
      wrapN  = mkName ("hs_wrap_" ++ clsName ++ "_" ++ fp)
      fldN   = mkName ("_" ++ sanitizeSel (mdSelector m))
      nArgs  = length args

  fpN  <- newName ("fp" ++ show slotIdx)
  selfN <- newName "_self"
  selN  <- newName "_cmd"
  argNs <- mapM (\i -> newName ("a" ++ show i)) [0 .. nArgs - 1]

  -- Build wrapper lambda body: convert C args to Haskell, call field, convert return
  let fieldAccess = AppE (VarE fldN) (VarE implN)
  callBody <- buildCall fieldAccess args ret argNs

  let wrapLam = LamE (fmap VarP ([selfN, selN] ++ argNs)) callBody

  let wrapStmt = BindS (VarP fpN) (AppE (VarE wrapN) wrapLam)
      pokeStmt = NoBindS $
        foldl AppE (VarE 'pokeElemOff)
          [ VarE vtblN
          , LitE (IntegerL (fromIntegral slotIdx))
          , AppE (VarE 'castFunPtrToPtr) (VarE fpN)
          ]

  pure [wrapStmt, pokeStmt]

-- | Build the expression that calls the Haskell method, converting
-- C-typed args to Haskell types and the return back to C.
buildCall :: Exp -> [Type] -> Type -> [Name] -> Q Exp
buildCall fieldAccess hsArgs hsRet argNames = do
  convArgs <- sequence (zipWith convertArg hsArgs argNames)
  let applied = foldl AppE fieldAccess convArgs
  wrapRet hsRet applied

convertArg :: Type -> Name -> Q Exp
convertArg (ConT n) v
  | n == ''RawId  = [| RawId $(varE v) |]
  | n == ''Int    = [| fromIntegral $(varE v) |]
  | n == ''Bool   = [| $(varE v) /= 0 |]
  | n == ''Double = [| realToFrac $(varE v) |]
convertArg _ v = [| RawId $(varE v) |]

wrapRet :: Type -> Exp -> Q Exp
wrapRet (ConT n) e
  | n == ''()     = pure e
  | n == ''Bool   = [| do r <- $(pure e); pure (if r then 1 else 0) |]
  | n == ''Int    = [| fromIntegral <$> $(pure e) |]
  | n == ''Double = [| realToFrac <$> $(pure e) |]
  | n == ''RawId  = [| unRawId <$> $(pure e) |]
wrapRet (TupleT 0) e = pure e
wrapRet _ e = [| unRawId <$> $(pure e) |]
