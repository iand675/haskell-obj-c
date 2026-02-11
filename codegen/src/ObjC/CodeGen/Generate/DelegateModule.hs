{-# LANGUAGE OverloadedStrings #-}

-- | Delegate module generation.
--
-- For each ObjC protocol, generates a module containing:
--
-- * An overrides record with @Maybe@-wrapped fields for each method
-- * A default overrides value (all 'Nothing')
-- * @foreign import ccall \"wrapper\"@ declarations per unique IMP signature
-- * A class CAF that creates the dynamic ObjC class (with stubs,
--   @respondsToSelector:@ override, and dealloc handler)
-- * A constructor @new\<Proto\> :: \<Proto\>Overrides -> IO RawId@
--
-- Uses 'ObjC.Runtime.StableIvar' infrastructure — no Template Haskell.
module ObjC.CodeGen.Generate.DelegateModule
  ( generateDelegateModules
  , generateDelegateModule
  ) where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap (isIdDesugared, isSELDesugared, isObjPtrDesugared)
import ObjC.CodeGen.Generate.Types (GeneratedModule(..))
import ObjC.CodeGen.Generate.Naming (lowerFirst)
import ObjC.CodeGen.Generate.Shared (propertyToMethods)

-- ---------------------------------------------------------------------------
-- Callback type classification
-- ---------------------------------------------------------------------------

-- | Classification of ObjC types for the callback direction (ObjC calls
-- into Haskell).  Each class determines the Haskell type seen by the
-- user, the C FFI type, and the conversions between them.
data CbTypeClass
  = CbObject   -- ^ @id@, @NSFoo *@, etc.
  | CbBool     -- ^ @BOOL@
  | CbLong     -- ^ @long@, @NSInteger@
  | CbULong    -- ^ @unsigned long@, @NSUInteger@
  | CbInt      -- ^ @int@
  | CbUInt     -- ^ @unsigned int@
  | CbDouble   -- ^ @double@, @CGFloat@
  | CbFloat    -- ^ @float@
  | CbVoid     -- ^ @void@
  | CbSel      -- ^ @SEL@
  | CbClass    -- ^ @Class@
  deriving (Eq, Ord, Show)

-- | Classify an ObjC type for the callback direction.  Returns 'Nothing'
-- for unsupported types (structs by value, blocks, etc.) — methods with
-- such types are omitted from the generated overrides record.
classifyCallback :: ObjCType -> Maybe CbTypeClass
classifyCallback ObjCVoid              = Just CbVoid
classifyCallback ObjCBool              = Just CbBool
classifyCallback ObjCSEL               = Just CbSel
classifyCallback (ObjCClassType _)     = Just CbClass
classifyCallback ObjCInstancetype      = Just CbObject
classifyCallback (ObjCId _ _)          = Just CbObject
classifyCallback (ObjCGeneric _ _ _)   = Just CbObject
classifyCallback (ObjCPrimitive q d)   = classifyPrim q d
classifyCallback (ObjCPointer (ObjCPrimitive _ d))
  | isIdDesugared d                    = Just CbObject
classifyCallback (ObjCQualified _ inner) = classifyCallback inner
classifyCallback _                     = Nothing

classifyPrim :: Text -> Text -> Maybe CbTypeClass
classifyPrim q d
  | d == "long" || d == "long long"                = Just CbLong
  | d == "unsigned long" || d == "unsigned long long" = Just CbULong
  | d == "int"                                     = Just CbInt
  | d == "unsigned int"                            = Just CbUInt
  | d == "double"                                  = Just CbDouble
  | d == "float"                                   = Just CbFloat
  | q == "NSInteger"                               = Just CbLong
  | q == "NSUInteger"                              = Just CbULong
  | q == "CGFloat"                                 = Just CbDouble
  | q == "NSTimeInterval"                          = Just CbDouble
  | q == "BOOL"                                    = Just CbBool
  | isIdDesugared d                                = Just CbObject
  | isSELDesugared d                               = Just CbSel
  | d == "Class"                                   = Just CbClass
  | isObjPtrDesugared d                            = Just CbObject
  | otherwise                                      = Nothing

-- ---------------------------------------------------------------------------
-- Type info tables
-- ---------------------------------------------------------------------------

-- | Haskell type for record field parameters/returns.
cbHsType :: CbTypeClass -> Text
cbHsType CbObject = "RawId"
cbHsType CbBool   = "Bool"
cbHsType CbLong   = "Int"
cbHsType CbULong  = "Int"
cbHsType CbInt    = "Int"
cbHsType CbUInt   = "Int"
cbHsType CbDouble = "Double"
cbHsType CbFloat  = "Float"
cbHsType CbVoid   = "()"
cbHsType CbSel    = "Selector"
cbHsType CbClass  = "Class"

-- | C FFI type for the IMP wrapper.
cbCType :: CbTypeClass -> Text
cbCType CbObject = "Ptr ObjCObject"
cbCType CbBool   = "CULong"
cbCType CbLong   = "CLong"
cbCType CbULong  = "CULong"
cbCType CbInt    = "CInt"
cbCType CbUInt   = "CUInt"
cbCType CbDouble = "CDouble"
cbCType CbFloat  = "CFloat"
cbCType CbVoid   = "()"
cbCType CbSel    = "Ptr ObjCSel"
cbCType CbClass  = "Ptr ObjCObject"

-- | ObjC type encoding character.
cbEnc :: CbTypeClass -> Text
cbEnc CbObject = "@"
cbEnc CbBool   = "B"
cbEnc CbLong   = "q"
cbEnc CbULong  = "Q"
cbEnc CbInt    = "i"
cbEnc CbUInt   = "I"
cbEnc CbDouble = "d"
cbEnc CbFloat  = "f"
cbEnc CbVoid   = "v"
cbEnc CbSel    = ":"
cbEnc CbClass  = "#"

-- | Signature fingerprint fragment (for deduplicating wrapper declarations).
cbFP :: CbTypeClass -> Text
cbFP CbObject = "at"
cbFP CbBool   = "B"
cbFP CbLong   = "q"
cbFP CbULong  = "Q"
cbFP CbInt    = "i"
cbFP CbUInt   = "I"
cbFP CbDouble = "d"
cbFP CbFloat  = "f"
cbFP CbVoid   = "v"
cbFP CbSel    = "sel"
cbFP CbClass  = "cls"

-- | C return type with parens when needed inside @IO@.
cbRetInIO :: CbTypeClass -> Text
cbRetInIO CbVoid   = "()"
cbRetInIO CbObject = "(Ptr ObjCObject)"
cbRetInIO CbSel    = "(Ptr ObjCSel)"
cbRetInIO CbClass  = "(Ptr ObjCObject)"
cbRetInIO c        = cbCType c

-- | C param → Haskell param conversion expression.
cbParamConv :: CbTypeClass -> Text -> Text
cbParamConv CbObject arg = "(RawId " <> arg <> ")"
cbParamConv CbBool   arg = "(" <> arg <> " /= 0)"
cbParamConv CbLong   arg = "(fromIntegral " <> arg <> ")"
cbParamConv CbULong  arg = "(fromIntegral " <> arg <> ")"
cbParamConv CbInt    arg = "(fromIntegral " <> arg <> ")"
cbParamConv CbUInt   arg = "(fromIntegral " <> arg <> ")"
cbParamConv CbDouble arg = "(realToFrac " <> arg <> ")"
cbParamConv CbFloat  arg = "(realToFrac " <> arg <> ")"
cbParamConv CbVoid   _   = "()"
cbParamConv CbSel    arg = "(Selector " <> arg <> ")"
cbParamConv CbClass  arg = "(Class (castPtr " <> arg <> "))"

-- | Haskell return → C return conversion expression.
cbRetConv :: CbTypeClass -> Text -> Text
cbRetConv CbObject r = "(castPtr (unRawId " <> r <> ") :: Ptr ObjCObject)"
cbRetConv CbBool   r = "(if " <> r <> " then 1 else 0)"
cbRetConv CbLong   r = "(fromIntegral " <> r <> ")"
cbRetConv CbULong  r = "(fromIntegral " <> r <> ")"
cbRetConv CbInt    r = "(fromIntegral " <> r <> ")"
cbRetConv CbUInt   r = "(fromIntegral " <> r <> ")"
cbRetConv CbDouble r = "(realToFrac " <> r <> ")"
cbRetConv CbFloat  r = "(realToFrac " <> r <> ")"
cbRetConv CbVoid   _ = "()"
cbRetConv CbSel    r = "(unSelector " <> r <> ")"
cbRetConv CbClass  r = "(castPtr (unClass " <> r <> ") :: Ptr ObjCObject)"

-- | Default return for an unimplemented optional method.
cbDefaultRet :: CbTypeClass -> Text
cbDefaultRet CbVoid   = "()"
cbDefaultRet CbObject = "nullPtr"
cbDefaultRet CbBool   = "0"
cbDefaultRet CbLong   = "0"
cbDefaultRet CbULong  = "0"
cbDefaultRet CbInt    = "0"
cbDefaultRet CbUInt   = "0"
cbDefaultRet CbDouble = "0.0"
cbDefaultRet CbFloat  = "0.0"
cbDefaultRet CbSel    = "nullPtr"
cbDefaultRet CbClass  = "nullPtr"

-- ---------------------------------------------------------------------------
-- Method analysis
-- ---------------------------------------------------------------------------

-- | A protocol method that has been classified for code generation.
data DelegateMethod = DelegateMethod
  { dmSelector     :: Text
  , dmFieldName    :: Text
  , dmParamClasses :: [CbTypeClass]
  , dmRetClass     :: CbTypeClass
  }

-- | Classify a method for delegate code generation.  Returns 'Nothing'
-- when the method is a class method, implicit, or has unsupported types.
analyzeMethod :: ObjCMethod -> Maybe DelegateMethod
analyzeMethod m
  | methodIsClass m    = Nothing
  | methodIsImplicit m = Nothing
  | otherwise = do
      retCls <- classifyCallback (methodReturnType m)
      paramCls <- traverse (classifyCallback . snd) (methodParams m)
      let sel = methodSelector m
          baseName = T.replace ":" "_" (T.dropWhileEnd (== ':') sel)
          fieldName = "_" <> lowerFirst baseName
      pure DelegateMethod
        { dmSelector     = sel
        , dmFieldName    = fieldName
        , dmParamClasses = paramCls
        , dmRetClass     = retCls
        }

-- | Deduplicate methods by field name, keeping the first occurrence.
dedupByField :: [DelegateMethod] -> [DelegateMethod]
dedupByField = go Set.empty
  where
    go _ [] = []
    go seen (m:ms)
      | Set.member (dmFieldName m) seen = go seen ms
      | otherwise = m : go (Set.insert (dmFieldName m) seen) ms

-- | Full ObjC type encoding: @retEnc \@ : param0Enc param1Enc ...@
fullEncoding :: DelegateMethod -> Text
fullEncoding dm =
  cbEnc (dmRetClass dm) <> "@:" <> T.concat (fmap cbEnc (dmParamClasses dm))

-- | IMP signature fingerprint for deduplicating wrapper declarations.
impFP :: DelegateMethod -> Text
impFP dm =
  T.intercalate "_" (fmap cbFP (dmParamClasses dm) ++ [cbFP (dmRetClass dm)])

-- | Build the IMP C type signature string for a wrapper.
-- Always starts with @Ptr ObjCObject -> Ptr ObjCSel@, then the param
-- types, then @IO retType@.
impTypeStr :: [CbTypeClass] -> CbTypeClass -> Text
impTypeStr params ret =
  T.intercalate " -> "
    (["Ptr ObjCObject", "Ptr ObjCSel"] ++ fmap cbCType params ++ ["IO " <> cbRetInIO ret])

-- ---------------------------------------------------------------------------
-- Top-level entry points
-- ---------------------------------------------------------------------------

-- | Generate delegate modules for all protocols belonging to a framework.
generateDelegateModules :: ClassHierarchy -> Text -> [GeneratedModule]
generateDelegateModules hierarchy framework =
  mapMaybe (generateDelegateModule framework) $
    filter (\p -> protoDeclFramework p == Just framework)
           (Map.elems (hierarchyProtocols hierarchy))

-- | Generate a delegate module for a single protocol.  Returns 'Nothing'
-- if the protocol has no methods that can be classified for callbacks.
generateDelegateModule :: Text -> ObjCProtocolDecl -> Maybe GeneratedModule
generateDelegateModule framework proto
  | null methods = Nothing
  | otherwise = Just GeneratedModule
      { genModuleName    = modName
      , genModuleContent = T.unlines content
      }
  where
    pName     = protoDeclName proto
    modName   = "ObjC." <> framework <> ".Delegate." <> pName
    recName   = pName <> "Overrides"
    defName   = "default" <> recName
    cafName   = lowerFirst pName <> "DelegateClass"
    ctorName  = "new" <> pName
    clsLabel  = "Hs" <> pName

    -- Gather all protocol methods (required + optional + property accessors)
    allMethods = protoDeclRequired proto
              ++ protoDeclOptional proto
              ++ concatMap propertyToMethods (protoDeclReqProps proto)
              ++ concatMap propertyToMethods (protoDeclOptProps proto)

    methods = dedupByField (mapMaybe analyzeMethod allMethods)

    -- Unique IMP signatures for wrapper declarations
    uniqueFPs :: Set Text
    uniqueFPs = Set.fromList (fmap impFP methods)

    content = concat
      [ genModHeader pName modName recName defName ctorName
      , [""]
      , genImports
      , [""]
      , genRecord pName recName methods
      , [""]
      , genDefault defName recName methods
      , [""]
      , genWrappers methods uniqueFPs
      , [""]
      , genRespondsWrapper
      , [""]
      , genCAF cafName clsLabel recName methods
      , [""]
      , genCtor ctorName recName cafName
      ]

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

genModHeader :: Text -> Text -> Text -> Text -> Text -> [Text]
genModHeader pName modName recName defName ctorName =
  [ "{-# OPTIONS_GHC -Wno-unused-imports #-}"
  , "{-# LANGUAGE ForeignFunctionInterface #-}"
  , ""
  , "-- | Generated delegate overrides for @\\@protocol " <> pName <> "@."
  , "--"
  , "-- Usage:"
  , "--"
  , "-- @"
  , "-- delegate <- " <> ctorName <> " " <> defName
  , "--   { ... = Just $ \\\\arg0 -> ..."
  , "--   }"
  , "-- @"
  , "module " <> modName
  , "  ( " <> recName <> "(..)"
  , "  , " <> defName
  , "  , " <> ctorName
  , "  ) where"
  ]

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

genImports :: [Text]
genImports =
  [ "import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)"
  , "import Foreign.C.Types"
  , "import Foreign.StablePtr (newStablePtr, deRefStablePtr)"
  , "import System.IO.Unsafe (unsafePerformIO)"
  , "import Foreign.C.String (withCString)"
  , "import Foreign.LibFFI (retCULong, argPtr)"
  , ""
  , "import ObjC.Runtime.Types"
  , "import ObjC.Runtime.Class (getRequiredClass, class_createInstance)"
  , "import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)"
  , "import ObjC.Runtime.Selector (mkSelector)"
  , "import ObjC.Runtime.MsgSend (sendSuperMsg)"
  , "import ObjC.Runtime.StableIvar"
  ]

-- ---------------------------------------------------------------------------
-- Overrides record
-- ---------------------------------------------------------------------------

genRecord :: Text -> Text -> [DelegateMethod] -> [Text]
genRecord pName recName methods =
  [ "-- | Overrides record for @\\@protocol " <> pName <> "@."
  , "--"
  , "-- Each field corresponds to a protocol method.  'Nothing' means the"
  , "-- method is not implemented (the object will not respond to that"
  , "-- selector).  'Just' provides the Haskell implementation."
  , "data " <> recName <> " = " <> recName
  ] ++ case methods of
         [] -> []
         (m0 : rest) ->
           ("  { " <> fieldDecl m0)
           : fmap (\dm -> "  , " <> fieldDecl dm) rest
           ++ ["  }"]
  where
    fieldDecl :: DelegateMethod -> Text
    fieldDecl dm =
      let paramTys = fmap cbHsType (dmParamClasses dm)
          retTy    = "IO " <> cbHsType (dmRetClass dm)
          inner    = case paramTys of
                       [] -> retTy
                       _  -> T.intercalate " -> " (paramTys ++ [retTy])
      in dmFieldName dm <> " :: !(Maybe (" <> inner <> "))"

-- ---------------------------------------------------------------------------
-- Default overrides
-- ---------------------------------------------------------------------------

genDefault :: Text -> Text -> [DelegateMethod] -> [Text]
genDefault defName recName methods =
  [ "-- | Default overrides with all methods unimplemented."
  , defName <> " :: " <> recName
  , defName <> " = " <> recName
  ] ++ case methods of
         [] -> []
         (m0 : rest) ->
           ("  { " <> dmFieldName m0 <> " = Nothing")
           : fmap (\dm -> "  , " <> dmFieldName dm <> " = Nothing") rest
           ++ ["  }"]

-- ---------------------------------------------------------------------------
-- FFI wrapper declarations
-- ---------------------------------------------------------------------------

genWrappers :: [DelegateMethod] -> Set Text -> [Text]
genWrappers methods _fps =
  -- Generate one wrapper per unique IMP fingerprint.
  -- We collect a representative method for each fingerprint to get
  -- the param/return types.
  let fpMap = foldl (\acc dm ->
                let fp = impFP dm
                in if Set.member fp (Set.fromList (fmap fst acc))
                   then acc
                   else (fp, dm) : acc
              ) [] methods
  in concatMap (\(fp, dm) -> genOneWrapper fp (dmParamClasses dm) (dmRetClass dm)) fpMap

genOneWrapper :: Text -> [CbTypeClass] -> CbTypeClass -> [Text]
genOneWrapper fp params ret =
  let tyStr = impTypeStr params ret
  in [ "foreign import ccall \"wrapper\""
     , "  wrap_" <> fp
     , "    :: (" <> tyStr <> ")"
     , "    -> IO (FunPtr (" <> tyStr <> "))"
     , ""
     ]

-- | The @respondsToSelector:@ wrapper has a fixed signature.
genRespondsWrapper :: [Text]
genRespondsWrapper =
  [ "foreign import ccall \"wrapper\""
  , "  wrap_respondsToSel"
  , "    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)"
  , "    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))"
  ]

-- ---------------------------------------------------------------------------
-- Class CAF
-- ---------------------------------------------------------------------------

genCAF :: Text -> Text -> Text -> [DelegateMethod] -> [Text]
genCAF cafName clsLabel recName methods =
  [ "-- | The ObjC class for this delegate.  Created once, shared by all instances."
  , "{-# NOINLINE " <> cafName <> " #-}"
  , cafName <> " :: Class"
  , cafName <> " = unsafePerformIO $ do"
  , "  superCls <- getRequiredClass \"NSObject\""
  , "  cls <- withCString " <> tshow clsLabel <> " $ \\n ->"
  , "    objc_allocateClassPair superCls n 0"
  , "  addHsDataIvar cls"
  ]
  -- Precompute interned selectors for respondsToSelector:
  ++ genSelectorLets methods
  -- Method stubs
  ++ concatMap (\(i, dm) -> genStub recName i dm) (zip [(0::Int)..] methods)
  -- respondsToSelector: override
  ++ genRespondsStub recName methods
  -- Dealloc + register
  ++ [ "  addStablePtrDeallocHandler cls"
     , "  objc_registerClassPair cls"
     , "  pure cls"
     ]

-- | Generate @let@ bindings for interned selectors used by
-- the @respondsToSelector:@ override.
genSelectorLets :: [DelegateMethod] -> [Text]
genSelectorLets [] = []
genSelectorLets (m0 : rest) =
  ["  let " <> selectorLet m0]
  ++ fmap (\dm -> "      " <> selectorLet dm) rest
  where
    selectorLet dm =
      "sel_" <> selVarName dm <> " = unSelector (mkSelector " <> tshow (dmSelector dm) <> ")"

-- | Short variable name derived from the field name (without leading @_@).
selVarName :: DelegateMethod -> Text
selVarName dm = T.drop 1 (dmFieldName dm)

-- | Generate the IMP stub creation and registration for a single method.
genStub :: Text -> Int -> DelegateMethod -> [Text]
genStub recName idx dm =
  let fp      = impFP dm
      sel     = dmSelector dm
      enc     = fullEncoding dm
      nParams = length (dmParamClasses dm)
      argVars = fmap (\j -> "arg" <> tshowInt j) [0 .. nParams - 1]
      retCls  = dmRetClass dm

      -- Lambda parameter list: self _cmd arg0 arg1 ...
      lamParams = T.unwords (["self", "_cmd"] ++ argVars)

      -- Converted argument expressions
      convArgs = zipWith cbParamConv (dmParamClasses dm) argVars
      callArgs = T.unwords convArgs

      -- Build the Just branch
      justBranch
        | retCls == CbVoid =
            ["      Just f -> f " <> callArgs]
        | otherwise =
            [ "      Just f -> do"
            , "        r <- f " <> callArgs
            , "        pure " <> cbRetConv retCls "r"
            ]

  in [ "  -- " <> sel
     , "  stub_" <> tshowInt idx <> " <- wrap_" <> fp <> " $ \\" <> lamParams <> " -> do"
     , "    sp <- readHsData self"
     , "    rec_ <- deRefStablePtr sp :: IO " <> recName
     , "    case " <> dmFieldName dm <> " rec_ of"
     , "      Nothing -> pure " <> cbDefaultRet retCls
     ] ++ justBranch
     ++ [ "  addObjCMethod cls " <> tshow sel <> " " <> tshow enc
            <> " stub_" <> tshowInt idx
        , ""
        ]

-- | Generate the @respondsToSelector:@ override stub.
genRespondsStub :: Text -> [DelegateMethod] -> [Text]
genRespondsStub recName methods =
  [ "  -- respondsToSelector: override"
  , "  rtsStub <- wrap_respondsToSel $ \\self _cmd queriedSel -> do"
  , "    sp <- readHsData self"
  , "    rec_ <- deRefStablePtr sp :: IO " <> recName
  ] ++ genIfChain methods
  ++ [ "  addObjCMethod cls \"respondsToSelector:\" \"B@::\" rtsStub"
     , ""
     ]

-- | Generate the if-else chain for respondsToSelector:.
-- Each method gets: @if queriedSel == sel_X then pure (maybe 0 (const 1) (fieldName rec_))@
-- The final else falls through to @[super respondsToSelector:]@.
genIfChain :: [DelegateMethod] -> [Text]
genIfChain [] =
  [ "    do"
  , "      let super_ = ObjCSuper (RawId self) superCls"
  , "      sendSuperMsg super_ (mkSelector \"respondsToSelector:\") retCULong"
  , "        [argPtr (castPtr queriedSel :: Ptr ())]"
  ]
genIfChain (dm : rest) =
  [ "    if queriedSel == sel_" <> selVarName dm
      <> " then pure (maybe 0 (const 1) (" <> dmFieldName dm <> " rec_))"
  ] ++ case rest of
         [] -> genElseSuper
         _  -> fmap prependElse (genIfChain rest)
  where
    prependElse line
      | "    if " `T.isPrefixOf` line = "    else " <> T.stripStart line
      | otherwise                     = line

    genElseSuper =
      [ "    else do"
      , "      let super_ = ObjCSuper (RawId self) superCls"
      , "      sendSuperMsg super_ (mkSelector \"respondsToSelector:\") retCULong"
      , "        [argPtr (castPtr queriedSel :: Ptr ())]"
      ]

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

genCtor :: Text -> Text -> Text -> [Text]
genCtor ctorName recName cafName =
  [ "-- | Create a new delegate implementing this protocol."
  , "--"
  , "-- The returned 'RawId' can be used as a delegate or data source."
  , ctorName <> " :: " <> recName <> " -> IO RawId"
  , ctorName <> " overrides = do"
  , "  inst <- class_createInstance " <> cafName <> " 0"
  , "  sp <- newStablePtr overrides"
  , "  writeHsData inst sp"
  , "  pure inst"
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Show a text value as a Haskell string literal.
tshow :: Text -> Text
tshow t = T.pack (show (T.unpack t))

-- | Show an integer as text.
tshowInt :: Int -> Text
tshowInt = T.pack . show
