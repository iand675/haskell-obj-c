{-# LANGUAGE OverloadedStrings #-}

-- | Struct module generation.
--
-- Generates @Internal.Structs@ modules containing, for each struct:
--
-- * A Haskell data type with strict fields
-- * A @Storable@ instance with computed offsets
-- * A cached libffi @CType@ descriptor
-- * @argXxx@ \/ @retXxx@ convenience functions
module ObjC.CodeGen.Generate.Structs
  ( generateStructsModule
  , generatePublicStructsModule
  , generateStructReExportModule
  , frameworkStructs
    -- * Struct layout (used by Package for dependency ordering)
  , sortStructsByDeps
    -- * Opaque struct resolution
  , resolveOpaqueStructFields
    -- * C-name → typedef resolution
  , buildCNameToTypedef
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap (hsTypeToText, HsType(..))
import ObjC.CodeGen.PrimitiveTypes (lookupPrimitive, PrimitiveTypeInfo(..))
import ObjC.CodeGen.Generate.Types (GeneratedModule(..))
import ObjC.CodeGen.Generate.Naming (lowerFirst, upperFirst, formatHaddock)
import ObjC.CodeGen.Generate.Shared (isStructSupported, isFieldTypeSupported)
import ObjC.CodeGen.Generate.Enums (enumFFIType, enumSizeAlign)

-- ---------------------------------------------------------------------------
-- Framework struct collection
-- ---------------------------------------------------------------------------

-- | Get all structs belonging to a framework, in dependency order.
frameworkStructs :: ClassHierarchy -> Text -> [StructDef]
frameworkStructs hierarchy fw =
  let allStructs = hierarchyStructs hierarchy
      fwOnly = filter (\sd -> structFramework sd == Just fw)
                      (Map.elems allStructs)
  in sortStructsByDeps allStructs fwOnly

-- | Sort structs so that simpler ones come before compound ones.
sortStructsByDeps :: Map Text StructDef -> [StructDef] -> [StructDef]
sortStructsByDeps allStructs = sortBy (comparing depthOf)
  where
    depthMap :: Map Text Int
    depthMap = Map.mapWithKey (\name _ -> structDepth Set.empty name) allStructs

    depthOf :: StructDef -> Int
    depthOf sd = fromMaybe 0 (Map.lookup (structTypedefName sd) depthMap)

    structDepth :: Set Text -> Text -> Int
    structDepth visited name
      | Set.member name visited = 0
      | otherwise = case Map.lookup name allStructs of
          Nothing -> 0
          Just sd ->
            let visited' = Set.insert name visited
                fieldDeps = mapMaybe fieldStructRef (structFields sd)
                childDepths = fmap (structDepth visited') fieldDeps
            in if null childDepths then 0
               else 1 + maximum childDepths

    fieldStructRef :: (Text, ObjCType) -> Maybe Text
    fieldStructRef (_, ObjCStruct name) = Just name
    fieldStructRef _ = Nothing

-- ---------------------------------------------------------------------------
-- Opaque struct resolution
-- ---------------------------------------------------------------------------

-- | Replace struct field references to non-generatable struct types
-- with opaque pointers.
resolveOpaqueStructFields :: ClassHierarchy -> ClassHierarchy
resolveOpaqueStructFields h =
  h { hierarchyStructs = Map.map (resolve generatable) (hierarchyStructs h) }
  where
    allStructs = hierarchyStructs h
    classNames_ = Map.keysSet (hierarchyClasses h)

    generatable = fixpoint (Map.keysSet allStructs)

    fixpoint current =
      let next = Set.filter (isGeneratable current) current
      in if next == current then current else fixpoint next

    isGeneratable genSet sn = case Map.lookup sn allStructs of
      Nothing -> False
      Just sd -> all (fieldOk genSet) (structFields sd)

    fieldOk genSet (_, ObjCStruct ref)        = Set.member ref genSet
    fieldOk _      (_, ObjCQualified _ inner)  = fieldOk' inner
    fieldOk _      (_, ty)                     = isFieldTypeSupported classNames_ ty

    fieldOk' (ObjCStruct _) = True
    fieldOk' ty             = isFieldTypeSupported classNames_ ty

    resolve genSet sd = sd
      { structFields = fmap (resolveField genSet) (structFields sd) }
    resolveField genSet (name, ObjCStruct sn)
      | not (Set.member sn genSet) = (name, ObjCPointer ObjCVoid)
    resolveField _ f = f

-- ---------------------------------------------------------------------------
-- C-name → typedef resolution
-- ---------------------------------------------------------------------------

-- | Build a map from C struct names to typedef names.
buildCNameToTypedef :: ClassHierarchy -> Text -> Map Text Text
buildCNameToTypedef hierarchy framework =
  let allStructs = Map.elems (hierarchyStructs hierarchy)
      pairs = fmap (\sd -> (structCName sd, (structTypedefName sd, structFramework sd))) allStructs
      grouped = foldl (\m (cn, info) -> Map.insertWith (++) cn [info] m) Map.empty pairs
  in Map.map (pickBest framework) grouped
  where
    pickBest :: Text -> [(Text, Maybe Text)] -> Text
    pickBest fw candidates =
      case filter (\(_, mfw) -> mfw == Just fw) candidates of
        ((name, _):_) -> name
        [] -> case candidates of
          ((name, _):_) -> name
          []            -> ""  -- should never happen: grouped map values are non-empty

-- ---------------------------------------------------------------------------
-- Module generation
-- ---------------------------------------------------------------------------

-- | Generate the @Internal.Structs@ module for a framework.
generateStructsModule :: ClassHierarchy -> Text -> [StructDef] -> GeneratedModule
generateStructsModule hierarchy framework structs =
  let modName = "ObjC." <> framework <> ".Internal.Structs"
      cNameMap = buildCNameToTypedef hierarchy framework
      resolvedStructs = filter (isStructSupported allKnown)
                               (fmap (resolveStructFields cNameMap) structs)
      allKnown = Map.keysSet (hierarchyClasses hierarchy)
  in GeneratedModule
    { genModuleName = modName
    , genModuleContent = T.unlines $ concat
        [ structsModuleHeader modName
        , [""]
        , structsModuleImports hierarchy framework resolvedStructs
        , concatMap (\sd -> [""] ++ generateStructDecls hierarchy sd) resolvedStructs
        ]
    }
  where
    resolveStructFields :: Map Text Text -> StructDef -> StructDef
    resolveStructFields cMap sd = sd
      { structFields = fmap (\(n, ty) -> (n, resolveFieldType cMap ty)) (structFields sd)
      }

-- | Generate the public @ObjC.Framework.Structs@ re-export module.
generatePublicStructsModule :: Text -> [StructDef] -> GeneratedModule
generatePublicStructsModule prefix _structs = GeneratedModule
  { genModuleName = publicMod
  , genModuleContent = T.unlines
      [ "-- | Public re-export of struct types for the framework."
      , "--"
      , "-- Generated by objc-codegen. Do not edit."
      , "module " <> publicMod
      , "  ( module " <> internalMod
      , "  ) where"
      , ""
      , "import " <> internalMod
      ]
  }
  where
    publicMod   = prefix <> ".Structs"
    internalMod = prefix <> ".Internal.Structs"

-- | Generate an umbrella re-export module for a struct-only framework.
generateStructReExportModule :: Text -> [StructDef] -> GeneratedModule
generateStructReExportModule prefix structs
  | null structs = GeneratedModule
      { genModuleName    = prefix
      , genModuleContent = T.unlines
          [ "-- | (No public exports for this framework.)"
          , "module " <> prefix <> " where"
          ]
      }
  | otherwise = GeneratedModule
      { genModuleName    = prefix
      , genModuleContent = T.unlines
          [ "-- | Re-exports struct types for the framework."
          , "module " <> prefix
          , "  ( module " <> structsMod
          , "  ) where"
          , ""
          , "import " <> structsMod
          ]
      }
  where
    structsMod = prefix <> ".Internal.Structs"

-- ---------------------------------------------------------------------------
-- Struct module header and imports
-- ---------------------------------------------------------------------------

structsModuleHeader :: Text -> [Text]
structsModuleHeader modName =
  [ "{-# LANGUAGE RecordWildCards #-}"
  , "{-# LANGUAGE TypeFamilies #-}"
  , ""
  , "-- | Struct types for this framework."
  , "--"
  , "-- Generated by objc-codegen. Do not edit."
  , "module " <> modName <> " where"
  ]

structsModuleImports :: ClassHierarchy -> Text -> [StructDef] -> [Text]
structsModuleImports hierarchy framework structs =
  [ "import Foreign.Ptr (Ptr, FunPtr)"
  , "import Foreign.Storable (Storable(..))"
  , "import Foreign.C.Types"
  , "import Foreign.LibFFI.Base (Arg, RetType, mkStorableArg, mkStorableRetType, newStructCType)"
  , "import Foreign.LibFFI.FFITypes"
  , "import Foreign.LibFFI.Internal (CType)"
  , "import System.IO.Unsafe (unsafePerformIO)"
  , "import ObjC.Runtime.Message (ObjCArgument(..), ObjCReturn(..), MsgSendVariant(..))"
  ] ++ crossFwStructImports ++ enumImports
  where
    enumMap = hierarchyEnums hierarchy
    crossFwStructImports =
      let refs = Set.fromList
            [ refName
            | sd <- structs
            , (_, ObjCStruct refName) <- structFields sd
            , not (any (\s -> structTypedefName s == refName) structs)
            ]
          fws = Set.fromList
            [ fw
            | refName <- Set.toList refs
            , Just refSd <- [Map.lookup refName (hierarchyStructs hierarchy)]
            , Just fw <- [structFramework refSd]
            , fw /= framework
            ]
      in fmap (\fw -> "import ObjC." <> fw <> ".Internal.Structs") (Set.toList fws)
    enumImports =
      let enumFieldNames = Set.fromList
            [ fieldTyName
            | sd <- structs
            , (_, fieldTy) <- structFields sd
            , fieldTyName <- enumFieldTypeName enumMap fieldTy
            ]
          localEnumRefs = Set.filter (\n -> case Map.lookup n enumMap of
            Just ed -> enumFramework ed == Just framework
            Nothing -> False) enumFieldNames
          crossEnumFws = Set.fromList
            [ fw
            | eName <- Set.toList enumFieldNames
            , Just ed <- [Map.lookup eName enumMap]
            , Just fw <- [enumFramework ed]
            , fw /= framework
            ]
          localImport
            | Set.null localEnumRefs = []
            | otherwise = ["import ObjC." <> framework <> ".Internal.Enums"]
          crossImports = fmap
            (\fw -> "import ObjC." <> fw <> ".Internal.Enums")
            (Set.toList crossEnumFws)
      in localImport ++ crossImports

    enumFieldTypeName :: Map Text EnumDef -> ObjCType -> [Text]
    enumFieldTypeName enums (ObjCPrimitive q d)
      | Just ed <- lookupEnumByQualType' enums q d = [enumName ed]
    enumFieldTypeName _ _ = []

    lookupEnumByQualType' :: Map Text EnumDef -> Text -> Text -> Maybe EnumDef
    lookupEnumByQualType' enums qualType desugared
      | "enum " `T.isPrefixOf` desugared
      , let eName = T.strip (T.drop 5 desugared)
      , Just ed <- Map.lookup eName enums = Just ed
      | Just ed <- Map.lookup qualType enums = Just ed
      | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Per-struct generation
-- ---------------------------------------------------------------------------

generateStructDecls :: ClassHierarchy -> StructDef -> [Text]
generateStructDecls hierarchy sd = concat
  [ generateStructDataType (hierarchyEnums hierarchy) sd
  , [""]
  , generateStorableInstance hierarchy sd
  , [""]
  , generateStructCType hierarchy sd
  , [""]
  , generateStructArgRet sd
  ]

-- | Generate the data type declaration.
generateStructDataType :: Map Text EnumDef -> StructDef -> [Text]
generateStructDataType enums sd =
  let name = structTypedefName sd
      fields = structFields sd
      prefix = lowerFirst name
      mkField (i, (fName, fTy)) =
        let sep = if i == (0 :: Int) then "  { " else "  , "
            hsName = prefix <> upperFirst fName
            hsTy = fieldTypeToHs enums fTy
        in sep <> hsName <> " :: !" <> wrapCompound hsTy
      wrapCompound t
        | T.any (== ' ') t = "(" <> t <> ")"
        | otherwise        = t
      docComment = case structDoc sd of
        Just doc -> formatHaddock doc
        Nothing  -> []
  in docComment
     ++ [ "data " <> name <> " = " <> name ]
     ++ zipWith (curry mkField) [0..] fields
     ++ [ "  } deriving (Eq, Show)" ]

-- | Generate the @Storable@ instance.
generateStorableInstance :: ClassHierarchy -> StructDef -> [Text]
generateStorableInstance hierarchy sd =
  let name = structTypedefName sd
      layout = computeStructLayout hierarchy (structFields sd)
      totalSize = slTotalSize layout
      alignment_ = slAlignment layout
      offsets = slOffsets layout
      prefix = lowerFirst name
      fields = structFields sd
      fieldNames = fmap (\(fName, _) -> prefix <> upperFirst fName) fields
      peekLines = case (fieldNames, offsets) of
        ([], _) -> ["  peek _ = pure " <> name]
        (_:rest, o0:restOff) ->
          ["  peek p = " <> name <> " <$> peekByteOff p " <> T.pack (show o0)]
          ++ zipWith (\_ off ->
               "    <*> peekByteOff p " <> T.pack (show off))
             rest restOff
        _ -> ["  peek _ = pure " <> name]
      pokeFields = zipWith (\fn off ->
        "    pokeByteOff p " <> T.pack (show off) <> " " <> fn)
        fieldNames offsets
      pokeBody = case pokeFields of
        []  -> ["  poke _ _ = pure ()"]
        [x] -> ["  poke p (" <> name <> " {..}) =", x]
        _   -> ["  poke p (" <> name <> " {..}) = do"]
              ++ pokeFields
  in [ "instance Storable " <> name <> " where"
     , "  sizeOf    _ = " <> T.pack (show totalSize)
     , "  alignment _ = " <> T.pack (show alignment_)
     ] ++ peekLines ++ pokeBody

-- | Generate the cached @CType@ descriptor.
generateStructCType :: ClassHierarchy -> StructDef -> [Text]
generateStructCType hierarchy sd =
  let name = structTypedefName sd
      lower = lowerFirst name
      ffiTypes = fmap (fieldTypeToFFIType hierarchy . snd) (structFields sd)
  in [ "{-# NOINLINE " <> lower <> "StructType #-}"
     , lower <> "StructType :: Ptr CType"
     , lower <> "StructType = unsafePerformIO $ fst <$> newStructCType "
       <> "[" <> T.intercalate ", " ffiTypes <> "]"
     ]

-- | Generate arg\/ret convenience functions and @ObjCArgument@\/@ObjCReturn@ instances.
generateStructArgRet :: StructDef -> [Text]
generateStructArgRet sd =
  let name = structTypedefName sd
      lower = lowerFirst name
  in [ "arg" <> name <> " :: " <> name <> " -> Arg"
     , "arg" <> name <> " = mkStorableArg " <> lower <> "StructType"
     , ""
     , "ret" <> name <> " :: RetType " <> name
     , "ret" <> name <> " = mkStorableRetType " <> lower <> "StructType"
     , ""
     , "instance ObjCArgument " <> name <> " where"
     , "  withObjCArg x k = k (arg" <> name <> " x)"
     , ""
     , "instance ObjCReturn " <> name <> " where"
     , "  type RawReturn " <> name <> " = " <> name
     , "  objcRetType = ret" <> name
     , "  msgSendVariant = MsgSendStret"
     , "  fromRetained = pure"
     , "  fromOwned = pure"
     ]

-- ---------------------------------------------------------------------------
-- Struct layout computation
-- ---------------------------------------------------------------------------

data StructLayout = StructLayout
  { slOffsets   :: [Int]
  , slTotalSize :: Int
  , slAlignment :: Int
  } deriving (Show)

computeStructLayout :: ClassHierarchy -> [(Text, ObjCType)] -> StructLayout
computeStructLayout hierarchy = computeStructLayout' hierarchy Set.empty

computeStructLayout' :: ClassHierarchy -> Set Text -> [(Text, ObjCType)] -> StructLayout
computeStructLayout' hierarchy visited fields =
  let fieldSAs = fmap (fieldSizeAlign hierarchy visited . snd) fields
      (revOffsets, cursor) = foldl addField ([], 0) fieldSAs
      offsets = reverse revOffsets
      maxAlign = if null fieldSAs then 1 else maximum (fmap snd fieldSAs)
      totalSize = alignUp cursor maxAlign
  in StructLayout
    { slOffsets   = offsets
    , slTotalSize = totalSize
    , slAlignment = maxAlign
    }
  where
    addField :: ([Int], Int) -> (Int, Int) -> ([Int], Int)
    addField (offAcc, curOff) (sz, al) =
      let aligned = alignUp curOff al
      in (aligned : offAcc, aligned + sz)

    alignUp :: Int -> Int -> Int
    alignUp off al = ((off + al - 1) `div` al) * al

fieldSizeAlign :: ClassHierarchy -> Set Text -> ObjCType -> (Int, Int)
fieldSizeAlign hierarchy visited (ObjCStruct name)
  | Set.member name visited = (0, 1)  -- cycle guard (shouldn't happen in valid C)
  | otherwise =
      case Map.lookup name (hierarchyStructs hierarchy) of
        Just sd ->
          let visited' = Set.insert name visited
              layout = computeStructLayout' hierarchy visited' (structFields sd)
          in (slTotalSize layout, slAlignment layout)
        Nothing -> (0, 1)
fieldSizeAlign _ _ (ObjCPointer _) = (8, 8)
fieldSizeAlign h v (ObjCQualified _ inner) = fieldSizeAlign h v inner
fieldSizeAlign h _ (ObjCPrimitive q d) = primitiveSizeAlign (hierarchyEnums h) q d
fieldSizeAlign _ _ (ObjCId _ _) = (8, 8)
fieldSizeAlign _ _ (ObjCGeneric _ _ _) = (8, 8)
fieldSizeAlign _ _ ObjCInstancetype = (8, 8)
fieldSizeAlign _ _ (ObjCClassType _) = (8, 8)
fieldSizeAlign _ _ ObjCSEL = (8, 8)
fieldSizeAlign _ _ (ObjCBlock _ _) = (8, 8)
fieldSizeAlign _ _ ObjCBool = (1, 1)
fieldSizeAlign _ _ ObjCVoid = (0, 1)

-- | Get (size, alignment) for a C primitive type.
primitiveSizeAlign :: Map Text EnumDef -> Text -> Text -> (Int, Int)
primitiveSizeAlign enums q d
  -- Known enum types
  | "enum " `T.isPrefixOf` d
  , Just eName <- T.stripPrefix "enum " d
  , Just ed <- Map.lookup (T.strip eName) enums
    = enumSizeAlign ed
  | Just ed <- Map.lookup q enums
    = enumSizeAlign ed
  -- Delegate to PrimitiveTypes table
  | Just info <- lookupPrimitive q d
    = (ptSize info, ptAlignment info)
  -- Pointer types mis-parsed as primitives
  | "*" `T.isSuffixOf` d    = (8, 8)
  | "(*)" `T.isInfixOf` d   = (8, 8)
  | "enum " `T.isPrefixOf` d = (4, 4)
  | "[" `T.isInfixOf` d     = (8, 8)
  | "union " `T.isPrefixOf` d = (8, 8)
  | otherwise                = (4, 4)  -- conservative default

-- ---------------------------------------------------------------------------
-- Field type helpers
-- ---------------------------------------------------------------------------

-- | Resolve a struct field type, replacing C struct names with typedef names.
resolveFieldType :: Map Text Text -> ObjCType -> ObjCType
resolveFieldType cNameMap (ObjCStruct name) =
  case Map.lookup name cNameMap of
    Just tdName -> ObjCStruct tdName
    Nothing     -> ObjCStruct name
resolveFieldType _ ty = ty

-- | Map an ObjCType field to a Haskell type name for struct fields.
fieldTypeToHs :: Map Text EnumDef -> ObjCType -> Text
fieldTypeToHs enums (ObjCPrimitive q d) = hsTypeToText (mapPrimFieldType enums q d)
  where
    mapPrimFieldType :: Map Text EnumDef -> Text -> Text -> HsType
    mapPrimFieldType emap qt dt
      | "enum " `T.isPrefixOf` dt
      , Just eName <- T.stripPrefix "enum " dt
      , Just ed <- Map.lookup (T.strip eName) emap
        = HsTyCon (enumName ed)
      | Just ed <- Map.lookup qt emap
        = HsTyCon (enumName ed)
      | Just info <- lookupPrimitive qt dt
        = HsTyCon (ptHsType info)
      | "*" `T.isSuffixOf` dt       = HsTyPtr HsTyUnit
      | "(*)" `T.isInfixOf` dt      = HsTyPtr HsTyUnit
      | "enum " `T.isPrefixOf` dt   = HsTyCon "CInt"
      | "[" `T.isInfixOf` dt        = HsTyPtr HsTyUnit
      | "union " `T.isPrefixOf` dt  = HsTyPtr HsTyUnit
      | otherwise = HsTyCon "CInt"  -- conservative fallback
fieldTypeToHs _ (ObjCStruct name) = name
fieldTypeToHs _ (ObjCPointer _) = "Ptr ()"
fieldTypeToHs enums (ObjCQualified _ inner) = fieldTypeToHs enums inner
fieldTypeToHs _ _ = "Ptr ()"  -- conservative fallback for unexpected types

-- | Map an ObjCType field to its libffi @ffi_type_*@ name.
fieldTypeToFFIType :: ClassHierarchy -> ObjCType -> Text
fieldTypeToFFIType hierarchy (ObjCPrimitive q d)
  | "enum " `T.isPrefixOf` d
  , Just eName <- T.stripPrefix "enum " d
  , Just ed <- Map.lookup (T.strip eName) (hierarchyEnums hierarchy)
    = enumFFIType ed
  | Just ed <- Map.lookup q (hierarchyEnums hierarchy)
    = enumFFIType ed
  | Just info <- lookupPrimitive q d
    = ptFFIType info
  | "*" `T.isSuffixOf` d           = "ffi_type_pointer"
  | "(*)" `T.isInfixOf` d          = "ffi_type_pointer"
  | "enum " `T.isPrefixOf` d       = "ffi_type_sint"
  | "[" `T.isInfixOf` d            = "ffi_type_pointer"
  | "union " `T.isPrefixOf` d      = "ffi_type_pointer"
  | otherwise                       = "ffi_type_sint"  -- conservative fallback
fieldTypeToFFIType _ (ObjCPointer _) = "ffi_type_pointer"
fieldTypeToFFIType h (ObjCQualified _ inner) = fieldTypeToFFIType h inner
fieldTypeToFFIType _ (ObjCStruct name) = lowerFirst name <> "StructType"
fieldTypeToFFIType _ _ = "ffi_type_pointer"  -- conservative fallback
