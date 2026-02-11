{-# LANGUAGE OverloadedStrings #-}

-- | Per-class module generation.
--
-- Generates one Haskell module per ObjC class, containing:
--
-- * Module header with exports
-- * Imports (standard + cross-framework)
-- * Method bindings (delegated to 'Generate.Method')
-- * Selector constants
-- * Extension modules for foreign categories
module ObjC.CodeGen.Generate.ClassModule
  ( generateClassModule
  , generateExtensionModules
  , generateReExportModule
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap
import ObjC.CodeGen.Generate.Types (GeneratedModule(..))
import ObjC.CodeGen.Generate.Naming
import ObjC.CodeGen.Generate.Shared
import ObjC.CodeGen.Generate.Method (generateMethods, generateSelectors)
-- Note: enumConstantHsName is defined locally to avoid circular imports

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Generate a single class module.
generateClassModule
  :: Map Text Text -> ClassHierarchy -> KnownTypes -> Text -> Set Text -> Text
  -> GeneratedModule
generateClassModule fwMap hierarchy allKnown framework depFws name =
  case Map.lookup name (hierarchyClasses hierarchy) of
    Nothing -> GeneratedModule
      { genModuleName = "ObjC.Unknown." <> name
      , genModuleContent = "-- Class " <> name <> " not found in hierarchy\n"
      }
    Just cls ->
      let fw = fromMaybe "Unknown" (Map.lookup name fwMap)
          modName = "ObjC." <> fw <> "." <> name
      in GeneratedModule
        { genModuleName    = modName
        , genModuleContent = T.unlines
            (generateContent fwMap allKnown hierarchy cls modName framework depFws framework)
        }

-- | Generate extension modules for foreign-category methods.
generateExtensionModules
  :: Map Text Text -> ClassHierarchy -> KnownTypes -> Text -> Set Text
  -> [GeneratedModule]
generateExtensionModules fwMap hierarchy allKnown framework depFws =
  let foreignClasses =
        [ (name, cls)
        | (name, cls) <- Map.toList (hierarchyClasses hierarchy)
        , classFramework cls /= Just framework
        , any (\m -> methodOriginFramework m == Just framework) (classMethods cls)
           || any (\p -> propOriginFramework p == Just framework) (classProperties cls)
        ]
  in fmap (mkExtMod fwMap hierarchy allKnown framework depFws) foreignClasses

-- | Generate the umbrella re-export module for a framework.
generateReExportModule :: Text -> [GeneratedModule] -> GeneratedModule
generateReExportModule prefix modules = GeneratedModule
  { genModuleName = prefix
  , genModuleContent = T.unlines $
      [ "-- | Re-exports all class modules for the framework."
      , "module " <> prefix
      , "  ( " <> T.intercalate "\n  , " reExportList
      , "  ) where"
      , ""
      ] ++ fmap (\m -> "import " <> genModuleName m) modules
  }
  where
    reExportList = fmap (\m -> "module " <> genModuleName m) modules

-- ---------------------------------------------------------------------------
-- Internal: extension module helper
-- ---------------------------------------------------------------------------

mkExtMod
  :: Map Text Text -> ClassHierarchy -> KnownTypes -> Text -> Set Text
  -> (Text, ObjCClass) -> GeneratedModule
mkExtMod fwMap hierarchy allKnown framework depFws (name, cls) =
  let modName = "ObjC." <> framework <> "." <> name
  in GeneratedModule
    { genModuleName    = modName
    , genModuleContent = T.unlines
        (generateContent fwMap allKnown hierarchy cls modName framework depFws framework)
    }

-- ---------------------------------------------------------------------------
-- Content generation (shared by class + extension modules)
-- ---------------------------------------------------------------------------

generateContent
  :: Map Text Text -> KnownTypes -> ClassHierarchy
  -> ObjCClass -> Text -> Text -> Set Text -> Text -> [Text]
generateContent fwMap allKnown hierarchy cls modName framework depFws originFw =
  let importableClasses = Set.fromList
        [ c | (c, fw) <- Map.toList fwMap
        , fw == framework || fwToPackageName fw `Set.member` depFws
        ]
      importableStructs = Set.fromList
        [ structTypedefName sd
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , Just sFw <- [structFramework sd]
        , sFw == framework || fwToPackageName sFw `Set.member` depFws
        ]
      importable = Set.union importableClasses importableStructs
      importableEnums = Map.filter
        (\ed -> case enumFramework ed of
          Just eFw -> eFw == framework || fwToPackageName eFw `Set.member` depFws
          Nothing  -> False)
        (ktEnums allKnown)
      restrictedKnown = KnownTypes
        { ktClasses = Set.intersection (ktClasses allKnown) importable
        , ktStructs = Set.intersection (ktStructs allKnown) importable
        , ktEnums   = importableEnums
        , ktAvailableFrameworks = ktAvailableFrameworks allKnown
        }
      originFilter m = methodOriginFramework m == Just originFw
      allKnownClasses = ktClasses allKnown
      allMethods = filter originFilter (allClassMethods importable allKnownClasses cls)
      allMethodTypes = concatMap
        (\m -> methodReturnType m : fmap snd (methodParams m)) allMethods
      -- NSString special handling
      isNSString = className cls == "NSString"
      nsStringPragmas
        | isNSString = ["{-# LANGUAGE FlexibleInstances #-}"]
        | otherwise = []
      nsStringImports
        | isNSString =
            [ "import Data.String (IsString(..))"
            , "import ObjC.Runtime.NSString (pureNSString)"
            ]
        | otherwise = []
      nsStringInstance
        | isNSString =
            [ ""
            , "-- | Allows using @OverloadedStrings@ for @Id NSString@."
            , "--"
            , "-- >>> :set -XOverloadedStrings"
            , "-- >>> let s = \"hello\" :: Id NSString"
            , "instance IsString (Id NSString) where"
            , "  fromString = pureNSString"
            ]
        | otherwise = []
  in concat
    [ nsStringPragmas
    , generateHeader modName cls restrictedKnown importable allKnownClasses originFilter allMethodTypes framework depFws
    , [""]
    , generateImports fwMap restrictedKnown hierarchy cls framework importable allKnownClasses depFws originFilter allMethodTypes
    , nsStringImports
    , [""]
    , generateMethods restrictedKnown hierarchy cls importable allKnownClasses originFilter
    , nsStringInstance
    , ["-- ---------------------------------------------------------------------------"]
    , ["-- Selectors"]
    , ["-- ---------------------------------------------------------------------------"]
    , [""]
    , generateSelectors restrictedKnown cls importable allKnownClasses originFilter
    ]

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

generateHeader
  :: Text -> ObjCClass -> KnownTypes -> Set Text -> Set Text
  -> (ObjCMethod -> Bool) -> [ObjCType]
  -> Text -> Set Text -> [Text]
generateHeader modName cls known importable allKnownClasses originFilter allMethodTypes framework depFws =
  let enumMap = ktEnums known
      enumRefs = referencedEnumNames enumMap allMethodTypes
      availableEnumRefs = Set.filter (isEnumAvailable enumMap framework depFws) enumRefs
      hasEnums = not (Set.null availableEnumRefs)
      patSynPragma
        | hasEnums  = ["{-# LANGUAGE PatternSynonyms #-}"]
        | otherwise = []
  in patSynPragma ++
  [ "{-# LANGUAGE TypeApplications #-}"
  , "{-# LANGUAGE ScopedTypeVariables #-}"
  , "{-# LANGUAGE FlexibleContexts #-}"
  , ""
  ] ++ moduleDoc ++
  [ "module " <> modName
  , "  ( " <> className cls
  , "  , Is" <> className cls <> "(..)"
  , exportMethods known importable allKnownClasses cls originFilter
  , exportEnums enumMap availableEnumRefs
  , "  ) where"
  ]
  where
    moduleDoc = case classDoc cls of
      Just doc ->
        formatHaddock doc
        ++ ["--", "-- Generated bindings for @" <> className cls <> "@."]
      Nothing ->
        ["-- | Generated bindings for @" <> className cls <> "@."]

isEnumAvailable :: Map Text EnumDef -> Text -> Set Text -> Text -> Bool
isEnumAvailable enumMap framework depFws eName =
  case Map.lookup eName enumMap of
    Nothing -> False
    Just ed -> case enumFramework ed of
      Nothing -> False
      Just fw -> fw == framework || fwToPackageName fw `Set.member` depFws

exportMethods :: KnownTypes -> Set Text -> Set Text -> ObjCClass -> (ObjCMethod -> Bool) -> Text
exportMethods known importable allKnownClasses cls originFilter =
  let methods = dedupByHsName cls $
        filter (\m -> not (methodIsImplicit m) && originFilter m
                              && isMethodSupported known m)
                       (allClassMethods importable allKnownClasses cls)
      instNames = instanceMethodNameSet cls methods
      methodExports = fmap (\m -> "  , " <> methodHaskellName cls instNames m) methods
      uniqueSels = dedup Set.empty Set.empty (fmap methodSelector methods)
      dedup _ _ [] = []
      dedup seenSel seenHs (s:ss)
        | Set.member s seenSel = dedup seenSel seenHs ss
        | Set.member hs seenHs = dedup seenSel seenHs ss
        | otherwise            = s : dedup (Set.insert s seenSel) (Set.insert hs seenHs) ss
        where hs = selectorHaskellName s
      selExports = fmap (\s -> "  , " <> selectorHaskellName s) uniqueSels
  in T.unlines (methodExports ++ selExports)

exportEnums :: Map Text EnumDef -> Set Text -> Text
exportEnums enumMap refs
  | Set.null refs = ""
  | otherwise = T.unlines $
      [ "  -- * Enum types" ] ++
      concatMap (enumExportLines enumMap) (Set.toList refs)

enumExportLines :: Map Text EnumDef -> Text -> [Text]
enumExportLines enumMap eName = case Map.lookup eName enumMap of
  Nothing -> []
  Just ed ->
    let tyName = enumName ed
        patNames = fmap (enumConstantHsName . ecName) (enumConstants ed)
    in [ "  , " <> tyName <> "(" <> tyName <> ")" ]
       ++ fmap (\p -> "  , pattern " <> p) patNames

-- Reuse the function from Enums module but locally for re-export
-- (This avoids a circular import since Enums doesn't depend on ClassModule)
enumConstantHsName :: Text -> Text
enumConstantHsName cName = case T.uncons cName of
  Just (c, rest) | c >= 'a' && c <= 'z' -> T.cons (toEnum (fromEnum c - 32)) rest
  _ -> cName

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

generateImports
  :: Map Text Text -> KnownTypes -> ClassHierarchy -> ObjCClass -> Text
  -> Set Text -> Set Text -> Set Text -> (ObjCMethod -> Bool) -> [ObjCType] -> [Text]
generateImports fwMap allKnown hierarchy cls framework importable allKnownClasses depFws originFilter allMethodTypes =
  standardImports ++ [""] ++ internalImport ++ structImports ++ enumImports ++ crossFwImportLines
  where
    usesStret = any (isStructReturn . methodReturnType)
                    (filter originFilter (allClassMethods importable allKnownClasses cls))
    msgSendImports
      | usesStret = "import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)"
      | otherwise = "import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)"
    standardImports =
      [ "import Foreign.Ptr (Ptr, nullPtr, castPtr)"
      , "import Foreign.LibFFI"
      , "import Foreign.C.Types"
      , "import Data.Int (Int8, Int16)"
      , "import Data.Word (Word16)"
      , "import Data.Coerce (coerce)"
      , ""
      , "import ObjC.Runtime.Types"
      , msgSendImports
      , "import ObjC.Runtime.Selector (mkSelector)"
      , "import ObjC.Runtime.Class (getRequiredClass)"
      ]

    internalImport =
      [ "import ObjC." <> framework <> ".Internal.Classes" ]

    -- Enum imports
    enumMap = ktEnums allKnown
    clsEnumRefs = referencedEnumNames enumMap allMethodTypes
    localEnumRefs = Set.filter
      (\eName -> case Map.lookup eName enumMap of
        Just ed -> enumFramework ed == Just framework
        Nothing -> False)
      clsEnumRefs
    localEnumModule
      | Set.null localEnumRefs = []
      | otherwise = ["import ObjC." <> framework <> ".Internal.Enums"]
    crossEnumFws = Set.fromList
      [ fw
      | eName <- Set.toList clsEnumRefs
      , Just ed <- [Map.lookup eName enumMap]
      , Just fw <- [enumFramework ed]
      , fw /= framework
      , fwToPackageName fw `Set.member` depFws
      ]
    crossEnumImports = fmap
      (\fw -> "import ObjC." <> fw <> ".Internal.Enums")
      (Set.toList crossEnumFws)
    enumImports = localEnumModule ++ crossEnumImports

    -- Struct imports
    structSet = ktStructs allKnown
    clsStructRefs = classReferencedStructs structSet framework cls
      <> foldMap (referencedStructs structSet) allMethodTypes
    localStructRefs = Set.filter
      (\sName -> case Map.lookup sName (hierarchyStructs hierarchy) of
        Just sd -> structFramework sd == Just framework
        Nothing -> False)
      clsStructRefs
    localStructModule
      | Set.null localStructRefs = []
      | otherwise =
          ["import ObjC." <> framework <> ".Internal.Structs"]
    crossStructFws = Set.fromList
      [ fw
      | sName <- Set.toList clsStructRefs
      , Just sd <- [Map.lookup sName (hierarchyStructs hierarchy)]
      , Just fw <- [structFramework sd]
      , fw /= framework
      , fwToPackageName fw `Set.member` depFws
      ]
    crossStructImports = fmap
      (\fw -> "import ObjC." <> fw <> ".Internal.Structs")
      (Set.toList crossStructFws)
    structImports = localStructModule ++ crossStructImports

    -- Cross-framework class imports
    methodRefs = foldMap referencedClasses allMethodTypes
    ancestors = fromMaybe Set.empty
      (Map.lookup (className cls) (hierarchyAncestors hierarchy))
    allDeps = Set.insert (className cls) (Set.union methodRefs ancestors)

    crossFws = Set.fromList
      [ fw
      | dep <- Set.toList allDeps
      , Set.member dep (ktAll allKnown)
      , Just fw <- [Map.lookup dep fwMap]
      , fw /= framework
      , fwToPackageName fw `Set.member` depFws
      ]
    crossFwImportLines
      | Set.null crossFws = []
      | otherwise = fmap
          (\fw -> "import ObjC." <> fw <> ".Internal.Classes")
          (Set.toList crossFws)
