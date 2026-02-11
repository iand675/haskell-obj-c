{-# LANGUAGE OverloadedStrings #-}

-- | Package generation.
--
-- Generates the overall package structure for a framework:
--
-- * Determines which modules to generate
-- * Computes cross-framework dependencies
-- * Collects everything into a 'GeneratedPackage'
-- * Generates @package.yaml@ contents
module ObjC.CodeGen.Generate.Package
  ( -- * Top-level entry point
    generateFrameworkPackages
    -- * Single-package generation
  , generatePackage
    -- * Dependency computation
  , frameworkDependencies
  , classFrameworkMap
  , computePackageDeps
    -- * Type closure
  , computeTypeClosure
  , classReferencedTypes
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap
import ObjC.CodeGen.Generate.Types (GeneratedModule(..), GeneratedPackage(..), SkippedBinding(..))
import ObjC.CodeGen.Generate.Shared
  ( isStructFieldSupported, isMethodSupported, isStructSupported
  , propertyToMethods, methodUnsupportedType
  , computeTypeClosure, classReferencedTypes
  )
import ObjC.CodeGen.Generate.Enums
  ( generateEnumsModule, generatePublicEnumsModule, frameworkEnums )
import ObjC.CodeGen.Generate.Structs
  ( generateStructsModule, generatePublicStructsModule
  , generateStructReExportModule, frameworkStructs
  , resolveOpaqueStructFields
  )
import ObjC.CodeGen.Generate.InternalClasses (generateInternalClassesModule)
import ObjC.CodeGen.Generate.ClassModule
  ( generateClassModule, generateExtensionModules )

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

-- | Generate packages for all (or a targeted subset of) frameworks
-- found in the class hierarchy.
generateFrameworkPackages :: ClassHierarchy -> Maybe Text -> [GeneratedPackage]
generateFrameworkPackages hierarchy0 targetFw =
  let hierarchy = resolveOpaqueStructFields hierarchy0
      fwMap    = hierarchyFrameworks hierarchy
      classNames_ = Map.keysSet (hierarchyClasses hierarchy)
      generatedStructNames = Set.fromList
        [ structTypedefName sd
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , isStructSupported classNames_ sd
        ]
      allKnown = KnownTypes
        { ktClasses = classNames_
        , ktStructs = generatedStructNames
        , ktEnums   = hierarchyEnums hierarchy
        , ktAvailableFrameworks = allFwNames
        }

      -- Group classes by framework
      fwGroups :: Map Text [Text]
      fwGroups = Map.foldlWithKey'
        (\m cls fw -> Map.insertWith (++) fw [cls] m)
        Map.empty fwMap

      -- Frameworks that have structs but no classes
      structOnlyFws :: Map Text [Text]
      structOnlyFws = Map.fromList
        [ (fw, [])
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , Just fw <- [structFramework sd]
        , not (Map.member fw fwGroups)
        ]

      allFwGroups :: Map Text [Text]
      allFwGroups = Map.union fwGroups structOnlyFws

      allFwNames = Map.keysSet allFwGroups

      allPackages = fmap (generatePackage hierarchy fwMap allKnown allFwNames)
                         (Map.toList allFwGroups)

      targetPackages = case targetFw of
        Nothing -> allPackages
        Just fw ->
          let pkgMap = Map.fromList
                [ (gpkgFramework pkg, pkg) | pkg <- allPackages ]
              closure = transitiveFwClosure pkgMap (Set.singleton fw)
          in filter (\pkg -> Set.member (gpkgFramework pkg) closure) allPackages

  in targetPackages

-- ---------------------------------------------------------------------------
-- Transitive framework closure
-- ---------------------------------------------------------------------------

transitiveFwClosure :: Map Text GeneratedPackage -> Set Text -> Set Text
transitiveFwClosure pkgMap = go Set.empty
  where
    go visited frontier
      | Set.null newFws = visited'
      | otherwise       = go visited' newFws
      where
        visited' = Set.union visited frontier
        depFws = foldMap
          (\fw -> case Map.lookup fw pkgMap of
            Just pkg -> Set.fromList
              [ depFw
              | depPkgName <- Set.toList (gpkgDeps pkg)
              , let depFw = pkgNameToFramework depPkgName
              ]
            Nothing -> Set.empty)
          (Set.toList frontier)
        newFws = Set.difference depFws visited'

    pkgNameToFramework :: Text -> Text
    pkgNameToFramework pkgName =
      case filter (\fw -> "objc-" <> T.toLower fw == pkgName) (Map.keys pkgMap) of
        (fw : _) -> fw
        []       -> T.drop 5 pkgName

-- ---------------------------------------------------------------------------
-- Single package generation
-- ---------------------------------------------------------------------------

-- | Generate a single framework package.
generatePackage
  :: ClassHierarchy
  -> Map Text Text   -- ^ class name -> framework name
  -> KnownTypes      -- ^ all known types
  -> Set Text        -- ^ all framework names with generated packages
  -> (Text, [Text])  -- ^ (framework name, class names)
  -> GeneratedPackage
generatePackage hierarchy fwMap allKnown availableFws (framework, classNames) =
  let genSet  = Set.fromList classNames
      ordered = filter (`Set.member` genSet) (hierarchyTopoOrder hierarchy)
      hasClasses = not (null classNames)

      -- Internal module
      internalMod = generateInternalClassesModule fwMap hierarchy framework ordered depFws

      -- Structs module
      fwStructs = frameworkStructs hierarchy framework
      structsMod = generateStructsModule hierarchy framework fwStructs

      -- Enums module
      fwEnums = frameworkEnums hierarchy framework
      enumsMod = generateEnumsModule framework fwEnums

      -- Dependency computation
      depFwsRaw = computePackageDeps hierarchy fwMap allKnown framework genSet
      availablePkgNames = Set.map (\fw -> "objc-" <> T.toLower fw) availableFws
      depFws = Set.intersection depFwsRaw availablePkgNames

      -- Per-class modules
      modules = fmap (generateClassModule fwMap hierarchy allKnown framework depFws) ordered

      -- Extension modules
      extModules = generateExtensionModules fwMap hierarchy allKnown framework depFws

      -- Protocol modules (disabled pending superprotocol constraint handling)
      -- TODO: Re-enable once protocol instance generation handles superprotocol
      -- constraints, __kindof types, and known-typedef parameters correctly.
      protocolsMod = Nothing -- generateProtocolsModule ...

      -- Public re-export modules
      publicStructsMod = generatePublicStructsModule prefix fwStructs
      publicEnumsMod   = generatePublicEnumsModule prefix fwEnums

      allExtraModules = concat
        [ [internalMod       | hasClasses]
        , [structsMod        | not (null fwStructs)]
        , [publicStructsMod  | not (null fwStructs)]
        , [enumsMod          | not (null fwEnums)]
        , [publicEnumsMod    | not (null fwEnums)]
        , maybe [] (: []) protocolsMod
        , modules
        , extModules
        ]

      pkgName  = "objc-" <> T.toLower framework
      prefix   = "ObjC." <> framework
      structReExport = generateStructReExportModule prefix fwStructs
      pkgYaml
        | hasClasses = generatePackageYaml pkgName framework depFws allExtraModules
        | otherwise  = generatePackageYaml pkgName framework depFws
                         (structReExport : allExtraModules)

      -- Skip reports
      methodSkips = concatMap (classMethodSkips allKnown framework) $
        mapMaybe (\n -> Map.lookup n (hierarchyClasses hierarchy)) ordered
      extSkips = concatMap (classMethodSkips allKnown framework . snd)
        [ (name, cls)
        | (name, cls) <- Map.toList (hierarchyClasses hierarchy)
        , classFramework cls /= Just framework
        , any (\m -> methodOriginFramework m == Just framework) (classMethods cls)
        ]
      protoSkips = concatMap (protoMethodSkips allKnown framework) $
        filter (\p -> protoDeclFramework p == Just framework)
               (Map.elems (hierarchyProtocols hierarchy))
      structSkips =
        [ SkippedBinding
          { skipFramework = framework
          , skipClass     = structTypedefName sd
          , skipSelector  = fName
          , skipReason    = "unsupported struct field type: " <> T.pack (show fTy)
          }
        | sd <- fwStructs
        , (fName, fTy) <- structFields sd
        , not (isStructFieldSupported (ktAll allKnown) (fName, fTy))
        ]
      allSkips = methodSkips ++ extSkips ++ protoSkips ++ structSkips

  in GeneratedPackage
    { gpkgName      = pkgName
    , gpkgFramework = framework
    , gpkgDeps      = depFws
    , gpkgModules   = allExtraModules
    , gpkgReExport  = if hasClasses then Nothing else Just structReExport
    , gpkgPackageYaml = pkgYaml
    , gpkgSkipped   = allSkips
    }

-- ---------------------------------------------------------------------------
-- Skip reports
-- ---------------------------------------------------------------------------

classMethodSkips :: KnownTypes -> Text -> ObjCClass -> [SkippedBinding]
classMethodSkips kt fw cls =
  let methods = classMethods cls ++ concatMap propertyToMethods (classProperties cls)
  in mapMaybe (methodSkip kt fw (className cls)) methods

protoMethodSkips :: KnownTypes -> Text -> ObjCProtocolDecl -> [SkippedBinding]
protoMethodSkips kt fw proto =
  let allMethods = protoDeclRequired proto ++ protoDeclOptional proto
        ++ concatMap propertyToMethods (protoDeclReqProps proto)
        ++ concatMap propertyToMethods (protoDeclOptProps proto)
  in mapMaybe (methodSkip kt fw (protoDeclName proto)) allMethods

methodSkip :: KnownTypes -> Text -> Text -> ObjCMethod -> Maybe SkippedBinding
methodSkip kt fw clsName m =
  case methodUnsupportedType kt m of
    Nothing -> Nothing
    Just ty -> Just SkippedBinding
      { skipFramework = fw
      , skipClass     = clsName
      , skipSelector  = methodSelector m
      , skipReason    = "unsupported type: " <> T.pack (show ty)
      }

-- ---------------------------------------------------------------------------
-- package.yaml generation
-- ---------------------------------------------------------------------------

generatePackageYaml :: Text -> Text -> Set Text -> [GeneratedModule] -> Text
generatePackageYaml pkgName framework deps modules = T.unlines $
  [ "name:                " <> pkgName
  , "version:             0.1.0.0"
  , "synopsis:            Generated Haskell bindings for the " <> framework <> " framework"
  , "license:             BSD-3-Clause"
  , ""
  , "dependencies:"
  , "- base >= 4.7 && < 5"
  , "- haskell-obj-c"
  , "- libffi"
  ] ++ fmap (\d -> "- " <> d) (Set.toList deps) ++
  [ ""
  , "ghc-options:"
  , "- -Wall"
  , "- -Wno-orphans"
  , ""
  , "library:"
  , "  source-dirs: src"
  , "  exposed-modules:"
  ] ++ fmap (\m -> "  - " <> m) (Set.toList (Set.fromList (fmap genModuleName modules)))

-- ---------------------------------------------------------------------------
-- Dependency computation
-- ---------------------------------------------------------------------------

-- | Build a map from class name to framework name.
classFrameworkMap :: ClassHierarchy -> Map Text Text
classFrameworkMap hierarchy =
  Map.fromList
    [ (name, fw)
    | (name, cls) <- Map.toList (hierarchyClasses hierarchy)
    , Just fw <- [classFramework cls]
    ]

-- | Compute the set of dependency package names for a framework.
frameworkDependencies :: ClassHierarchy -> Text -> Set Text
frameworkDependencies hierarchy framework =
  Set.fromList
    [ "objc-" <> T.toLower depFw
    | (_name, cls) <- Map.toList (hierarchyClasses hierarchy)
    , classFramework cls == Just framework
    , depFw <- classDepFrameworks hierarchy cls
    , depFw /= framework
    ]

classDepFrameworks :: ClassHierarchy -> ObjCClass -> [Text]
classDepFrameworks hierarchy cls =
  let superFws = case classSuperclass cls of
        Nothing -> []
        Just super -> case Map.lookup super (hierarchyClasses hierarchy) of
          Just sCls -> case classFramework sCls of
            Just fw -> [fw]
            Nothing -> []
          Nothing -> []
      methodTypeFws = concatMap typeFrameworks
        (concatMap methodAllTypes (classMethods cls))
  in superFws ++ methodTypeFws
  where
    methodAllTypes m = methodReturnType m : fmap snd (methodParams m)
    typeFrameworks (ObjCId (Just n) _) =
      case Map.lookup n (hierarchyClasses hierarchy) of
        Just c -> case classFramework c of
          Just fw -> [fw]
          Nothing -> []
        Nothing -> []
    typeFrameworks (ObjCGeneric n _ _) =
      case Map.lookup n (hierarchyClasses hierarchy) of
        Just c -> case classFramework c of
          Just fw -> [fw]
          Nothing -> []
        Nothing -> []
    typeFrameworks _ = []

-- | Compute the package dependencies for a framework.
computePackageDeps
  :: ClassHierarchy -> Map Text Text -> KnownTypes
  -> Text -> Set Text -> Set Text
computePackageDeps hierarchy fwMap allKnown framework ownClasses =
  let -- Superclass deps
      superDeps = Set.fromList
        [ "objc-" <> T.toLower fw
        | name <- Set.toList ownClasses
        , Just cls <- [Map.lookup name (hierarchyClasses hierarchy)]
        , Just super <- [classSuperclass cls]
        , Just fw <- [Map.lookup super fwMap]
        , fw /= framework
        ]
      -- Method type deps
      methodDeps = Set.fromList
        [ "objc-" <> T.toLower fw
        | name <- Set.toList ownClasses
        , Just cls <- [Map.lookup name (hierarchyClasses hierarchy)]
        , m <- classMethods cls
        , isMethodSupported allKnown m
        , ty <- methodReturnType m : fmap snd (methodParams m)
        , depName <- Set.toList (referencedClasses' ty)
        , Just fw <- [Map.lookup depName fwMap]
        , fw /= framework
        ]
      -- Struct deps
      structDeps = Set.fromList
        [ "objc-" <> T.toLower fw
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , structFramework sd == Just framework
        , (_, ObjCStruct refName) <- structFields sd
        , Just refSd <- [Map.lookup refName (hierarchyStructs hierarchy)]
        , Just fw <- [structFramework refSd]
        , fw /= framework
        ]
  in Set.unions [superDeps, methodDeps, structDeps]
  where
    referencedClasses' :: ObjCType -> Set Text
    referencedClasses' (ObjCId (Just n) _)     = Set.singleton n
    referencedClasses' (ObjCGeneric n args _)   =
      Set.insert n (foldMap referencedClasses' args)
    referencedClasses' (ObjCPointer inner)     = referencedClasses' inner
    referencedClasses' (ObjCQualified _ inner)  = referencedClasses' inner
    referencedClasses' (ObjCBlock ret params)  =
      referencedClasses' ret <> foldMap referencedClasses' params
    referencedClasses' _ = Set.empty
