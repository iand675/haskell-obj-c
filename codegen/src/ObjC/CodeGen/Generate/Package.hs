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
    -- * Extension packages
  , generateExtensionPackages
    -- * Dependency computation
  , frameworkDependencies
  , classFrameworkMap
  , computePackageDeps
    -- * Type closure
  , computeTypeClosure
  , classReferencedTypes
  ) where

import Data.Char (toLower)
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
  , fwToPackageName
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
  ( generateClassModule, generateExtensionModules, mkExtMod )
import ObjC.CodeGen.Generate.DelegateModule
  ( generateDelegateModules )

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

-- | Generate packages for all (or a targeted subset of) frameworks
-- found in the class hierarchy, plus extension packages for
-- cross-framework category methods.
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

      -- Group classes by framework (skip private frameworks starting with '_')
      fwGroups :: Map Text [Text]
      fwGroups = Map.foldlWithKey'
        (\m cls fw -> if isPrivateFramework fw then m
                      else Map.insertWith (++) fw [cls] m)
        Map.empty fwMap

      -- Frameworks that have structs but no classes
      structOnlyFws :: Map Text [Text]
      structOnlyFws = Map.fromList
        [ (fw, [])
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , Just fw <- [structFramework sd]
        , not (isPrivateFramework fw)
        , not (Map.member fw fwGroups)
        ]

      allFwGroups :: Map Text [Text]
      allFwGroups = Map.union fwGroups structOnlyFws

      allFwNames = Map.keysSet allFwGroups

      -- Step 1: compute core (structural) deps per framework as framework
      -- names — these are acyclic by construction.
      coreDepFwMap :: Map Text (Set Text)
      coreDepFwMap = Map.mapWithKey
        (\fw clss -> computeCoreDepsFw hierarchy fwMap fw (Set.fromList clss))
        allFwGroups

      -- Step 2: compute raw method deps per framework (as framework names).
      rawMethodDepFwMap :: Map Text (Set Text)
      rawMethodDepFwMap = Map.mapWithKey
        (\fw clss -> computeMethodDepsFw hierarchy fwMap allKnown fw (Set.fromList clss))
        allFwGroups

      -- Step 3: iteratively accept method deps that don't create cycles.
      -- Start from core deps and greedily add method deps, recomputing
      -- reachability after each addition to catch indirect cycles.
      finalDepFwMap :: Map Text (Set Text)
      finalDepFwMap = addMethodDepsNoCycles coreDepFwMap rawMethodDepFwMap

      -- Convert to package names.
      safeDeps :: Map Text (Set Text)
      safeDeps = Map.map (Set.map fwToPackageName) finalDepFwMap

      basePkgs = fmap (generatePackageWith safeDeps hierarchy fwMap allKnown allFwNames)
                      (Map.toList allFwGroups)

      -- Extension packages for cross-framework categories
      extPkgs = generateExtensionPackages hierarchy fwMap allKnown allFwNames

      -- Methods that reference cycle-inducing types will naturally
      -- fall back to RawId since their framework won't be in depFws.
      allPackages = basePkgs ++ extPkgs

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
      case filter (\fw -> fwToPackageName fw == pkgName) (Map.keys pkgMap) of
        (fw : _) -> fw
        []       -> T.drop 5 pkgName

-- | Compute the set of frameworks transitively reachable from a given
-- framework via the core (structural) dependency graph.
-- The input map is @framework -> Set packageName@ (core deps only).
reachableFrom :: Map Text (Set Text) -> Text -> Set Text
reachableFrom depMap start = go Set.empty (Set.singleton start)
  where
    go visited frontier
      | Set.null newFws = visited'
      | otherwise       = go visited' newFws
      where
        visited' = Set.union visited frontier
        depPkgs  = foldMap
          (\fw -> Map.findWithDefault Set.empty fw depMap)
          (Set.toList frontier)
        newFws   = Set.difference depPkgs visited'

-- ---------------------------------------------------------------------------
-- Single package generation
-- ---------------------------------------------------------------------------

-- | Generate a single framework package using precomputed dependencies.
generatePackageWith
  :: Map Text (Set Text)  -- ^ precomputed per-framework deps (package names)
  -> ClassHierarchy
  -> Map Text Text
  -> KnownTypes
  -> Set Text
  -> (Text, [Text])
  -> GeneratedPackage
generatePackageWith depOverrides hierarchy fwMap allKnown availableFws (framework, classNames) =
  let genSet  = Set.fromList classNames
      ordered = dedupCaseInsensitive $
                  filter (`Set.member` genSet) (hierarchyTopoOrder hierarchy)
      hasClasses = not (null classNames)

      -- Use precomputed deps (already cycle-free)
      depFwsRaw = Map.findWithDefault Set.empty framework depOverrides
      availablePkgNames = Set.map fwToPackageName availableFws
      depFws = Set.intersection depFwsRaw availablePkgNames

      internalMod = generateInternalClassesModule fwMap hierarchy framework ordered depFws
      fwStructs = frameworkStructs hierarchy framework
      structsMod = generateStructsModule hierarchy framework fwStructs
      fwEnums = frameworkEnums hierarchy framework
      enumsMod = generateEnumsModule framework fwEnums

      modules = fmap (generateClassModule fwMap hierarchy allKnown framework depFws) ordered
      protocolsMod = Nothing

      -- Delegate modules (per-protocol override records + constructors)
      delegateMods = generateDelegateModules hierarchy framework

      pkgName  = fwToPackageName framework
      prefix   = "ObjC." <> framework
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
        , delegateMods
        ]

      structReExport = generateStructReExportModule prefix fwStructs
      pkgYaml
        | hasClasses = generatePackageYaml pkgName framework depFws allExtraModules
        | otherwise  = generatePackageYaml pkgName framework depFws
                         (structReExport : allExtraModules)

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
      ordered = dedupCaseInsensitive $
                  filter (`Set.member` genSet) (hierarchyTopoOrder hierarchy)
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
      availablePkgNames = Set.map fwToPackageName availableFws
      depFws = Set.intersection depFwsRaw availablePkgNames

      -- Per-class modules
      modules = fmap (generateClassModule fwMap hierarchy allKnown framework depFws) ordered

      -- Protocol modules (disabled pending superprotocol constraint handling)
      -- TODO: Re-enable once protocol instance generation handles superprotocol
      -- constraints, __kindof types, and known-typedef parameters correctly.
      protocolsMod = Nothing -- generateProtocolsModule ...

      -- Delegate modules (per-protocol override records + constructors)
      delegateMods = generateDelegateModules hierarchy framework

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
        , delegateMods
        ]

      pkgName  = fwToPackageName framework
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
-- Extension packages
-- ---------------------------------------------------------------------------

-- | Generate separate packages for cross-framework category methods.
--
-- When framework A defines ObjC categories on classes belonging to
-- framework B, the generated bindings go into a standalone package
-- @objc-\<a\>-\<b\>-ext@ that depends on both @objc-a@ and @objc-b@
-- (plus any other frameworks referenced by the extension methods).
-- This avoids dependency cycles between the base framework packages.
generateExtensionPackages
  :: ClassHierarchy
  -> Map Text Text   -- ^ class name -> framework name
  -> KnownTypes
  -> Set Text        -- ^ all framework names
  -> [GeneratedPackage]
generateExtensionPackages hierarchy fwMap allKnown allFwNames =
  let availablePkgNames = Set.map fwToPackageName allFwNames
      -- Discover (categoryFw, classFw) pairs
      pairs :: Map (Text, Text) [(Text, ObjCClass)]
      pairs = Map.foldlWithKey' collectPairs Map.empty (hierarchyClasses hierarchy)

      collectPairs acc _name cls =
        case classFramework cls of
          Nothing -> acc
          Just classFw ->
            let -- category frameworks that differ from the class framework
                catFws = Set.fromList
                  [ catFw
                  | m <- classMethods cls
                  , Just catFw <- [methodOriginFramework m]
                  , catFw /= classFw
                  , Set.member catFw allFwNames
                  ]
                catFwsP = Set.fromList
                  [ catFw
                  | p <- classProperties cls
                  , Just catFw <- [propOriginFramework p]
                  , catFw /= classFw
                  , Set.member catFw allFwNames
                  ]
                allCatFws = Set.union catFws catFwsP
            in Set.foldl'
                 (\m catFw -> Map.insertWith (++) (catFw, classFw) [(_name, cls)] m)
                 acc allCatFws

  in mapMaybe (mkExtPkg hierarchy fwMap allKnown availablePkgNames)
              (Map.toList pairs)

-- | Build one extension package for a (categoryFw, classFw) pair.
mkExtPkg
  :: ClassHierarchy
  -> Map Text Text
  -> KnownTypes
  -> Set Text        -- ^ available package names
  -> ((Text, Text), [(Text, ObjCClass)])
  -> Maybe GeneratedPackage
mkExtPkg hierarchy fwMap allKnown availablePkgNames ((catFw, classFw), classes) =
  let pkgName = "apple-" <> T.toLower catFw <> "-" <> T.toLower classFw <> "-gen-ext"
      -- The extension modules live under the category framework's namespace
      -- so they don't conflict with the base class modules.
      -- Deps: both base framework packages + any types the extension methods reference
      baseDeps = Set.fromList
        [ fwToPackageName catFw
        , fwToPackageName classFw
        ]
      -- Compute additional deps from types used in extension methods
      extraDeps = Set.fromList
        [ fwToPackageName fw
        | (_name, cls) <- classes
        , m <- classMethods cls
        , methodOriginFramework m == Just catFw
        , isMethodSupported allKnown m
        , ty <- methodReturnType m : fmap snd (methodParams m)
        , depName <- Set.toList (referencedClasses' ty)
        , Just fw <- [Map.lookup depName fwMap]
        , fw /= catFw && fw /= classFw
        ]
      allDeps = Set.intersection
        (Set.difference (Set.unions [baseDeps, extraDeps])
                        (Set.singleton pkgName))
        availablePkgNames
      -- Generate the extension modules from the already-grouped class list.
      -- Using `classes` directly (rather than re-discovering all extension
      -- classes) ensures each (catFw, classFw) pair only emits modules for
      -- its own classes, avoiding cross-framework misplacement.
      extModules = fmap (mkExtMod fwMap hierarchy allKnown catFw allDeps) classes
  in if null extModules then Nothing
     else Just GeneratedPackage
       { gpkgName      = pkgName
       , gpkgFramework = catFw <> "+" <> classFw
       , gpkgDeps      = allDeps
       , gpkgModules   = extModules
       , gpkgReExport  = Nothing
       , gpkgPackageYaml = generatePackageYaml pkgName
           (catFw <> " extensions on " <> classFw)
           allDeps extModules
       , gpkgSkipped   = []
       }
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
-- Private framework filtering
-- ---------------------------------------------------------------------------

-- | Private/internal Apple frameworks (prefixed with @_@) cannot be
-- used as Haskell package names and should be excluded from
-- generation and dependency lists.
isPrivateFramework :: Text -> Bool
isPrivateFramework fw = case T.uncons fw of
  Just ('_', _) -> True
  _             -> False

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
-- Case-insensitive deduplication
-- ---------------------------------------------------------------------------

-- | Remove class names that would collide on a case-insensitive filesystem.
--
-- Apple sometimes provides both an old name (e.g., @MTRBaseClusterOTA...@)
-- and a new name (e.g., @MTRBaseClusterOta...@) for the same class.
-- On macOS (HFS+/APFS case-insensitive), both generate the same file path.
-- We keep the first occurrence (alphabetically) and drop duplicates.
dedupCaseInsensitive :: [Text] -> [Text]
dedupCaseInsensitive = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member key seen = go seen xs
      | otherwise           = x : go (Set.insert key seen) xs
      where key = T.map toLower x

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
    [ fwToPackageName depFw
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
--
-- This returns the full set of dependencies (core + method).
-- Use 'computeCoreDeps' and 'computeMethodDeps' separately when you need
-- cycle-aware dependency computation.
computePackageDeps
  :: ClassHierarchy -> Map Text Text -> KnownTypes
  -> Text -> Set Text -> Set Text
computePackageDeps hierarchy fwMap allKnown framework ownClasses =
  Set.unions
    [ computeCoreDeps hierarchy fwMap framework ownClasses
    , computeMethodDeps hierarchy fwMap allKnown framework ownClasses
    ]

-- | Iteratively add method dependencies to the core dep graph, skipping
-- any edge that would introduce a cycle.  Processes all frameworks in a
-- fixed order; for each framework, tries to add each method dep and
-- checks for cycles via transitive reachability in the accumulated graph.
addMethodDepsNoCycles
  :: Map Text (Set Text)  -- ^ core dep graph (framework → frameworks)
  -> Map Text (Set Text)  -- ^ raw method deps per framework
  -> Map Text (Set Text)  -- ^ final dep graph (core + safe method deps)
addMethodDepsNoCycles coreGraph rawMethods =
  foldl tryAddFw coreGraph (Map.toList rawMethods)
  where
    tryAddFw graph (fw, mDeps) =
      foldl (tryAddEdge fw) graph (Set.toList mDeps)

    tryAddEdge fw graph depFw
      -- Adding fw → depFw creates a cycle if fw is reachable from depFw
      -- in the current graph.
      | fw `Set.member` reachableFrom graph depFw = graph
      | otherwise = Map.insertWith Set.union fw (Set.singleton depFw) graph

-- | Core (structural) dependencies as framework names.
-- Used for cycle detection in the reachability graph.
computeCoreDepsFw
  :: ClassHierarchy -> Map Text Text
  -> Text -> Set Text -> Set Text
computeCoreDepsFw hierarchy fwMap framework ownClasses =
  let superDeps = Set.fromList
        [ fw
        | name <- Set.toList ownClasses
        , Just cls <- [Map.lookup name (hierarchyClasses hierarchy)]
        , Just super <- [classSuperclass cls]
        , Just fw <- [Map.lookup super fwMap]
        , fw /= framework
        , not (isPrivateFramework fw)
        ]
      structDeps = Set.fromList
        [ fw
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , structFramework sd == Just framework
        , (_, ObjCStruct refName) <- structFields sd
        , Just refSd <- [Map.lookup refName (hierarchyStructs hierarchy)]
        , Just fw <- [structFramework refSd]
        , fw /= framework
        , not (isPrivateFramework fw)
        ]
  in Set.union superDeps structDeps

-- | Core (structural) dependencies: superclass and struct-field references.
-- These form a DAG by construction and cannot create cycles.
-- Returns package names (not framework names).
computeCoreDeps
  :: ClassHierarchy -> Map Text Text
  -> Text -> Set Text -> Set Text
computeCoreDeps hierarchy fwMap framework ownClasses =
  let superDeps = Set.fromList
        [ fwToPackageName fw
        | name <- Set.toList ownClasses
        , Just cls <- [Map.lookup name (hierarchyClasses hierarchy)]
        , Just super <- [classSuperclass cls]
        , Just fw <- [Map.lookup super fwMap]
        , fw /= framework
        , not (isPrivateFramework fw)
        ]
      structDeps = Set.fromList
        [ fwToPackageName fw
        | sd <- Map.elems (hierarchyStructs hierarchy)
        , structFramework sd == Just framework
        , (_, ObjCStruct refName) <- structFields sd
        , Just refSd <- [Map.lookup refName (hierarchyStructs hierarchy)]
        , Just fw <- [structFramework refSd]
        , fw /= framework
        , not (isPrivateFramework fw)
        ]
  in Set.union superDeps structDeps

-- | Method type dependencies as framework names (not package names).
-- Used for cycle-aware dependency computation.
computeMethodDepsFw
  :: ClassHierarchy -> Map Text Text -> KnownTypes
  -> Text -> Set Text -> Set Text
computeMethodDepsFw hierarchy fwMap allKnown framework ownClasses =
  let methodBelongsHere m = case methodOriginFramework m of
        Nothing -> True
        Just fw -> fw == framework
  in Set.fromList
        [ fw
        | name <- Set.toList ownClasses
        , Just cls <- [Map.lookup name (hierarchyClasses hierarchy)]
        , m <- classMethods cls
        , methodBelongsHere m
        , isMethodSupported allKnown m
        , ty <- methodReturnType m : fmap snd (methodParams m)
        , depName <- Set.toList (referencedClasses' ty)
        , Just fw <- [Map.lookup depName fwMap]
        , fw /= framework
        , not (isPrivateFramework fw)
        ]
  where
    referencedClasses' :: ObjCType -> Set Text
    referencedClasses' (ObjCId (Just n) _) = Set.singleton n
    referencedClasses' (ObjCGeneric n _ _) = Set.singleton n
    referencedClasses' _                   = Set.empty

-- | Method type dependencies: frameworks referenced by method parameter and
-- return types.  Only considers methods originating from *this* framework.
-- Returns package names.
computeMethodDeps
  :: ClassHierarchy -> Map Text Text -> KnownTypes
  -> Text -> Set Text -> Set Text
computeMethodDeps hierarchy fwMap allKnown framework ownClasses =
  let methodBelongsHere m = case methodOriginFramework m of
        Nothing -> True
        Just fw -> fw == framework
  in Set.fromList
        [ fwToPackageName fw
        | name <- Set.toList ownClasses
        , Just cls <- [Map.lookup name (hierarchyClasses hierarchy)]
        , m <- classMethods cls
        , methodBelongsHere m
        , isMethodSupported allKnown m
        , ty <- methodReturnType m : fmap snd (methodParams m)
        , depName <- Set.toList (referencedClasses' ty)
        , Just fw <- [Map.lookup depName fwMap]
        , fw /= framework
        , not (isPrivateFramework fw)
        ]
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
