{-# LANGUAGE OverloadedStrings #-}

-- | Shared utilities for the code generator: type traversal,
-- method filtering, property synthesis, and support checks.
--
-- These are used across multiple generation sub-modules
-- ('Generate.ClassModule', 'Generate.Package', 'Generate.Protocols').
module ObjC.CodeGen.Generate.Shared
  ( -- * Type traversal
    referencedClasses
  , referencedStructs
  , referencedEnumNames
  , referencedEnumsInType
  , classReferencedTypes
  , classReferencedStructs
    -- * Type closure
  , computeTypeClosure
    -- * Method support checks
  , isMethodSupported
  , methodUnsupportedType
  , isStructSupported
  , isStructFieldSupported
  , isFieldTypeSupported
    -- * Property → method synthesis
  , propertyToMethods
  , allClassMethods
    -- * Deduplication
  , dedupBySelector
  , dedupByHsName
    -- * Struct return detection
  , isStructReturn
    -- * Nullability stripping
  , stripNullab
    -- * Ownership
  , ownershipWrapper
    -- * Managed parameter detection
  , isManagedObjCParam
    -- * Package naming
  , fwToPackageName
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap
import ObjC.CodeGen.Generate.Naming (methodHaskellName, instanceMethodNameSet, upperFirst)

-- ---------------------------------------------------------------------------
-- Type traversal
-- ---------------------------------------------------------------------------

-- | Collect all class names referenced in an 'ObjCType'.
referencedClasses :: ObjCType -> Set Text
referencedClasses (ObjCId (Just name) _)     = Set.singleton name
referencedClasses (ObjCGeneric name args _)  =
  Set.insert name (foldMap referencedClasses args)
referencedClasses (ObjCPointer inner)        = referencedClasses inner
referencedClasses (ObjCQualified _ inner)    = referencedClasses inner
referencedClasses (ObjCBlock ret params)     =
  referencedClasses ret <> foldMap referencedClasses params
referencedClasses _                          = Set.empty

-- | Collect all struct names referenced in a type.
referencedStructs :: Set Text -> ObjCType -> Set Text
referencedStructs _structs (ObjCStruct name)           = Set.singleton name
referencedStructs structs  (ObjCPointer inner)         = referencedStructs structs inner
referencedStructs structs  (ObjCQualified _ inner)     = referencedStructs structs inner
referencedStructs structs  (ObjCBlock ret params)      =
  referencedStructs structs ret <> foldMap (referencedStructs structs) params
referencedStructs _structs (ObjCPrimitive q d)
  | q == d && isStructLikePrimitive d                  = Set.singleton d
referencedStructs structs  (ObjCId (Just n) _)
  | Set.member n structs  = Set.singleton n
  | Set.member bn structs = Set.singleton bn
  where bn = extractClassName n
referencedStructs _structs _                           = Set.empty

-- | Collect all known enum names referenced in a list of types.
referencedEnumNames :: Map Text EnumDef -> [ObjCType] -> Set Text
referencedEnumNames enums = foldMap (referencedEnumsInType enums)

-- | Collect enum names referenced in a single type.
referencedEnumsInType :: Map Text EnumDef -> ObjCType -> Set Text
referencedEnumsInType enums (ObjCPrimitive q d)
  | "enum " `T.isPrefixOf` d
  , Just eName <- T.stripPrefix "enum " d
  , Map.member (T.strip eName) enums
    = Set.singleton (T.strip eName)
  | Map.member q enums = Set.singleton q
referencedEnumsInType enums (ObjCPointer inner) = referencedEnumsInType enums inner
referencedEnumsInType enums (ObjCQualified _ inner) = referencedEnumsInType enums inner
referencedEnumsInType enums (ObjCBlock ret params) =
  referencedEnumsInType enums ret <> foldMap (referencedEnumsInType enums) params
referencedEnumsInType _ _ = Set.empty

-- | Collect class names referenced by a class's explicit method
-- signatures, filtered to methods originating from the given framework.
classReferencedTypes :: Text -> ObjCClass -> Set Text
classReferencedTypes fw cls =
  let ownMethods = filter (\m -> methodOriginFramework m == Just fw) (classMethods cls)
      methodTypes = concatMap
        (\m -> methodReturnType m : fmap snd (methodParams m))
        ownMethods
  in foldMap referencedClasses methodTypes

-- | Collect struct names referenced by a class's method signatures,
-- filtered to methods originating from the given framework.
classReferencedStructs :: Set Text -> Text -> ObjCClass -> Set Text
classReferencedStructs structs fw cls =
  let ownMethods = filter (\m -> methodOriginFramework m == Just fw) (classMethods cls)
      methodTypes = concatMap
        (\m -> methodReturnType m : fmap snd (methodParams m))
        ownMethods
  in foldMap (referencedStructs structs) methodTypes

-- ---------------------------------------------------------------------------
-- Type closure
-- ---------------------------------------------------------------------------

-- | Compute the transitive closure of types referenced by a set of
-- seed classes, including all superclasses.
computeTypeClosure :: ClassHierarchy -> Set Text -> Set Text
computeTypeClosure hierarchy seeds = go seeds seeds
  where
    allClasses = Map.keysSet (hierarchyClasses hierarchy)

    allReferencedTypes :: ObjCClass -> Set Text
    allReferencedTypes cls =
      let methodTypes = concatMap
            (\m -> methodReturnType m : fmap snd (methodParams m))
            (classMethods cls)
      in foldMap referencedClasses methodTypes

    go visited frontier
      | Set.null newTypes = visited
      | otherwise         = go (Set.union visited newTypes) newTypes
      where
        refs = foldMap
          (\name -> maybe Set.empty allReferencedTypes
                      (Map.lookup name (hierarchyClasses hierarchy)))
          (Set.toList frontier)

        supers = foldMap
          (\name -> maybe Set.empty
                      (maybe Set.empty Set.singleton . classSuperclass)
                      (Map.lookup name (hierarchyClasses hierarchy)))
          (Set.toList frontier)

        newTypes = Set.intersection
          (Set.difference (Set.union refs supers) visited)
          allClasses

-- ---------------------------------------------------------------------------
-- Method support checks
-- ---------------------------------------------------------------------------

-- | Check whether all types in a method signature are supported.
isMethodSupported :: KnownTypes -> ObjCMethod -> Bool
isMethodSupported kt m =
  let allTypes = methodReturnType m : fmap snd (methodParams m)
  in all (isTypeSupported kt) allTypes

-- | Return the first unsupported type in a method, if any.
methodUnsupportedType :: KnownTypes -> ObjCMethod -> Maybe ObjCType
methodUnsupportedType kt m =
  let allTypes = methodReturnType m : fmap snd (methodParams m)
  in case filter (not . isTypeSupported kt) allTypes of
    (t : _) -> Just t
    []      -> Nothing

-- | Check whether all fields in a struct are supported.
isStructSupported :: Set Text -> StructDef -> Bool
isStructSupported known sd = all (isStructFieldSupported known) (structFields sd)

-- | Check whether a struct field type is supported.
isStructFieldSupported :: Set Text -> (Text, ObjCType) -> Bool
isStructFieldSupported known (_, ty) = isFieldTypeSupported known ty

-- | Check whether a struct field type is supported.
isFieldTypeSupported :: Set Text -> ObjCType -> Bool
isFieldTypeSupported _ (ObjCPrimitive q d) = isPrimitiveSupported q d
isFieldTypeSupported _ (ObjCStruct _)      = True
isFieldTypeSupported _ (ObjCPointer _)     = True
isFieldTypeSupported known (ObjCQualified _ inner) = isFieldTypeSupported known inner
isFieldTypeSupported _ _                   = False

-- ---------------------------------------------------------------------------
-- Property → method synthesis
-- ---------------------------------------------------------------------------

-- | Synthesize getter (and optionally setter) methods from a property.
propertyToMethods :: ObjCProperty -> [ObjCMethod]
propertyToMethods prop =
  let getter = ObjCMethod
        { methodSelector        = propName prop
        , methodReturnType      = propType prop
        , methodParams          = []
        , methodIsClass         = propIsClass prop
        , methodIsImplicit      = False
        , methodAvailability    = []
        , methodDoc             = propDoc prop
        , methodOriginFramework = propOriginFramework prop
        }
      setter = ObjCMethod
        { methodSelector        = "set" <> upperFirst (propName prop) <> ":"
        , methodReturnType      = ObjCVoid
        , methodParams          = [("value", propType prop)]
        , methodIsClass         = propIsClass prop
        , methodIsImplicit      = False
        , methodAvailability    = []
        , methodDoc             = propDoc prop
        , methodOriginFramework = propOriginFramework prop
        }
  in getter : [setter | not (propReadonly prop)]

-- | All methods for a class: explicit methods plus synthesized property
-- accessors, deduplicated by selector.
--
-- The first set is the importable names (classes + structs from available
-- frameworks).  The second set is /all/ known class names across all
-- frameworks.  The distinction matters for the importability check:
-- a referenced name that is a known class but not importable means
-- we lack the framework dependency and must skip the method.  A name
-- that is not a known class at all (protocol, type parameter, etc.)
-- maps to 'RawId' and needs no import — so it should not block the
-- method.
allClassMethods :: Set Text -> Set Text -> ObjCClass -> [ObjCMethod]
allClassMethods importable allKnownClasses cls =
  let explicit = classMethods cls
      explicitSels = Set.fromList
        (fmap (\m -> (methodSelector m, methodIsClass m)) explicit)
      propMethods = filter
        (\m -> not (Set.member (methodSelector m, methodIsClass m) explicitSels))
        (concatMap propertyToMethods (classProperties cls))
      filteredProps
        | Set.null importable = propMethods
        | otherwise = filter isImportable propMethods
      -- Only require importability for refs that are actual known classes.
      -- Protocol names, type parameters, etc. are not in allKnownClasses
      -- and map to RawId in the generated code — they need no import.
      isImportable m =
        let types = methodReturnType m : fmap snd (methodParams m)
            refs = foldMap referencedClasses types
            classRefs = Set.intersection refs allKnownClasses
        in all (`Set.member` importable) (Set.toList classRefs)
      combined = explicit ++ filteredProps
      deduped = dedupBy (\m -> (methodSelector m, methodIsClass m))
                        Set.empty combined
      dedupBy _ _ [] = []
      dedupBy keyFn seen (m:ms) =
        let k = keyFn m
        in if Set.member k seen
           then dedupBy keyFn seen ms
           else m : dedupBy keyFn (Set.insert k seen) ms
  in deduped

-- ---------------------------------------------------------------------------
-- Deduplication
-- ---------------------------------------------------------------------------

-- | Deduplicate methods by selector (first occurrence wins).
dedupBySelector :: [ObjCMethod] -> [ObjCMethod]
dedupBySelector = go Set.empty
  where
    go _ [] = []
    go seen (m:ms) =
      let k = methodSelector m
      in if Set.member k seen
         then go seen ms
         else m : go (Set.insert k seen) ms

-- | Deduplicate methods by their generated Haskell name.
dedupByHsName :: ObjCClass -> [ObjCMethod] -> [ObjCMethod]
dedupByHsName cls methods =
  let instNames = instanceMethodNameSet cls methods
  in go instNames Set.empty methods
  where
    go _ _ [] = []
    go instNames seen (m:ms) =
      let hn = methodHaskellName cls instNames m
      in if Set.member hn seen
         then go instNames seen ms
         else m : go instNames (Set.insert hn seen) ms

-- ---------------------------------------------------------------------------
-- Struct return detection
-- ---------------------------------------------------------------------------

-- | Check if a return type is a struct (needs sendMsgStret).
isStructReturn :: ObjCType -> Bool
isStructReturn (ObjCStruct _)          = True
isStructReturn (ObjCQualified _ inner) = isStructReturn inner
isStructReturn _                       = False

-- ---------------------------------------------------------------------------
-- Nullability stripping
-- ---------------------------------------------------------------------------

-- | Strip nullability from an ObjC type, treating it as nonnull.
stripNullab :: ObjCType -> ObjCType
stripNullab (ObjCId name _)           = ObjCId name Nonnull
stripNullab (ObjCGeneric name args _) = ObjCGeneric name args Nonnull
stripNullab (ObjCQualified q inner)   = ObjCQualified q (stripNullab inner)
stripNullab other                     = other

-- ---------------------------------------------------------------------------
-- Ownership
-- ---------------------------------------------------------------------------

-- | Determine the ownership wrapper based on ObjC naming conventions.
ownershipWrapper :: Text -> Text
ownershipWrapper sel
  | "alloc" `T.isPrefixOf` sel        = "ownedObject"
  | "new" `T.isPrefixOf` sel          = "ownedObject"
  | "copy" `T.isPrefixOf` sel         = "ownedObject"
  | "mutableCopy" `T.isPrefixOf` sel  = "ownedObject"
  | "init" `T.isPrefixOf` sel         = "ownedObject"
  | otherwise                          = "retainedObject"

-- ---------------------------------------------------------------------------
-- Managed parameter detection
-- ---------------------------------------------------------------------------

-- | Does this ObjC parameter type map to a managed @Id@ that needs
-- @withObjCPtr@ wrapping?
isManagedObjCParam :: KnownTypes -> ObjCType -> Bool
isManagedObjCParam kt = go
  where
    classes = ktClasses kt
    go (ObjCId (Just n) _) =
      let bn = extractClassName n
      in Set.member n classes || Set.member bn classes
    go (ObjCId Nothing _) = False
    go (ObjCGeneric n _ _) =
      let bn = extractClassName n
      in n /= "id" && bn /= "id" && n /= "Class" && bn /= "Class"
         && (Set.member n classes || Set.member bn classes)
    go ObjCInstancetype = True
    go (ObjCQualified _ inner) = go inner
    go (ObjCPointer (ObjCPrimitive _ d))
      | "__kindof " `T.isPrefixOf` d =
          let clsName = extractClassName (T.drop 9 d)
          in Set.member clsName classes
    go (ObjCPrimitive _ d)
      | "__kindof " `T.isPrefixOf` d =
          let clsName = extractClassName (T.drop 9 d)
          in Set.member clsName classes
      | isObjPtrDesugared d =
          let pointee = extractClassName (T.strip (T.dropEnd 1 d))
          in Set.member pointee classes
    go _ = False

-- ---------------------------------------------------------------------------
-- Package naming
-- ---------------------------------------------------------------------------

-- | Convert a framework name to the generated Haskell package name.
--
-- >>> fwToPackageName "Foundation"
-- "apple-foundation-gen"
-- >>> fwToPackageName "CoreData"
-- "apple-coredata-gen"
fwToPackageName :: Text -> Text
fwToPackageName fw = "apple-" <> T.toLower fw <> "-gen"
