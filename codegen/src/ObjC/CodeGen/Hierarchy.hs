{-# LANGUAGE OverloadedStrings #-}

-- | Build the class hierarchy graph from parsed AST declarations.
--
-- This module:
--
-- 1. Merges interface declarations with their category extensions
-- 2. Builds the superclass DAG
-- 3. Computes transitive ancestor sets and protocol conformance
-- 4. Topologically sorts classes (parents before children)
-- 5. Builds a class-to-framework mapping from source locations
module ObjC.CodeGen.Hierarchy
  ( buildHierarchy
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import ObjC.CodeGen.IR
import ObjC.CodeGen.ClangAST (ParsedAST(..))

-- | Build the complete class hierarchy from a parsed AST.
buildHierarchy :: ParsedAST -> ClassHierarchy
buildHierarchy ast = ClassHierarchy
  { hierarchyClasses       = mergedClasses
  , hierarchyProtocols     = protoMap
  , hierarchyParentOf      = parentMap
  , hierarchyProtoConforms = directProtos
  , hierarchyTopoOrder     = topoSorted
  , hierarchyAncestors     = ancestorMap
  , hierarchyAllProtos     = allProtosMap
  , hierarchyFrameworks    = fwMap
  , hierarchyStructs       = structMap
  , hierarchyEnums         = enumMap
  }
  where
    -- Step 1: Merge interfaces and categories into ObjCClass
    mergedClasses = mergeDecls (parsedInterfaces ast) (parsedCategories ast)

    -- Step 2: Build protocol map
    protoMap = Map.fromList
      [ (protoDeclName p, p) | p <- parsedProtocols ast ]

    -- Step 3: Build parent map (child -> direct superclass)
    parentMap = Map.fromList
      [ (className cls, super)
      | cls <- Map.elems mergedClasses
      , Just super <- [classSuperclass cls]
      ]

    -- Step 4: Build direct protocol conformance map
    directProtos = Map.fromList
      [ (className cls, Set.fromList (classProtocols cls))
      | cls <- Map.elems mergedClasses
      ]

    -- Step 5: Topological sort (parents before children)
    topoSorted = topoSort mergedClasses parentMap

    -- Step 6: Compute transitive ancestors
    ancestorMap = computeAncestors topoSorted parentMap

    -- Step 7: Compute all protocols (direct + inherited)
    allProtosMap = computeAllProtos topoSorted directProtos ancestorMap

    -- Step 8: Build class -> framework map
    fwMap = Map.fromList
      [ (className cls, fw)
      | cls <- Map.elems mergedClasses
      , Just fw <- [classFramework cls]
      ]

    -- Step 9: Build struct typedef name -> StructDef map
    structMap = Map.fromList
      [ (structTypedefName sd, sd)
      | sd <- parsedStructDefs ast
      ]

    -- Step 10: Build enum name -> EnumDef map
    enumMap = Map.fromList
      [ (enumName ed, ed)
      | ed <- parsedEnumDefs ast
      ]

-- ---------------------------------------------------------------------------
-- Merging interfaces and categories
-- ---------------------------------------------------------------------------

-- | Merge interface declarations with their category extensions into
-- a single 'ObjCClass' per class name.
--
-- Multiple interface declarations for the same name (e.g., forward decls
-- followed by the full definition) are merged by taking the one with the
-- most information (has superclass, has methods).
mergeDecls :: [ObjCInterface] -> [ObjCCategory] -> Map Text ObjCClass
mergeDecls ifaces cats = Map.mapWithKey addCategories baseMap
  where
    -- Build base map from interfaces, preferring definitions over forward decls
    baseMap :: Map Text ObjCClass
    baseMap = foldl mergeIface Map.empty ifaces

    mergeIface :: Map Text ObjCClass -> ObjCInterface -> Map Text ObjCClass
    mergeIface m iface =
      let name = ifaceName iface
          cls  = ifaceToClass iface
      in Map.insertWith pickBetter name cls m

    -- When merging duplicate interface declarations, prefer the one with
    -- more information (has a superclass, has methods, has framework, has doc).
    -- When the old definition wins overall but lacks a doc, carry over the
    -- doc from the new one.
    pickBetter :: ObjCClass -> ObjCClass -> ObjCClass
    pickBetter new old
      | classSuperclass new /= Nothing && classSuperclass old == Nothing = carryDoc new old
      | not (null (classMethods new)) && null (classMethods old) = carryDoc new old
      | classFramework new /= Nothing && classFramework old == Nothing = carryDoc new old
      | otherwise = carryDoc old new

    -- | Copy the doc from @donor@ when @winner@ has none.
    carryDoc :: ObjCClass -> ObjCClass -> ObjCClass
    carryDoc winner donor = case classDoc winner of
      Just _  -> winner
      Nothing -> winner { classDoc = classDoc donor }

    ifaceToClass :: ObjCInterface -> ObjCClass
    ifaceToClass iface = ObjCClass
      { className       = ifaceName iface
      , classSuperclass = ifaceSuperclass iface
      , classTypeParams = ifaceTypeParams iface
      , classProtocols  = ifaceProtocols iface
      , classMethods    = ifaceMethods iface
      , classProperties = ifaceProperties iface
      , classFramework  = ifaceFramework iface
      , classDoc        = ifaceDoc iface
      }

    -- Group categories by class name
    catsByClass :: Map Text [ObjCCategory]
    catsByClass = foldl (\m cat -> Map.insertWith (++) (catClassName cat) [cat] m)
                        Map.empty cats

    -- Add category methods/properties/protocols to each class
    addCategories :: Text -> ObjCClass -> ObjCClass
    addCategories name cls =
      let catList = fromMaybe [] (Map.lookup name catsByClass)
          extraMethods = concatMap catMethods catList
          extraProps   = concatMap catProperties catList
          extraProtos  = concatMap catProtocols catList
      in cls
        { classMethods    = classMethods cls ++ extraMethods
        , classProperties = classProperties cls ++ extraProps
        , classProtocols  = classProtocols cls ++ extraProtos
        }

-- ---------------------------------------------------------------------------
-- Topological sort
-- ---------------------------------------------------------------------------

-- | Topological sort of classes: parents before children.
--
-- Classes with no known superclass (or whose superclass is not in the map)
-- come first.
topoSort :: Map Text ObjCClass -> Map Text Text -> [Text]
topoSort classes parentMap = go Set.empty [] roots
  where
    allNames = Map.keysSet classes

    -- Children map: parent -> set of children
    childrenOf :: Map Text (Set Text)
    childrenOf = Map.foldlWithKey'
      (\m child parent -> Map.insertWith Set.union parent (Set.singleton child) m)
      Map.empty parentMap

    -- Root classes: no parent, or parent not in our class set
    roots :: [Text]
    roots = filter isRoot (Map.keys classes)

    isRoot :: Text -> Bool
    isRoot name = case Map.lookup name parentMap of
      Nothing     -> True
      Just parent -> not (Set.member parent allNames)

    go :: Set Text -> [Text] -> [Text] -> [Text]
    go _ acc [] = acc
    go visited acc (name : rest)
      | Set.member name visited = go visited acc rest
      | otherwise =
          let visited' = Set.insert name visited
              children = Set.toList (fromMaybe Set.empty (Map.lookup name childrenOf))
              acc'     = acc ++ [name]
          in go visited' acc' (rest ++ children)

-- ---------------------------------------------------------------------------
-- Transitive ancestors
-- ---------------------------------------------------------------------------

-- | Compute the transitive set of ancestors for each class.
--
-- Processes classes in topological order so that parent ancestors are
-- computed before children.
computeAncestors :: [Text] -> Map Text Text -> Map Text (Set Text)
computeAncestors topoOrder parentMap = foldl addAncestors Map.empty topoOrder
  where
    addAncestors :: Map Text (Set Text) -> Text -> Map Text (Set Text)
    addAncestors acc name =
      let ancestors = case Map.lookup name parentMap of
            Nothing     -> Set.empty
            Just parent ->
              Set.insert parent (fromMaybe Set.empty (Map.lookup parent acc))
      in Map.insert name ancestors acc

-- ---------------------------------------------------------------------------
-- Transitive protocol conformance
-- ---------------------------------------------------------------------------

-- | Compute all protocols for each class (direct + inherited from superclasses).
computeAllProtos :: [Text] -> Map Text (Set Text) -> Map Text (Set Text)
                 -> Map Text (Set Text)
computeAllProtos topoOrder directProtos ancestorMap =
  foldl addProtos Map.empty topoOrder
  where
    addProtos :: Map Text (Set Text) -> Text -> Map Text (Set Text)
    addProtos acc name =
      let direct    = fromMaybe Set.empty (Map.lookup name directProtos)
          ancestors = fromMaybe Set.empty (Map.lookup name ancestorMap)
          -- Collect protocols from all ancestors
          inherited = Set.foldl'
            (\ps anc -> Set.union ps (fromMaybe Set.empty (Map.lookup anc acc)))
            Set.empty ancestors
      in Map.insert name (Set.union direct inherited) acc
