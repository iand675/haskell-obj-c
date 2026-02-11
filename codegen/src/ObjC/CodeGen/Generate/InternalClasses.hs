{-# LANGUAGE OverloadedStrings #-}

-- | Generation of the @Internal.Classes@ module for a framework.
--
-- This module defines, for each ObjC class in the framework:
--
-- * A phantom type tag (@data NSObject@)
-- * An @IsObjCObject@ instance
-- * An @IsXxx@ type class with the superclass chain
-- * Self and ancestor instances
--
-- Centralising all type definitions in one module avoids import
-- cycles between per-class modules.
module ObjC.CodeGen.Generate.InternalClasses
  ( generateInternalClassesModule
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.Generate.Types (GeneratedModule(..))
import ObjC.CodeGen.Generate.Naming (formatHaddock)

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Generate the internal classes module for a framework.
generateInternalClassesModule
  :: Map Text Text    -- ^ class → framework
  -> ClassHierarchy
  -> Text             -- ^ framework name
  -> [Text]           -- ^ ordered class names (topological)
  -> Set Text         -- ^ dependency package names
  -> GeneratedModule
generateInternalClassesModule fwMap hierarchy framework classNames depFws =
  let modName = "ObjC." <> framework <> ".Internal.Classes"
      ownClasses = filter
        (\name -> Map.lookup name fwMap == Just framework) classNames
      crossFwModules = internalCrossFwModules hierarchy framework depFws
  in GeneratedModule
    { genModuleName    = modName
    , genModuleContent = T.unlines $ concat
        [ internalHeader modName crossFwModules
        , [""]
        , internalImportLines crossFwModules
        , concatMap (\name ->
            case Map.lookup name (hierarchyClasses hierarchy) of
              Nothing  -> []
              Just cls -> [""] ++ internalClassDecls hierarchy name cls
          ) ownClasses
        ]
    }

-- ---------------------------------------------------------------------------
-- Header and imports
-- ---------------------------------------------------------------------------

internalHeader :: Text -> [Text] -> [Text]
internalHeader modName crossFwModules =
  [ "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , ""
  , "-- | Internal module: all type tags, aliases, type classes, and"
  , "-- hierarchy instances for this framework."
  , "--"
  , "-- Exists to break import cycles between per-class modules."
  , "-- Import the per-class modules for the public API."
  ] ++ case crossFwModules of
    [] -> ["module " <> modName <> " where"]
    _  -> ["module " <> modName <> " ("]
       ++ ["    module " <> modName <> ","]
       ++ [    "    module " <> m <> ","  | m <- crossFwModules ]
       ++ ["  ) where"]

internalImportLines :: [Text] -> [Text]
internalImportLines crossFwModules =
  [ "import Data.Proxy (Proxy(..))"
  , "import ObjC.Runtime.Types"
  , "import ObjC.Runtime.Class (getRequiredClass)"
  ] ++ fmap (\m -> "import " <> m) crossFwModules

-- ---------------------------------------------------------------------------
-- Cross-framework module computation
-- ---------------------------------------------------------------------------

internalCrossFwModules :: ClassHierarchy -> Text -> Set Text -> [Text]
internalCrossFwModules hierarchy framework depFws =
  fmap (\fw -> "ObjC." <> fw <> ".Internal.Classes")
    (Set.toList externalFws)
  where
    depFwNames = Set.fromList
      [ fw
      | depPkg <- Set.toList depFws
      , let rawFw = T.drop 5 depPkg
      , fw <- lookupFrameworkName rawFw
      ]
    allFws = Set.fromList (Map.elems (hierarchyFrameworks hierarchy))
    externalFws = Set.intersection depFwNames (Set.delete framework allFws)

    lookupFrameworkName :: Text -> [Text]
    lookupFrameworkName lowerName =
      [ fw | fw <- Set.toList allFws, T.toLower fw == lowerName ]

-- ---------------------------------------------------------------------------
-- Per-class declarations
-- ---------------------------------------------------------------------------

internalClassDecls :: ClassHierarchy -> Text -> ObjCClass -> [Text]
internalClassDecls hierarchy name cls = concat
  [ [ "-- ---------- " <> name <> " ----------"
    , ""
    ]
  , phantomDoc
  , [ "data " <> name
    , ""
    ]
  , internalIsObjCObject name
  , [""]
  , internalTypeClass cls
  , [""]
  , internalSelfInstance name
  , internalAncestorInstances hierarchy name
  ]
  where
    phantomDoc = case classDoc cls of
      Just doc ->
        formatHaddock doc
        ++ ["-- ", "-- Phantom type for @" <> name <> "@."]
      Nothing ->
        ["-- | Phantom type for @" <> name <> "@."]

-- | @IsObjCObject@ instance for @Id Name@.
internalIsObjCObject :: Text -> [Text]
internalIsObjCObject name =
  [ "instance IsObjCObject (Id " <> name <> ") where"
  , "  toRawId = idToRawId"
  , "  withObjCPtr = withIdObjCPtr"
  , "  staticClass _ = getRequiredClass " <> T.pack (show (T.unpack name))
  ]

-- | Type class declaration.
internalTypeClass :: ObjCClass -> [Text]
internalTypeClass cls =
  [ "class " <> superConstraint <> "Is" <> name <> " a where"
  , "  to" <> name <> " :: a -> Id " <> name
  ]
  where
    name = className cls
    superConstraint = case classSuperclass cls of
      Nothing    -> "IsObjCObject a => "
      Just super -> "Is" <> super <> " a => "

-- | Self instance.
internalSelfInstance :: Text -> [Text]
internalSelfInstance name =
  [ "instance Is" <> name <> " (Id " <> name <> ") where"
  , "  to" <> name <> " = unsafeCastId"
  ]

-- | Ancestor instances.
internalAncestorInstances :: ClassHierarchy -> Text -> [Text]
internalAncestorInstances hierarchy name =
  let ancestors = fromMaybe Set.empty
        (Map.lookup name (hierarchyAncestors hierarchy))
  in concatMap (\anc ->
       [ ""
       , "instance Is" <> anc <> " (Id " <> name <> ") where"
       , "  to" <> anc <> " = unsafeCastId"
       ]) (Set.toList ancestors)
