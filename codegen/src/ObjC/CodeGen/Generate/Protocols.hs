{-# LANGUAGE OverloadedStrings #-}

-- | Protocol module generation.
--
-- For each ObjC protocol, generates:
--
-- * A type class encoding the required methods
-- * Default implementations for optional methods
-- * A \"conformance\" instance for each class that adopts the protocol
module ObjC.CodeGen.Generate.Protocols
  ( generateProtocolModule
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap
import ObjC.CodeGen.Generate.Types (GeneratedModule(..))
import ObjC.CodeGen.Generate.Naming
import ObjC.CodeGen.Generate.Shared
import ObjC.CodeGen.Generate.Method (generateMethodBody, MethodBodyConfig(..), ReceiverKind(..))

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Generate a module for a protocol.
--
-- If the protocol has no supported methods, returns 'Nothing'.
generateProtocolModule
  :: Map Text Text -> ClassHierarchy -> KnownTypes -> Text -> Set Text
  -> ObjCProtocolDecl -> Maybe GeneratedModule
generateProtocolModule fwMap hierarchy allKnown framework depFws proto
  | null requiredMethods && null optionalMethods = Nothing
  | otherwise = Just GeneratedModule
      { genModuleName    = modName
      , genModuleContent = T.unlines content
      }
  where
    pName = protoDeclName proto
    modName = "ObjC." <> framework <> ".Protocol." <> pName

    importable = Set.fromList
      [ c | (c, fw) <- Map.toList fwMap
      , fw == framework || ("objc-" <> T.toLower fw) `Set.member` depFws
      ]

    requiredMethods = filter (isMethodSupported allKnown) (protoDeclRequired proto)
    optionalMethods = filter (isMethodSupported allKnown) (protoDeclOptional proto)

    content = concat
      [ moduleHeader pName modName
      , [""]
      , moduleImports framework
      , [""]
      , protocolTypeClass allKnown pName requiredMethods optionalMethods
      , concatMap (\m -> [""] ++ optionalMethodDefault allKnown pName m) optionalMethods
      , [""]
      , adoptingInstances allKnown hierarchy pName importable proto
      ]

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

moduleHeader :: Text -> Text -> [Text]
moduleHeader pName modName =
  [ "{-# LANGUAGE TypeApplications #-}"
  , "{-# LANGUAGE ScopedTypeVariables #-}"
  , "{-# LANGUAGE FlexibleContexts #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE DefaultSignatures #-}"
  , ""
  , "-- | Generated bindings for protocol @" <> pName <> "@."
  , "module " <> modName <> " where"
  ]

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

moduleImports :: Text -> [Text]
moduleImports framework =
  [ "import Foreign.Ptr (Ptr, castPtr)"
  , "import Foreign.LibFFI"
  , "import Foreign.C.Types"
  , "import Data.Coerce (coerce)"
  , ""
  , "import ObjC.Runtime.Types"
  , "import ObjC.Runtime.MsgSend (sendMsg)"
  , "import ObjC.Runtime.Selector (mkSelector)"
  , "import ObjC." <> framework <> ".Internal.Classes"
  ]

-- ---------------------------------------------------------------------------
-- Protocol type class
-- ---------------------------------------------------------------------------

protocolTypeClass :: KnownTypes -> Text -> [ObjCMethod] -> [ObjCMethod] -> [Text]
protocolTypeClass known pName reqMethods optMethods =
  [ "class (IsObjCObject a) => Protocol" <> pName <> " a where" ]
  ++ concatMap (requiredMethodSig known) reqMethods
  ++ concatMap (optionalMethodSig known) optMethods

requiredMethodSig :: KnownTypes -> ObjCMethod -> [Text]
requiredMethodSig known method =
  let sel = methodSelector method
      hsName = selectorHaskellName sel
      retTy = mapType known (stripNullab (methodReturnType method))
      paramTys = fmap (\(_, pty) -> mapType known (stripNullab pty)) (methodParams method)
      allTys = [HsTyCon "a"] ++ paramTys ++ [HsTyIO retTy]
      sig = T.intercalate " -> " (fmap hsTypeToText allTys)
  in [ "  " <> hsName <> " :: " <> sig ]

optionalMethodSig :: KnownTypes -> ObjCMethod -> [Text]
optionalMethodSig known method =
  requiredMethodSig known method
  ++ ["  -- (optional, has default implementation)"]

-- ---------------------------------------------------------------------------
-- Optional method default implementations
-- ---------------------------------------------------------------------------

optionalMethodDefault :: KnownTypes -> Text -> ObjCMethod -> [Text]
optionalMethodDefault _known _pName _method = []
  -- TODO: Generate default implementations that use performSelector:
  -- or raise an error on invocation. Left as stub for now since
  -- optional protocol methods often have no meaningful default.

-- ---------------------------------------------------------------------------
-- Adopting instances
-- ---------------------------------------------------------------------------

adoptingInstances :: KnownTypes -> ClassHierarchy -> Text -> Set Text -> ObjCProtocolDecl -> [Text]
adoptingInstances known hierarchy pName importable proto =
  let allMethods = protoDeclRequired proto ++ protoDeclOptional proto
      adopters = filter (\(name, _) -> Set.member name importable) $
        findAdoptingClasses hierarchy pName
  in concatMap (adoptingInstance known pName allMethods) adopters

findAdoptingClasses :: ClassHierarchy -> Text -> [(Text, ObjCClass)]
findAdoptingClasses hierarchy pName =
  [ (name, cls)
  | (name, cls) <- Map.toList (hierarchyClasses hierarchy)
  , pName `elem` classProtocols cls
  ]

adoptingInstance :: KnownTypes -> Text -> [ObjCMethod] -> (Text, ObjCClass) -> [Text]
adoptingInstance known pName methods (clsName, _cls) =
  [ ""
  , "instance Protocol" <> pName <> " (Id " <> clsName <> ") where"
  ] ++ concatMap (instanceMethodImpl known clsName) methods
  where
    instanceMethodImpl :: KnownTypes -> Text -> ObjCMethod -> [Text]
    instanceMethodImpl kt cName method =
      let sel = methodSelector method
          hsName = selectorHaskellName sel
          params = methodParams method
          selfVar = "self"
          paramNames = fmap (\(pn, _) -> sanitizeParamName pn) params
          argPattern = T.unwords (selfVar : paramNames)
          -- Build a placeholder ObjCClass just for the method body generator
          placeholderCls = ObjCClass
            { className = cName
            , classSuperclass = Nothing
            , classTypeParams = []
            , classProtocols = []
            , classDoc = Nothing
            , classFramework = Nothing
            , classMethods = []
            , classProperties = []
            }
          body = generateMethodBody kt placeholderCls
            MethodBodyConfig
              { mbcReceiverKind = InstanceReceiver selfVar
              , mbcBaseIndent   = 4
              }
            method
      in [ "  " <> hsName <> " " <> argPattern <> " =" ]
         ++ fmap ("    " <>) body
