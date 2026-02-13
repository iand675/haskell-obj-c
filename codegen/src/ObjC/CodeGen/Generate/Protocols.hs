{-# LANGUAGE OverloadedStrings #-}

-- | Protocol module generation.
--
-- For each ObjC protocol, generates:
--
-- * A type class encoding the required methods
-- * Default implementations for optional methods
-- * A \"conformance\" instance for each class that adopts the protocol
-- * Top-level typed @Selector@ bindings
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
import ObjC.CodeGen.Generate.Method (generateMethodBody, MethodBodyConfig(..), ReceiverKind(..), selectorTypeText)

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
      , fw == framework || fwToPackageName fw `Set.member` depFws
      ]

    requiredMethods = filter (isMethodSupported allKnown) (protoDeclRequired proto)
    optionalMethods = filter (isMethodSupported allKnown) (protoDeclOptional proto)
    allMethods = requiredMethods ++ optionalMethods

    content = concat
      [ moduleHeader pName modName
      , [""]
      , moduleImports framework
      , [""]
      , protocolTypeClass allKnown pName requiredMethods optionalMethods
      , concatMap (\m -> [""] ++ optionalMethodDefault allKnown pName m) optionalMethods
      , [""]
      , adoptingInstances allKnown hierarchy pName importable proto
      , [""]
      , ["-- ---------------------------------------------------------------------------"]
      , ["-- Selectors"]
      , ["-- ---------------------------------------------------------------------------"]
      , [""]
      , protocolSelectors allKnown allMethods
      ]

-- ---------------------------------------------------------------------------
-- Protocol method naming
-- ---------------------------------------------------------------------------

-- | Compute the Haskell name for a protocol type class method.
--
-- Same logic as 'methodHaskellName' but without class/instance collision
-- handling (protocols don't have that distinction).
protoMethodName :: ObjCMethod -> Text
protoMethodName method =
  let sel = methodSelector method
      baseName = T.replace ":" "_" (T.dropWhileEnd (== ':') sel)
  in escapeReserved (lowerFirst baseName)

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

moduleHeader :: Text -> Text -> [Text]
moduleHeader pName modName =
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeApplications #-}"
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
  [ "import Foreign.Ptr (Ptr, FunPtr)"
  , "import Foreign.C.Types"
  , ""
  , "import ObjC.Runtime.Types"
  , "import ObjC.Runtime.Message (sendMessage, sendOwnedMessage)"
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
  let hsName = protoMethodName method
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
-- Selectors
-- ---------------------------------------------------------------------------

-- | Generate top-level typed @Selector@ bindings for protocol methods.
--
-- Methods returning @instancetype@ are excluded — their return type
-- varies per adopting class, so those use an inline typed @mkSelector@
-- in each adopting instance instead.
protocolSelectors :: KnownTypes -> [ObjCMethod] -> [Text]
protocolSelectors known methods =
  let -- Only generate top-level selectors for non-instancetype methods
      eligible = filter (not . isInstancetypeReturn) methods
      uniqueMethods = dedupBySel Set.empty eligible
      dedupBySel _ [] = []
      dedupBySel seen (m:ms)
        | Set.member sel seen = dedupBySel seen ms
        | otherwise           = m : dedupBySel (Set.insert sel seen) ms
        where sel = methodSelector m
  in concatMap (genProtoSel known) uniqueMethods

genProtoSel :: KnownTypes -> ObjCMethod -> [Text]
genProtoSel known method =
  let sel    = methodSelector method
      hsName = selectorHaskellName sel
      selStr = T.pack (show (T.unpack sel))
      selTy  = selectorTypeText known "()" method
      -- "()" is a placeholder class name — it won't be used since
      -- instancetype methods are filtered out above.
  in [ "-- | @Selector@ for @" <> sel <> "@"
     , hsName <> " :: " <> selTy
     , hsName <> " = mkSelector " <> selStr
     , ""
     ]

isInstancetypeReturn :: ObjCMethod -> Bool
isInstancetypeReturn method = case methodReturnType method of
  ObjCInstancetype -> True
  _                -> False

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
          hsName = protoMethodName method
          -- For instancetype methods, the return type depends on the
          -- adopting class, so we inline a typed mkSelector.
          -- For everything else, reference the top-level selector.
          selExpr
            | isInstancetypeReturn method =
                let selStr = T.pack (show (T.unpack sel))
                    selTy  = selectorTypeText kt cName method
                in "(mkSelector " <> selStr <> " :: " <> selTy <> ")"
            | otherwise = selectorHaskellName sel
          params = methodParams method
          selfVar = "self"
          paramNames = fmap (\(pn, _) -> sanitizeParamName pn) params
          argPattern = T.unwords (selfVar : paramNames)
          body = generateMethodBody
            MethodBodyConfig
              { mbcReceiverKind   = InstanceReceiver selfVar
              , mbcBaseIndent     = 4
              , mbcSelectorExpr   = selExpr
              , mbcArgExprs       = paramNames
              , mbcIsOwned        = ownershipWrapper sel == "ownedObject"
              }
      in [ "  " <> hsName <> " " <> argPattern <> " =" ]
         ++ fmap ("    " <>) body
