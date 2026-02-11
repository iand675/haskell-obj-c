{-# LANGUAGE StrictData #-}

-- | Core data types produced by the code generator.
--
-- Separated from the generation logic so that the 'CodeGen' monad
-- and every sub-module can reference them without import cycles.
module ObjC.CodeGen.Generate.Types
  ( GeneratedModule(..)
  , GeneratedPackage(..)
  , SkippedBinding(..)
  ) where

import Data.Set (Set)
import Data.Text (Text)

-- | A generated Haskell module.
data GeneratedModule = GeneratedModule
  { genModuleName    :: Text
    -- ^ Full module name (e.g., @\"ObjC.Foundation.NSString\"@).
  , genModuleContent :: Text
    -- ^ The Haskell source code.
  } deriving (Show)

-- | A generated framework package.
data GeneratedPackage = GeneratedPackage
  { gpkgName       :: Text
    -- ^ Package name (e.g., @\"objc-foundation\"@).
  , gpkgFramework  :: Text
    -- ^ Framework name (e.g., @\"Foundation\"@).
  , gpkgDeps       :: Set Text
    -- ^ Dependencies on other generated packages.
  , gpkgModules    :: [GeneratedModule]
    -- ^ Per-class modules.
  , gpkgReExport   :: Maybe GeneratedModule
    -- ^ Re-export module (Nothing when simplified names would conflict).
  , gpkgPackageYaml :: Text
    -- ^ Contents of @package.yaml@.
  , gpkgSkipped    :: [SkippedBinding]
    -- ^ Methods/fields that were skipped due to unsupported types.
  } deriving (Show)

-- | A record of something that was skipped during code generation.
data SkippedBinding = SkippedBinding
  { skipFramework :: Text
    -- ^ Framework the binding belongs to.
  , skipClass     :: Text
    -- ^ Class (or struct\/protocol) name.
  , skipSelector  :: Text
    -- ^ Method selector, property name, or field name.
  , skipReason    :: Text
    -- ^ Human-readable reason.
  } deriving (Show)
