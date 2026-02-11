{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFeature
--
-- ICScannerFeature class is an abstract base class used to describe a scanner feature. ImageCaptureCore defines three concrete subclasses of ICScannerFeature: ICScannerFeatureEnumeration, ICScannerFeatureRange and ICScannerFeatureBoolean.
--
-- The scanner functional units may have one or more instances of these classes to allow users to choose scanner-specific settings or operations before performing a scan.
--
-- Generated bindings for @ICScannerFeature@.
module ObjC.ImageCaptureCore.ICScannerFeature
  ( ICScannerFeature
  , IsICScannerFeature(..)
  , type_
  , internalName
  , humanReadableName
  , tooltip
  , typeSelector
  , internalNameSelector
  , humanReadableNameSelector
  , tooltipSelector

  -- * Enum types
  , ICScannerFeatureType(ICScannerFeatureType)
  , pattern ICScannerFeatureTypeEnumeration
  , pattern ICScannerFeatureTypeRange
  , pattern ICScannerFeatureTypeBoolean
  , pattern ICScannerFeatureTypeTemplate

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | type
--
-- ￼Scanner feature type.
--
-- ObjC selector: @- type@
type_ :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO ICScannerFeatureType
type_ icScannerFeature  =
  fmap (coerce :: CULong -> ICScannerFeatureType) $ sendMsg icScannerFeature (mkSelector "type") retCULong []

-- | internalName
--
-- ￼The internal name of this feature.
--
-- ObjC selector: @- internalName@
internalName :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO (Id NSString)
internalName icScannerFeature  =
  sendMsg icScannerFeature (mkSelector "internalName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | humanReadableName
--
-- The human readable name of this feature.
--
-- ObjC selector: @- humanReadableName@
humanReadableName :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO (Id NSString)
humanReadableName icScannerFeature  =
  sendMsg icScannerFeature (mkSelector "humanReadableName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tooltip
--
-- ￼Tooltip text describing the feature.
--
-- ObjC selector: @- tooltip@
tooltip :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO (Id NSString)
tooltip icScannerFeature  =
  sendMsg icScannerFeature (mkSelector "tooltip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @internalName@
internalNameSelector :: Selector
internalNameSelector = mkSelector "internalName"

-- | @Selector@ for @humanReadableName@
humanReadableNameSelector :: Selector
humanReadableNameSelector = mkSelector "humanReadableName"

-- | @Selector@ for @tooltip@
tooltipSelector :: Selector
tooltipSelector = mkSelector "tooltip"

