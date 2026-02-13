{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , humanReadableNameSelector
  , internalNameSelector
  , tooltipSelector
  , typeSelector

  -- * Enum types
  , ICScannerFeatureType(ICScannerFeatureType)
  , pattern ICScannerFeatureTypeEnumeration
  , pattern ICScannerFeatureTypeRange
  , pattern ICScannerFeatureTypeBoolean
  , pattern ICScannerFeatureTypeTemplate

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
type_ icScannerFeature =
  sendMessage icScannerFeature typeSelector

-- | internalName
--
-- ￼The internal name of this feature.
--
-- ObjC selector: @- internalName@
internalName :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO (Id NSString)
internalName icScannerFeature =
  sendMessage icScannerFeature internalNameSelector

-- | humanReadableName
--
-- The human readable name of this feature.
--
-- ObjC selector: @- humanReadableName@
humanReadableName :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO (Id NSString)
humanReadableName icScannerFeature =
  sendMessage icScannerFeature humanReadableNameSelector

-- | tooltip
--
-- ￼Tooltip text describing the feature.
--
-- ObjC selector: @- tooltip@
tooltip :: IsICScannerFeature icScannerFeature => icScannerFeature -> IO (Id NSString)
tooltip icScannerFeature =
  sendMessage icScannerFeature tooltipSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] ICScannerFeatureType
typeSelector = mkSelector "type"

-- | @Selector@ for @internalName@
internalNameSelector :: Selector '[] (Id NSString)
internalNameSelector = mkSelector "internalName"

-- | @Selector@ for @humanReadableName@
humanReadableNameSelector :: Selector '[] (Id NSString)
humanReadableNameSelector = mkSelector "humanReadableName"

-- | @Selector@ for @tooltip@
tooltipSelector :: Selector '[] (Id NSString)
tooltipSelector = mkSelector "tooltip"

