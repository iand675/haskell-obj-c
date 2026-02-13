{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayQuantizationDescriptor
--
-- This depends on Metal.framework.
--
-- Common methods for quantization descriptors
--
-- Generated bindings for @MPSNDArrayQuantizationDescriptor@.
module ObjC.MetalPerformanceShaders.MPSNDArrayQuantizationDescriptor
  ( MPSNDArrayQuantizationDescriptor
  , IsMPSNDArrayQuantizationDescriptor(..)
  , quantizationDataType
  , quantizationScheme
  , quantizationDataTypeSelector
  , quantizationSchemeSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8
  , MPSNDArrayQuantizationScheme(MPSNDArrayQuantizationScheme)
  , pattern MPSNDArrayQuantizationTypeNone
  , pattern MPSNDArrayQuantizationTypeAffine
  , pattern MPSNDArrayQuantizationTypeLUT

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | quantizationDataType
--
-- The datatype to use with quantization - the default is MPSDataTypeUint8
--
-- ObjC selector: @- quantizationDataType@
quantizationDataType :: IsMPSNDArrayQuantizationDescriptor mpsndArrayQuantizationDescriptor => mpsndArrayQuantizationDescriptor -> IO MPSDataType
quantizationDataType mpsndArrayQuantizationDescriptor =
  sendMessage mpsndArrayQuantizationDescriptor quantizationDataTypeSelector

-- | quantizationScheme
--
-- The quantization scheme for this descriptor. The default is MPSNDArrayQuantizationTypeNone.
--
-- ObjC selector: @- quantizationScheme@
quantizationScheme :: IsMPSNDArrayQuantizationDescriptor mpsndArrayQuantizationDescriptor => mpsndArrayQuantizationDescriptor -> IO MPSNDArrayQuantizationScheme
quantizationScheme mpsndArrayQuantizationDescriptor =
  sendMessage mpsndArrayQuantizationDescriptor quantizationSchemeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quantizationDataType@
quantizationDataTypeSelector :: Selector '[] MPSDataType
quantizationDataTypeSelector = mkSelector "quantizationDataType"

-- | @Selector@ for @quantizationScheme@
quantizationSchemeSelector :: Selector '[] MPSNDArrayQuantizationScheme
quantizationSchemeSelector = mkSelector "quantizationScheme"

