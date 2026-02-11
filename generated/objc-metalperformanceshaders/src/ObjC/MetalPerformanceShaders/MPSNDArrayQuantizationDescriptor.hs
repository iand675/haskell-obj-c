{-# LANGUAGE PatternSynonyms #-}
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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | quantizationDataType
--
-- The datatype to use with quantization - the default is MPSDataTypeUint8
--
-- ObjC selector: @- quantizationDataType@
quantizationDataType :: IsMPSNDArrayQuantizationDescriptor mpsndArrayQuantizationDescriptor => mpsndArrayQuantizationDescriptor -> IO MPSDataType
quantizationDataType mpsndArrayQuantizationDescriptor  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsndArrayQuantizationDescriptor (mkSelector "quantizationDataType") retCUInt []

-- | quantizationScheme
--
-- The quantization scheme for this descriptor. The default is MPSNDArrayQuantizationTypeNone.
--
-- ObjC selector: @- quantizationScheme@
quantizationScheme :: IsMPSNDArrayQuantizationDescriptor mpsndArrayQuantizationDescriptor => mpsndArrayQuantizationDescriptor -> IO MPSNDArrayQuantizationScheme
quantizationScheme mpsndArrayQuantizationDescriptor  =
  fmap (coerce :: CULong -> MPSNDArrayQuantizationScheme) $ sendMsg mpsndArrayQuantizationDescriptor (mkSelector "quantizationScheme") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quantizationDataType@
quantizationDataTypeSelector :: Selector
quantizationDataTypeSelector = mkSelector "quantizationDataType"

-- | @Selector@ for @quantizationScheme@
quantizationSchemeSelector :: Selector
quantizationSchemeSelector = mkSelector "quantizationScheme"

