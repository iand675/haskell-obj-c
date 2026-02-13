{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayLUTQuantizationDescriptor
--
-- This depends on Metal.framework.
--
-- Describes a lookup-table based quantization scheme
--
-- Generated bindings for @MPSNDArrayLUTQuantizationDescriptor@.
module ObjC.MetalPerformanceShaders.MPSNDArrayLUTQuantizationDescriptor
  ( MPSNDArrayLUTQuantizationDescriptor
  , IsMPSNDArrayLUTQuantizationDescriptor(..)
  , initWithDataType
  , initWithDataType_vectorAxis
  , initWithDataTypeSelector
  , initWithDataType_vectorAxisSelector

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

-- | Initializes a scalar lookup-table quantization descriptor.
--
-- @quantizationDataType@ — Which quantized datatype is used.
--
-- Returns: A new quantization descriptor.
--
-- ObjC selector: @- initWithDataType:@
initWithDataType :: IsMPSNDArrayLUTQuantizationDescriptor mpsndArrayLUTQuantizationDescriptor => mpsndArrayLUTQuantizationDescriptor -> MPSDataType -> IO (Id MPSNDArrayLUTQuantizationDescriptor)
initWithDataType mpsndArrayLUTQuantizationDescriptor quantizationDataType =
  sendOwnedMessage mpsndArrayLUTQuantizationDescriptor initWithDataTypeSelector quantizationDataType

-- | Initializes a vector lookup-table quantization descriptor.
--
-- @quantizationDataType@ — Which quantized datatype is used.
--
-- @vectorAxis@ — The quantization vector axis - this axis will receive the vector component in the destination.
--
-- Returns: A new quantization descriptor.
--
-- ObjC selector: @- initWithDataType:vectorAxis:@
initWithDataType_vectorAxis :: IsMPSNDArrayLUTQuantizationDescriptor mpsndArrayLUTQuantizationDescriptor => mpsndArrayLUTQuantizationDescriptor -> MPSDataType -> CULong -> IO (Id MPSNDArrayLUTQuantizationDescriptor)
initWithDataType_vectorAxis mpsndArrayLUTQuantizationDescriptor quantizationDataType vectorAxis =
  sendOwnedMessage mpsndArrayLUTQuantizationDescriptor initWithDataType_vectorAxisSelector quantizationDataType vectorAxis

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDataType:@
initWithDataTypeSelector :: Selector '[MPSDataType] (Id MPSNDArrayLUTQuantizationDescriptor)
initWithDataTypeSelector = mkSelector "initWithDataType:"

-- | @Selector@ for @initWithDataType:vectorAxis:@
initWithDataType_vectorAxisSelector :: Selector '[MPSDataType, CULong] (Id MPSNDArrayLUTQuantizationDescriptor)
initWithDataType_vectorAxisSelector = mkSelector "initWithDataType:vectorAxis:"

