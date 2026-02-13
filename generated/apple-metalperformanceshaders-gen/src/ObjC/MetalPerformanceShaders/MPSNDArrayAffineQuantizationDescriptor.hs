{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayAffineQuantizationDescriptor
--
-- This depends on Metal.framework.
--
-- Describes an affine quantization scheme
--
-- Generated bindings for @MPSNDArrayAffineQuantizationDescriptor@.
module ObjC.MetalPerformanceShaders.MPSNDArrayAffineQuantizationDescriptor
  ( MPSNDArrayAffineQuantizationDescriptor
  , IsMPSNDArrayAffineQuantizationDescriptor(..)
  , init_
  , initWithDataType_hasZeroPoint_hasMinValue
  , hasZeroPoint
  , setHasZeroPoint
  , hasMinValue
  , setHasMinValue
  , hasMinValueSelector
  , hasZeroPointSelector
  , initSelector
  , initWithDataType_hasZeroPoint_hasMinValueSelector
  , setHasMinValueSelector
  , setHasZeroPointSelector

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

-- | @- init@
init_ :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> IO (Id MPSNDArrayAffineQuantizationDescriptor)
init_ mpsndArrayAffineQuantizationDescriptor =
  sendOwnedMessage mpsndArrayAffineQuantizationDescriptor initSelector

-- | Initializes an affine quantization descriptor.
--
-- @quantizationDataType@ — Which quantized datatype is used.
--
-- @hasZeroPoint@ — A flag indicating that a zero-point input is expected.
--
-- @hasMinValue@ — A flag indicating that a minimum value input is expected.
--
-- Returns: A new quantization descriptor.
--
-- ObjC selector: @- initWithDataType:hasZeroPoint:hasMinValue:@
initWithDataType_hasZeroPoint_hasMinValue :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> MPSDataType -> Bool -> Bool -> IO (Id MPSNDArrayAffineQuantizationDescriptor)
initWithDataType_hasZeroPoint_hasMinValue mpsndArrayAffineQuantizationDescriptor quantizationDataType hasZeroPoint hasMinValue =
  sendOwnedMessage mpsndArrayAffineQuantizationDescriptor initWithDataType_hasZeroPoint_hasMinValueSelector quantizationDataType hasZeroPoint hasMinValue

-- | hasZeroPoint
--
-- If yes then asymmetric quantization is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- hasZeroPoint@
hasZeroPoint :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> IO Bool
hasZeroPoint mpsndArrayAffineQuantizationDescriptor =
  sendMessage mpsndArrayAffineQuantizationDescriptor hasZeroPointSelector

-- | hasZeroPoint
--
-- If yes then asymmetric quantization is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- setHasZeroPoint:@
setHasZeroPoint :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> Bool -> IO ()
setHasZeroPoint mpsndArrayAffineQuantizationDescriptor value =
  sendMessage mpsndArrayAffineQuantizationDescriptor setHasZeroPointSelector value

-- | hasMinValue
--
-- If yes then offset is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- hasMinValue@
hasMinValue :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> IO Bool
hasMinValue mpsndArrayAffineQuantizationDescriptor =
  sendMessage mpsndArrayAffineQuantizationDescriptor hasMinValueSelector

-- | hasMinValue
--
-- If yes then offset is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- setHasMinValue:@
setHasMinValue :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> Bool -> IO ()
setHasMinValue mpsndArrayAffineQuantizationDescriptor value =
  sendMessage mpsndArrayAffineQuantizationDescriptor setHasMinValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSNDArrayAffineQuantizationDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataType:hasZeroPoint:hasMinValue:@
initWithDataType_hasZeroPoint_hasMinValueSelector :: Selector '[MPSDataType, Bool, Bool] (Id MPSNDArrayAffineQuantizationDescriptor)
initWithDataType_hasZeroPoint_hasMinValueSelector = mkSelector "initWithDataType:hasZeroPoint:hasMinValue:"

-- | @Selector@ for @hasZeroPoint@
hasZeroPointSelector :: Selector '[] Bool
hasZeroPointSelector = mkSelector "hasZeroPoint"

-- | @Selector@ for @setHasZeroPoint:@
setHasZeroPointSelector :: Selector '[Bool] ()
setHasZeroPointSelector = mkSelector "setHasZeroPoint:"

-- | @Selector@ for @hasMinValue@
hasMinValueSelector :: Selector '[] Bool
hasMinValueSelector = mkSelector "hasMinValue"

-- | @Selector@ for @setHasMinValue:@
setHasMinValueSelector :: Selector '[Bool] ()
setHasMinValueSelector = mkSelector "setHasMinValue:"

