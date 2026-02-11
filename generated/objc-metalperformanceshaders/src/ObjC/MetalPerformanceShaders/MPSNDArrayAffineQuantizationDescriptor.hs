{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithDataType_hasZeroPoint_hasMinValueSelector
  , hasZeroPointSelector
  , setHasZeroPointSelector
  , hasMinValueSelector
  , setHasMinValueSelector

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

-- | @- init@
init_ :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> IO (Id MPSNDArrayAffineQuantizationDescriptor)
init_ mpsndArrayAffineQuantizationDescriptor  =
  sendMsg mpsndArrayAffineQuantizationDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithDataType_hasZeroPoint_hasMinValue mpsndArrayAffineQuantizationDescriptor  quantizationDataType hasZeroPoint hasMinValue =
  sendMsg mpsndArrayAffineQuantizationDescriptor (mkSelector "initWithDataType:hasZeroPoint:hasMinValue:") (retPtr retVoid) [argCUInt (coerce quantizationDataType), argCULong (if hasZeroPoint then 1 else 0), argCULong (if hasMinValue then 1 else 0)] >>= ownedObject . castPtr

-- | hasZeroPoint
--
-- If yes then asymmetric quantization is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- hasZeroPoint@
hasZeroPoint :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> IO Bool
hasZeroPoint mpsndArrayAffineQuantizationDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsndArrayAffineQuantizationDescriptor (mkSelector "hasZeroPoint") retCULong []

-- | hasZeroPoint
--
-- If yes then asymmetric quantization is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- setHasZeroPoint:@
setHasZeroPoint :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> Bool -> IO ()
setHasZeroPoint mpsndArrayAffineQuantizationDescriptor  value =
  sendMsg mpsndArrayAffineQuantizationDescriptor (mkSelector "setHasZeroPoint:") retVoid [argCULong (if value then 1 else 0)]

-- | hasMinValue
--
-- If yes then offset is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- hasMinValue@
hasMinValue :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> IO Bool
hasMinValue mpsndArrayAffineQuantizationDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsndArrayAffineQuantizationDescriptor (mkSelector "hasMinValue") retCULong []

-- | hasMinValue
--
-- If yes then offset is used. See MPSNDArrayQuantizationScheme.
--
-- ObjC selector: @- setHasMinValue:@
setHasMinValue :: IsMPSNDArrayAffineQuantizationDescriptor mpsndArrayAffineQuantizationDescriptor => mpsndArrayAffineQuantizationDescriptor -> Bool -> IO ()
setHasMinValue mpsndArrayAffineQuantizationDescriptor  value =
  sendMsg mpsndArrayAffineQuantizationDescriptor (mkSelector "setHasMinValue:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataType:hasZeroPoint:hasMinValue:@
initWithDataType_hasZeroPoint_hasMinValueSelector :: Selector
initWithDataType_hasZeroPoint_hasMinValueSelector = mkSelector "initWithDataType:hasZeroPoint:hasMinValue:"

-- | @Selector@ for @hasZeroPoint@
hasZeroPointSelector :: Selector
hasZeroPointSelector = mkSelector "hasZeroPoint"

-- | @Selector@ for @setHasZeroPoint:@
setHasZeroPointSelector :: Selector
setHasZeroPointSelector = mkSelector "setHasZeroPoint:"

-- | @Selector@ for @hasMinValue@
hasMinValueSelector :: Selector
hasMinValueSelector = mkSelector "hasMinValue"

-- | @Selector@ for @setHasMinValue:@
setHasMinValueSelector :: Selector
setHasMinValueSelector = mkSelector "setHasMinValue:"

