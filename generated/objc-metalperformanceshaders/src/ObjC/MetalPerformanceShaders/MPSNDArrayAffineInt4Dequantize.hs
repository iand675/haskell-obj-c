{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayAffineInt4Dequantize
--
-- This depends on Metal.framework.
--
-- A kernel which dequantizes an input with affine quantization scheme.
--
-- The kernel works with 2-4 inputs, order of inputs: 1) quantized input, 2) scale, 3) zeropoint, 4) minValue
--
-- Generated bindings for @MPSNDArrayAffineInt4Dequantize@.
module ObjC.MetalPerformanceShaders.MPSNDArrayAffineInt4Dequantize
  ( MPSNDArrayAffineInt4Dequantize
  , IsMPSNDArrayAffineInt4Dequantize(..)
  , initWithDevice
  , initWithDevice_quantizationDescriptor
  , initWithDevice_sourceCount
  , initWithDeviceSelector
  , initWithDevice_quantizationDescriptorSelector
  , initWithDevice_sourceCountSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayAffineInt4Dequantize mpsndArrayAffineInt4Dequantize => mpsndArrayAffineInt4Dequantize -> RawId -> IO (Id MPSNDArrayAffineInt4Dequantize)
initWithDevice mpsndArrayAffineInt4Dequantize  device =
  sendMsg mpsndArrayAffineInt4Dequantize (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a kernel for 4-bit affine dequantization.
--
-- @device@ — The Metal device to be used with this kernel.
--
-- @quantizationDescriptor@ — Describes the quantization scheme.
--
-- Returns: A new vector LUT dequantization kernel.
--
-- ObjC selector: @- initWithDevice:quantizationDescriptor:@
initWithDevice_quantizationDescriptor :: (IsMPSNDArrayAffineInt4Dequantize mpsndArrayAffineInt4Dequantize, IsMPSNDArrayAffineQuantizationDescriptor quantizationDescriptor) => mpsndArrayAffineInt4Dequantize -> RawId -> quantizationDescriptor -> IO (Id MPSNDArrayAffineInt4Dequantize)
initWithDevice_quantizationDescriptor mpsndArrayAffineInt4Dequantize  device quantizationDescriptor =
withObjCPtr quantizationDescriptor $ \raw_quantizationDescriptor ->
    sendMsg mpsndArrayAffineInt4Dequantize (mkSelector "initWithDevice:quantizationDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_quantizationDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayAffineInt4Dequantize mpsndArrayAffineInt4Dequantize => mpsndArrayAffineInt4Dequantize -> RawId -> CULong -> IO (Id MPSNDArrayAffineInt4Dequantize)
initWithDevice_sourceCount mpsndArrayAffineInt4Dequantize  device sourceCount =
  sendMsg mpsndArrayAffineInt4Dequantize (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral sourceCount)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:quantizationDescriptor:@
initWithDevice_quantizationDescriptorSelector :: Selector
initWithDevice_quantizationDescriptorSelector = mkSelector "initWithDevice:quantizationDescriptor:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

