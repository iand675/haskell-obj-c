{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDilatedPoolingMax
--
-- This depends on Metal.framework
--
-- Specifies the dilated max pooling filter.  For each pixel, returns the maximum value of pixels              in the kernelWidth x kernelHeight filter region by step size dilationRateX x dilationRateY.
--
-- Generated bindings for @MPSCNNDilatedPoolingMax@.
module ObjC.MetalPerformanceShaders.MPSCNNDilatedPoolingMax
  ( MPSCNNDilatedPoolingMax
  , IsMPSCNNDilatedPoolingMax(..)
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , dilationRateX
  , dilationRateY
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector
  , initWithCoder_deviceSelector
  , dilationRateXSelector
  , dilationRateYSelector


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

-- | Initialize a MPSCNNDilatedPoolingMax pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- @dilationRateX@ — The dilation rate in the x dimension.
--
-- @dilationRateY@ — The dilation rate in the y dimension.
--
-- @strideInPixelsX@ — The output stride (downsampling factor) in the x dimension.
--
-- @strideInPixelsY@ — The output stride (downsampling factor) in the y dimension.
--
-- Returns: A valid MPSCNNDilatedPoolingMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY :: IsMPSCNNDilatedPoolingMax mpscnnDilatedPoolingMax => mpscnnDilatedPoolingMax -> RawId -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMax)
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY mpscnnDilatedPoolingMax  device kernelWidth kernelHeight dilationRateX dilationRateY strideInPixelsX strideInPixelsY =
  sendMsg mpscnnDilatedPoolingMax (mkSelector "initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral dilationRateX), argCULong (fromIntegral dilationRateY), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel.h initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNDilatedPoolingMax
--
-- @device@ — The MTLDevice on which to make the MPSCNNDilatedPoolingMax
--
-- Returns: A new MPSCNNDilatedPoolingMax object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDilatedPoolingMax mpscnnDilatedPoolingMax, IsNSCoder aDecoder) => mpscnnDilatedPoolingMax -> aDecoder -> RawId -> IO (Id MPSCNNDilatedPoolingMax)
initWithCoder_device mpscnnDilatedPoolingMax  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnDilatedPoolingMax (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | dilationRateX
--
-- dilationRateX for accessing the image passed in as source
--
-- ObjC selector: @- dilationRateX@
dilationRateX :: IsMPSCNNDilatedPoolingMax mpscnnDilatedPoolingMax => mpscnnDilatedPoolingMax -> IO CULong
dilationRateX mpscnnDilatedPoolingMax  =
  sendMsg mpscnnDilatedPoolingMax (mkSelector "dilationRateX") retCULong []

-- | dilationRateY
--
-- dilationRateY for accessing the image passed in as source
--
-- ObjC selector: @- dilationRateY@
dilationRateY :: IsMPSCNNDilatedPoolingMax mpscnnDilatedPoolingMax => mpscnnDilatedPoolingMax -> IO CULong
dilationRateY mpscnnDilatedPoolingMax  =
  sendMsg mpscnnDilatedPoolingMax (mkSelector "dilationRateY") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector
dilationRateYSelector = mkSelector "dilationRateY"

