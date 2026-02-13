{-# LANGUAGE DataKinds #-}
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
  , dilationRateXSelector
  , dilationRateYSelector
  , initWithCoder_deviceSelector
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY mpscnnDilatedPoolingMax device kernelWidth kernelHeight dilationRateX dilationRateY strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnDilatedPoolingMax initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight dilationRateX dilationRateY strideInPixelsX strideInPixelsY

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
initWithCoder_device mpscnnDilatedPoolingMax aDecoder device =
  sendOwnedMessage mpscnnDilatedPoolingMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | dilationRateX
--
-- dilationRateX for accessing the image passed in as source
--
-- ObjC selector: @- dilationRateX@
dilationRateX :: IsMPSCNNDilatedPoolingMax mpscnnDilatedPoolingMax => mpscnnDilatedPoolingMax -> IO CULong
dilationRateX mpscnnDilatedPoolingMax =
  sendMessage mpscnnDilatedPoolingMax dilationRateXSelector

-- | dilationRateY
--
-- dilationRateY for accessing the image passed in as source
--
-- ObjC selector: @- dilationRateY@
dilationRateY :: IsMPSCNNDilatedPoolingMax mpscnnDilatedPoolingMax => mpscnnDilatedPoolingMax -> IO CULong
dilationRateY mpscnnDilatedPoolingMax =
  sendMessage mpscnnDilatedPoolingMax dilationRateYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMax)
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNDilatedPoolingMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector '[] CULong
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector '[] CULong
dilationRateYSelector = mkSelector "dilationRateY"

