{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDilatedPoolingMaxGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the dilated max pooling filter.              For details see comments on MPSCNNPoolingMaxGradient.
--
-- Generated bindings for @MPSCNNDilatedPoolingMaxGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNDilatedPoolingMaxGradient
  ( MPSCNNDilatedPoolingMaxGradient
  , IsMPSCNNDilatedPoolingMaxGradient(..)
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , initWithCoder_deviceSelector
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSCNNDilatedPoolingMaxGradient pooling filter
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
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY :: IsMPSCNNDilatedPoolingMaxGradient mpscnnDilatedPoolingMaxGradient => mpscnnDilatedPoolingMaxGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY mpscnnDilatedPoolingMaxGradient device kernelWidth kernelHeight dilationRateX dilationRateY strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnDilatedPoolingMaxGradient initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight dilationRateX dilationRateY strideInPixelsX strideInPixelsY

-- | @- initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNDilatedPoolingMaxGradient mpscnnDilatedPoolingMaxGradient => mpscnnDilatedPoolingMaxGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnDilatedPoolingMaxGradient device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnDilatedPoolingMaxGradient initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight strideInPixelsX strideInPixelsY

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPoolingMaxGradient
--
-- @device@ — The MTLDevice on which to make the MPSCNNPoolingMaxGradient
--
-- Returns: A new MPSCNNPoolingMaxGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDilatedPoolingMaxGradient mpscnnDilatedPoolingMaxGradient, IsNSCoder aDecoder) => mpscnnDilatedPoolingMaxGradient -> aDecoder -> RawId -> IO (Id MPSCNNDilatedPoolingMaxGradient)
initWithCoder_device mpscnnDilatedPoolingMaxGradient aDecoder device =
  sendOwnedMessage mpscnnDilatedPoolingMaxGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNDilatedPoolingMaxGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

