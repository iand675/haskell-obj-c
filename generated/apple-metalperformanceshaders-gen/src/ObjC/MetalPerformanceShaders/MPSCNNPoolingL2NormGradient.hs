{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPoolingL2NormGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the L2-Norm pooling filter.              The operation backpropagates a gradient vector using chain rule.
--
-- L2-Norm pooling forward pass is defined as:
--
-- out(x) = sqrt( sum_{dx  Window(x)} in(s*x+dx) * in(s*x+dx) ), where
--
-- the pooling window definition 'Window(x)' follows MPSCNNPooling specification and              's' is the pixel stride and in() is the source input image.
--
-- Hence the partial derivative of the output value wrt. to the input value needed in the              gradient backpropagation in MPSCNNPoolingGradient is:
--
-- d out(x)/d in(y) = sum_{dx  Window(x)} delta_{s*x+dx, y} in(s*x+dx) / out(x), where
--
-- delta_{x,y} is the Kronecker delta symbol for which
--
-- delta_{x,y} =  {  1, when x == y                                 {  0, otherwise,              and out(x) is the L2-norm pooling value at point 'x' defined above.
--
-- Generated bindings for @MPSCNNPoolingL2NormGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingL2NormGradient
  ( MPSCNNPoolingL2NormGradient
  , IsMPSCNNPoolingL2NormGradient(..)
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , initWithCoder_deviceSelector
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

-- | Initialize a gradient L2-norm pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- @strideInPixelsX@ — The input stride (upsampling factor) in the x dimension.
--
-- @strideInPixelsY@ — The input stride (upsampling factor) in the y dimension.
--
-- Returns: A valid MPSCNNPoolingL2NormGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPoolingL2NormGradient mpscnnPoolingL2NormGradient => mpscnnPoolingL2NormGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingL2NormGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingL2NormGradient device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnPoolingL2NormGradient initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight strideInPixelsX strideInPixelsY

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPoolingL2NormGradient
--
-- @device@ — The MTLDevice on which to make the MPSCNNPoolingL2NormGradient
--
-- Returns: A new MPSCNNPoolingL2NormGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNPoolingL2NormGradient mpscnnPoolingL2NormGradient, IsNSCoder aDecoder) => mpscnnPoolingL2NormGradient -> aDecoder -> RawId -> IO (Id MPSCNNPoolingL2NormGradient)
initWithCoder_device mpscnnPoolingL2NormGradient aDecoder device =
  sendOwnedMessage mpscnnPoolingL2NormGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong] (Id MPSCNNPoolingL2NormGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNPoolingL2NormGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

