{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPoolingMaxGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the max pooling filter.              The operation backpropagates a gradient vector using chain rule.
--
-- Dilated Max pooling forward pass is defined as:
--
-- out(x) = max_{dx  Window(x)} in(s*x+D*dx), where
--
-- the pooling window definition 'Window(x)' follows MPSCNNPooling specification,              's' is the pixel stride and in() is the source input image and D is the dilation factor.              For MPSCNNPoolingMaxGradient the dilationRate 'D' is one. NOTE: For even-sized pooling              windows with dilation rate greater than one the effective pooling window is centered              around s*x with non-even windows leaning towards top-left corner. For example if              kernel width = 2, dilation rate = 3, then the pooling considers positions '-2' and '+1'              relative to the pooling window center 's*x'.
--
-- Hence the partial derivative of the output value wrt. to the input value needed in the              gradient backpropagation in MPSCNNPoolingGradient is:
--
-- d out(x)/d in(y) = delta_{x_m, y}, where
--
-- delta_{x,y} is the Kronecker delta symbol (see MPSCNNPoolingAverageGradient) and x_m              is the index of the maximum value in the corresponding pooling window.
--
-- In practice this means that the gradient value for the destination image at pixel 'x' is              the sum over these contributions coming from all pooling windows that contribute              to the max pooling computation in the forward pass, multiplied by the input              gradient value in the source area of the corresponding pooling window. If there are              multiple maximal values within a single pooling window one of them is picked for the              gradient and this decision is implementation specific, which means that it can vary              between different architectures and even between different filter parameters.
--
-- Note: The gradient max pooling needs the secondary input image in order to compute              the indices of maximal values for each pooling window, but this means redundant computations.              Later we may add encode calls to MPSCNNPoolingMax that produce a state that contains the              coordinates of the maximal values to be consumed by the gradient filters.
--
-- Generated bindings for @MPSCNNPoolingMaxGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingMaxGradient
  ( MPSCNNPoolingMaxGradient
  , IsMPSCNNPoolingMaxGradient(..)
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
  , initWithCoder_deviceSelector


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

-- | Initialize a gradient max pooling filter
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
-- Returns: A valid MPSCNNPoolingGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPoolingMaxGradient mpscnnPoolingMaxGradient => mpscnnPoolingMaxGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingMaxGradient  device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendMsg mpscnnPoolingMaxGradient (mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNPoolingMaxGradient mpscnnPoolingMaxGradient, IsNSCoder aDecoder) => mpscnnPoolingMaxGradient -> aDecoder -> RawId -> IO (Id MPSCNNPoolingMaxGradient)
initWithCoder_device mpscnnPoolingMaxGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnPoolingMaxGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

