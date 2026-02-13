{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPoolingAverageGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the average pooling filter.              The operation backpropagates a gradient vector using chain rule.
--
-- Average pooling forward pass is defined as:
--
-- out(x) = sum_{dx  Window(x)} in(s*x+dx) / N(x), where
--
-- the pooling window definition 'Window(x)' follows MPSCNNPooling specification,              'N(x)' is effective pooling window size in pixels as specified in MPSCNNPoolingAverage,              's' is the pixel stride and in() is the source input image.
--
-- Hence the partial derivative of the output value wrt. to the input value needed in the              gradient backpropagation in MPSCNNPoolingGradient is:
--
-- d out(x)/d in(y) = sum_{dx  Window(x)} delta_{s*x+dx, y} / N(x), where
--
-- delta_{x,y} is the Kronecker delta symbol for which
--
-- delta_{x,y} =  {  1, when x == y                                 {  0, otherwise.
--
-- In practice this means that the gradient value for the destination image at pixel 'x' is              the sum over these contributions coming from all pooling windows that contribute              to the average pooling computation in the forward pass, multiplied by the input              gradient value in the source area of the corresponding pooling window.
--
-- Note: As average pooling is a linear operation of its inputs, the gradient does not              depend at all on the original input values, but the original input image size is needed              so that we know the limits where the input values seize to exist to inhibit accumulation              of gradient values for those pixels. Therefore, as secondary input, any correctly sized              image will produce correct results for the gradient backpropagation and hence it is              recommended to use a temporary image of correct size (see MPSTemporaryImage) for the              secondary source image parameter.
--
-- Generated bindings for @MPSCNNPoolingAverageGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingAverageGradient
  ( MPSCNNPoolingAverageGradient
  , IsMPSCNNPoolingAverageGradient(..)
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , zeroPadSizeX
  , setZeroPadSizeX
  , zeroPadSizeY
  , setZeroPadSizeY
  , initWithCoder_deviceSelector
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
  , setZeroPadSizeXSelector
  , setZeroPadSizeYSelector
  , zeroPadSizeXSelector
  , zeroPadSizeYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a gradient average pooling filter
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
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPoolingAverageGradient mpscnnPoolingAverageGradient => mpscnnPoolingAverageGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingAverageGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingAverageGradient device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnPoolingAverageGradient initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight strideInPixelsX strideInPixelsY

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPoolingAverageGradient
--
-- @device@ — The MTLDevice on which to make the MPSCNNPoolingAverageGradient
--
-- Returns: A new MPSCNNPoolingAverageGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNPoolingAverageGradient mpscnnPoolingAverageGradient, IsNSCoder aDecoder) => mpscnnPoolingAverageGradient -> aDecoder -> RawId -> IO (Id MPSCNNPoolingAverageGradient)
initWithCoder_device mpscnnPoolingAverageGradient aDecoder device =
  sendOwnedMessage mpscnnPoolingAverageGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | zeroPadSizeX
--
-- How much zero padding to apply to both left and right borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- zeroPadSizeX@
zeroPadSizeX :: IsMPSCNNPoolingAverageGradient mpscnnPoolingAverageGradient => mpscnnPoolingAverageGradient -> IO CULong
zeroPadSizeX mpscnnPoolingAverageGradient =
  sendMessage mpscnnPoolingAverageGradient zeroPadSizeXSelector

-- | zeroPadSizeX
--
-- How much zero padding to apply to both left and right borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- setZeroPadSizeX:@
setZeroPadSizeX :: IsMPSCNNPoolingAverageGradient mpscnnPoolingAverageGradient => mpscnnPoolingAverageGradient -> CULong -> IO ()
setZeroPadSizeX mpscnnPoolingAverageGradient value =
  sendMessage mpscnnPoolingAverageGradient setZeroPadSizeXSelector value

-- | zeroPadSizeY
--
-- How much zero padding to apply to both top and bottom borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- zeroPadSizeY@
zeroPadSizeY :: IsMPSCNNPoolingAverageGradient mpscnnPoolingAverageGradient => mpscnnPoolingAverageGradient -> IO CULong
zeroPadSizeY mpscnnPoolingAverageGradient =
  sendMessage mpscnnPoolingAverageGradient zeroPadSizeYSelector

-- | zeroPadSizeY
--
-- How much zero padding to apply to both top and bottom borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- setZeroPadSizeY:@
setZeroPadSizeY :: IsMPSCNNPoolingAverageGradient mpscnnPoolingAverageGradient => mpscnnPoolingAverageGradient -> CULong -> IO ()
setZeroPadSizeY mpscnnPoolingAverageGradient value =
  sendMessage mpscnnPoolingAverageGradient setZeroPadSizeYSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong] (Id MPSCNNPoolingAverageGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNPoolingAverageGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @zeroPadSizeX@
zeroPadSizeXSelector :: Selector '[] CULong
zeroPadSizeXSelector = mkSelector "zeroPadSizeX"

-- | @Selector@ for @setZeroPadSizeX:@
setZeroPadSizeXSelector :: Selector '[CULong] ()
setZeroPadSizeXSelector = mkSelector "setZeroPadSizeX:"

-- | @Selector@ for @zeroPadSizeY@
zeroPadSizeYSelector :: Selector '[] CULong
zeroPadSizeYSelector = mkSelector "zeroPadSizeY"

-- | @Selector@ for @setZeroPadSizeY:@
setZeroPadSizeYSelector :: Selector '[CULong] ()
setZeroPadSizeYSelector = mkSelector "setZeroPadSizeY:"

