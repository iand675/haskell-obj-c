{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageConvolution
--
-- The MPSImageConvolution convolves an image with given filter of odd width and height.              The center of the kernel aligns with the MPSImageConvolution.offset. That is, the position               of the top left corner of the area covered by the kernel is given by               MPSImageConvolution.offset - {kernel_width>>1, kernel_height>>1, 0}
--
-- Optimized cases include 3x3,5x5,7x7,9x9,11x11, 1xN and Nx1. If a convolution kernel               does not fall into one of these cases but is a rank-1 matrix (a.k.a. separable)              then it will fall on an optimzied separable path. Other convolutions will execute with              full MxN complexity.
--
-- If there are multiple channels in the source image, each channel is processed independently.
--
-- Separable convolution filters may perform better when done in two passes. A convolution filter              is separable if the ratio of filter values between all rows is constant over the whole row. For              example, this edge detection filter:
--
-- -1      0       1
-- -2      0       2
-- -1      0       1
--
-- can be separated into the product of two vectors:
--
-- 1
-- 2      x    [-1  0   1]
-- 1
--
-- and consequently can be done as two, one-dimensional convolution passes back to back on the same image.               In this way, the number of multiplies (ignoring the fact that we could skip zeros here) is reduced from              3*3=9 to 3+3 = 6. There are similar savings for addition. For large filters, the savings can be profound.
--
-- Generated bindings for @MPSImageConvolution@.
module ObjC.MetalPerformanceShaders.MPSImageConvolution
  ( MPSImageConvolution
  , IsMPSImageConvolution(..)
  , initWithDevice_kernelWidth_kernelHeight_weights
  , initWithCoder_device
  , kernelHeight
  , kernelWidth
  , bias
  , setBias
  , biasSelector
  , initWithCoder_deviceSelector
  , initWithDevice_kernelWidth_kernelHeight_weightsSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , setBiasSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a convolution filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — the width of the kernel
--
-- @kernelHeight@ — the height of the kernel
--
-- @kernelWeights@ — A pointer to an array of kernelWidth * kernelHeight values to be used as the kernel.                              These are in row major order.
--
-- Returns: A valid MPSImageConvolution object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:weights:@
initWithDevice_kernelWidth_kernelHeight_weights :: IsMPSImageConvolution mpsImageConvolution => mpsImageConvolution -> RawId -> CULong -> CULong -> Const (Ptr CFloat) -> IO (Id MPSImageConvolution)
initWithDevice_kernelWidth_kernelHeight_weights mpsImageConvolution device kernelWidth kernelHeight kernelWeights =
  sendOwnedMessage mpsImageConvolution initWithDevice_kernelWidth_kernelHeight_weightsSelector device kernelWidth kernelHeight kernelWeights

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSImageConvolution mpsImageConvolution, IsNSCoder aDecoder) => mpsImageConvolution -> aDecoder -> RawId -> IO (Id MPSImageConvolution)
initWithCoder_device mpsImageConvolution aDecoder device =
  sendOwnedMessage mpsImageConvolution initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | kernelHeight
--
-- The height of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSImageConvolution mpsImageConvolution => mpsImageConvolution -> IO CULong
kernelHeight mpsImageConvolution =
  sendMessage mpsImageConvolution kernelHeightSelector

-- | kernelWidth
--
-- The width of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSImageConvolution mpsImageConvolution => mpsImageConvolution -> IO CULong
kernelWidth mpsImageConvolution =
  sendMessage mpsImageConvolution kernelWidthSelector

-- | bias
--
-- The bias is a value to be added to convolved pixel before it is converted back to the storage format.               It can be used to convert negative values into a representable range for a unsigned MTLPixelFormat.               For example, many edge detection filters produce results in the range [-k,k]. By scaling the filter               weights by 0.5/k and adding 0.5, the results will be in range [0,1] suitable for use with unorm formats.                It can be used in combination with renormalization of the filter weights to do video ranging as part                of the convolution effect. It can also just be used to increase the brightness of the image.
--
-- Default value is 0.0f.
--
-- ObjC selector: @- bias@
bias :: IsMPSImageConvolution mpsImageConvolution => mpsImageConvolution -> IO CFloat
bias mpsImageConvolution =
  sendMessage mpsImageConvolution biasSelector

-- | bias
--
-- The bias is a value to be added to convolved pixel before it is converted back to the storage format.               It can be used to convert negative values into a representable range for a unsigned MTLPixelFormat.               For example, many edge detection filters produce results in the range [-k,k]. By scaling the filter               weights by 0.5/k and adding 0.5, the results will be in range [0,1] suitable for use with unorm formats.                It can be used in combination with renormalization of the filter weights to do video ranging as part                of the convolution effect. It can also just be used to increase the brightness of the image.
--
-- Default value is 0.0f.
--
-- ObjC selector: @- setBias:@
setBias :: IsMPSImageConvolution mpsImageConvolution => mpsImageConvolution -> CFloat -> IO ()
setBias mpsImageConvolution value =
  sendMessage mpsImageConvolution setBiasSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:weights:@
initWithDevice_kernelWidth_kernelHeight_weightsSelector :: Selector '[RawId, CULong, CULong, Const (Ptr CFloat)] (Id MPSImageConvolution)
initWithDevice_kernelWidth_kernelHeight_weightsSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageConvolution)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @bias@
biasSelector :: Selector '[] CFloat
biasSelector = mkSelector "bias"

-- | @Selector@ for @setBias:@
setBiasSelector :: Selector '[CFloat] ()
setBiasSelector = mkSelector "setBias:"

