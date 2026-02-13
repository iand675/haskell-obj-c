{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImagePyramid
--
-- The MPSImagePyramid is a base class for creating different kinds of pyramid images
--
-- Currently supported pyramid-types are:              MPSImageGaussianPyramid
--
-- The Gaussian image pyramid kernel is enqueued as a in-place operation using              MPSUnaryImageKernel::encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:              and all mipmap levels after level=1, present in the provided image are filled using              the provided filtering kernel. The fallbackCopyAllocator parameter is not used.
--
-- The Gaussian image pyramid filter ignores clipRect and offset and fills              the entire mipmap levels.
--
-- Note: Make sure your texture type is compatible with mipmapping and supports texture views                  (see MTLTextureUsagePixelFormatView).
--
-- Note: Recall the size of the nth mipmap level:
--
-- w_n = max(1, floor(w_0 / 2^n))
-- h_n = max(1, floor(h_0 / 2^n)),
--
-- where w_0, h_0 are the zeroth level width and height. ie the image dimensions themselves.
--
-- Generated bindings for @MPSImagePyramid@.
module ObjC.MetalPerformanceShaders.MPSImagePyramid
  ( MPSImagePyramid
  , IsMPSImagePyramid(..)
  , initWithDevice
  , initWithDevice_centerWeight
  , initWithDevice_kernelWidth_kernelHeight_weights
  , initWithCoder_device
  , kernelHeight
  , kernelWidth
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_centerWeightSelector
  , initWithDevice_kernelWidth_kernelHeight_weightsSelector
  , kernelHeightSelector
  , kernelWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a downwards 5-tap image pyramid with the default filter kernel and device
--
-- @device@ — The device the filter will run on
--
-- The filter kernel is the outer product of w = [ 1/16,  1/4,  3/8,  1/4,  1/16 ]^T, with itself
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> RawId -> IO (Id MPSImagePyramid)
initWithDevice mpsImagePyramid device =
  sendOwnedMessage mpsImagePyramid initWithDeviceSelector device

-- | Initialize a downwards 5-tap image pyramid with a central weight parameter and device
--
-- @device@ — The device the filter will run on
--
-- @centerWeight@ — Defines form of the filter-kernel  through the outer product ww^T, where              w = [ (1/4 - a/2),  1/4,  a,  1/4,  (1/4 - a/2) ]^T and 'a' is centerWeight.
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:centerWeight:@
initWithDevice_centerWeight :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> RawId -> CFloat -> IO (Id MPSImagePyramid)
initWithDevice_centerWeight mpsImagePyramid device centerWeight =
  sendOwnedMessage mpsImagePyramid initWithDevice_centerWeightSelector device centerWeight

-- | Initialize a downwards n-tap pyramid with a custom filter kernel and device
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the filtering kernel. See MPSImageConvolution.
--
-- @kernelHeight@ — The height of the filtering kernel. See MPSImageConvolution.
--
-- @kernelWeights@ — A pointer to an array of kernelWidth * kernelHeight values to be                              used as the kernel.                              These are in row major order. See MPSImageConvolution.
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:weights:@
initWithDevice_kernelWidth_kernelHeight_weights :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> RawId -> CULong -> CULong -> Const (Ptr CFloat) -> IO (Id MPSImagePyramid)
initWithDevice_kernelWidth_kernelHeight_weights mpsImagePyramid device kernelWidth kernelHeight kernelWeights =
  sendOwnedMessage mpsImagePyramid initWithDevice_kernelWidth_kernelHeight_weightsSelector device kernelWidth kernelHeight kernelWeights

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSCNNPooling object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSImagePyramid mpsImagePyramid, IsNSCoder aDecoder) => mpsImagePyramid -> aDecoder -> RawId -> IO (Id MPSImagePyramid)
initWithCoder_device mpsImagePyramid aDecoder device =
  sendOwnedMessage mpsImagePyramid initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | kernelHeight
--
-- The height of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> IO CULong
kernelHeight mpsImagePyramid =
  sendMessage mpsImagePyramid kernelHeightSelector

-- | kernelWidth
--
-- The width of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> IO CULong
kernelWidth mpsImagePyramid =
  sendMessage mpsImagePyramid kernelWidthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImagePyramid)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:centerWeight:@
initWithDevice_centerWeightSelector :: Selector '[RawId, CFloat] (Id MPSImagePyramid)
initWithDevice_centerWeightSelector = mkSelector "initWithDevice:centerWeight:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:weights:@
initWithDevice_kernelWidth_kernelHeight_weightsSelector :: Selector '[RawId, CULong, CULong, Const (Ptr CFloat)] (Id MPSImagePyramid)
initWithDevice_kernelWidth_kernelHeight_weightsSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImagePyramid)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

