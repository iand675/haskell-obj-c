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
  , initWithDeviceSelector
  , initWithDevice_centerWeightSelector
  , initWithDevice_kernelWidth_kernelHeight_weightsSelector
  , initWithCoder_deviceSelector
  , kernelHeightSelector
  , kernelWidthSelector


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
initWithDevice mpsImagePyramid  device =
  sendMsg mpsImagePyramid (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_centerWeight mpsImagePyramid  device centerWeight =
  sendMsg mpsImagePyramid (mkSelector "initWithDevice:centerWeight:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral centerWeight)] >>= ownedObject . castPtr

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
initWithDevice_kernelWidth_kernelHeight_weights mpsImagePyramid  device kernelWidth kernelHeight kernelWeights =
  sendMsg mpsImagePyramid (mkSelector "initWithDevice:kernelWidth:kernelHeight:weights:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argPtr (unConst kernelWeights)] >>= ownedObject . castPtr

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
initWithCoder_device mpsImagePyramid  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImagePyramid (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | kernelHeight
--
-- The height of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> IO CULong
kernelHeight mpsImagePyramid  =
  sendMsg mpsImagePyramid (mkSelector "kernelHeight") retCULong []

-- | kernelWidth
--
-- The width of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSImagePyramid mpsImagePyramid => mpsImagePyramid -> IO CULong
kernelWidth mpsImagePyramid  =
  sendMsg mpsImagePyramid (mkSelector "kernelWidth") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:centerWeight:@
initWithDevice_centerWeightSelector :: Selector
initWithDevice_centerWeightSelector = mkSelector "initWithDevice:centerWeight:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:weights:@
initWithDevice_kernelWidth_kernelHeight_weightsSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_weightsSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

