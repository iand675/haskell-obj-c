{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageGaussianBlur
--
-- The MPSImageGaussianBlur convolves an image with gaussian of given sigma in both x and y direction.
--
-- The MPSImageGaussianBlur utilizes a very fast algorith that typically runs at approximately                  1/2 of copy speeds. Notably, it is faster than either the tent or box blur except perhaps                  for very large filter windows. Mathematically, it is an approximate gaussian. Some                  non-gaussian behavior may be detectable with advanced analytical methods such as FFT.                    If a analytically clean gaussian filter is required, please use the MPSImageConvolution                   filter instead with an appropriate set of weights. The MPSImageGaussianBlur is intended                  to be suitable for all common image processing needs demanding ~10 bits of precision or                  less.
--
-- Generated bindings for @MPSImageGaussianBlur@.
module ObjC.MetalPerformanceShaders.MPSImageGaussianBlur
  ( MPSImageGaussianBlur
  , IsMPSImageGaussianBlur(..)
  , initWithDevice_sigma
  , initWithCoder_device
  , initWithDevice
  , sigma
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_sigmaSelector
  , sigmaSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a gaussian blur filter for a particular sigma and device
--
-- @device@ — The device the filter will run on
--
-- @sigma@ — The standard deviation of gaussian blur filter.                       Gaussian weight, centered at 0, at integer grid i is given as                             w(i) = 1/sqrt(2*pi*sigma) * exp(-i^2/(2*sigma^2))                      If we take cut off at 1% of w(0) (max weight) beyond which weights                      are considered 0, we have                               ceil (sqrt(-log(0.01)*2)*sigma) ~ ceil(3.7*sigma)                       as rough estimate of filter width
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:sigma:@
initWithDevice_sigma :: IsMPSImageGaussianBlur mpsImageGaussianBlur => mpsImageGaussianBlur -> RawId -> CFloat -> IO (Id MPSImageGaussianBlur)
initWithDevice_sigma mpsImageGaussianBlur device sigma =
  sendOwnedMessage mpsImageGaussianBlur initWithDevice_sigmaSelector device sigma

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
initWithCoder_device :: (IsMPSImageGaussianBlur mpsImageGaussianBlur, IsNSCoder aDecoder) => mpsImageGaussianBlur -> aDecoder -> RawId -> IO (Id MPSImageGaussianBlur)
initWithCoder_device mpsImageGaussianBlur aDecoder device =
  sendOwnedMessage mpsImageGaussianBlur initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageGaussianBlur mpsImageGaussianBlur => mpsImageGaussianBlur -> RawId -> IO (Id MPSImageGaussianBlur)
initWithDevice mpsImageGaussianBlur device =
  sendOwnedMessage mpsImageGaussianBlur initWithDeviceSelector device

-- | sigma
--
-- Read-only sigma value with which filter was created
--
-- ObjC selector: @- sigma@
sigma :: IsMPSImageGaussianBlur mpsImageGaussianBlur => mpsImageGaussianBlur -> IO CFloat
sigma mpsImageGaussianBlur =
  sendMessage mpsImageGaussianBlur sigmaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sigma:@
initWithDevice_sigmaSelector :: Selector '[RawId, CFloat] (Id MPSImageGaussianBlur)
initWithDevice_sigmaSelector = mkSelector "initWithDevice:sigma:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageGaussianBlur)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageGaussianBlur)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @sigma@
sigmaSelector :: Selector '[] CFloat
sigmaSelector = mkSelector "sigma"

