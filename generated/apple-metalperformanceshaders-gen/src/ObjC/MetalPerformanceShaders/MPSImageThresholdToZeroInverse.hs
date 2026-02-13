{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageThresholdToZeroInverse
--
-- The MPSImageThresholdToZeroInverse filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdToZeroINverse function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? 0 : sourcePixelValue
--
-- Generated bindings for @MPSImageThresholdToZeroInverse@.
module ObjC.MetalPerformanceShaders.MPSImageThresholdToZeroInverse
  ( MPSImageThresholdToZeroInverse
  , IsMPSImageThresholdToZeroInverse(..)
  , initWithDevice_thresholdValue_linearGrayColorTransform
  , initWithCoder_device
  , initWithDevice
  , thresholdValue
  , transform
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_thresholdValue_linearGrayColorTransformSelector
  , thresholdValueSelector
  , transformSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initialize a MPSImageThresholdToZeroInverse filter
--
-- @device@ — The device the filter will run on
--
-- @thresholdValue@ — The threshold value to use
--
-- @transform@ — This matrix is an array of 3 floats.                              The default if no transform is specifed is BT.601/JPEG: {0.299f, 0.587f, 0.114f};
--
-- ObjC selector: @- initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransform :: IsMPSImageThresholdToZeroInverse mpsImageThresholdToZeroInverse => mpsImageThresholdToZeroInverse -> RawId -> CFloat -> Const (Ptr CFloat) -> IO (Id MPSImageThresholdToZeroInverse)
initWithDevice_thresholdValue_linearGrayColorTransform mpsImageThresholdToZeroInverse device thresholdValue transform =
  sendOwnedMessage mpsImageThresholdToZeroInverse initWithDevice_thresholdValue_linearGrayColorTransformSelector device thresholdValue transform

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
initWithCoder_device :: (IsMPSImageThresholdToZeroInverse mpsImageThresholdToZeroInverse, IsNSCoder aDecoder) => mpsImageThresholdToZeroInverse -> aDecoder -> RawId -> IO (Id MPSImageThresholdToZeroInverse)
initWithCoder_device mpsImageThresholdToZeroInverse aDecoder device =
  sendOwnedMessage mpsImageThresholdToZeroInverse initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageThresholdToZeroInverse mpsImageThresholdToZeroInverse => mpsImageThresholdToZeroInverse -> RawId -> IO (Id MPSImageThresholdToZeroInverse)
initWithDevice mpsImageThresholdToZeroInverse device =
  sendOwnedMessage mpsImageThresholdToZeroInverse initWithDeviceSelector device

-- | thresholdValue
--
-- The threshold value used to init the threshold filter
--
-- ObjC selector: @- thresholdValue@
thresholdValue :: IsMPSImageThresholdToZeroInverse mpsImageThresholdToZeroInverse => mpsImageThresholdToZeroInverse -> IO CFloat
thresholdValue mpsImageThresholdToZeroInverse =
  sendMessage mpsImageThresholdToZeroInverse thresholdValueSelector

-- | transform
--
-- The color transform used to init the threshold filter
--
-- ObjC selector: @- transform@
transform :: IsMPSImageThresholdToZeroInverse mpsImageThresholdToZeroInverse => mpsImageThresholdToZeroInverse -> IO (Const (Ptr CFloat))
transform mpsImageThresholdToZeroInverse =
  sendMessage mpsImageThresholdToZeroInverse transformSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransformSelector :: Selector '[RawId, CFloat, Const (Ptr CFloat)] (Id MPSImageThresholdToZeroInverse)
initWithDevice_thresholdValue_linearGrayColorTransformSelector = mkSelector "initWithDevice:thresholdValue:linearGrayColorTransform:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageThresholdToZeroInverse)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageThresholdToZeroInverse)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @thresholdValue@
thresholdValueSelector :: Selector '[] CFloat
thresholdValueSelector = mkSelector "thresholdValue"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Const (Ptr CFloat))
transformSelector = mkSelector "transform"

