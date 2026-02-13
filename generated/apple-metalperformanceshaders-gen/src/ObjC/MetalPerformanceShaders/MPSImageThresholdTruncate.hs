{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageThresholdTruncate
--
-- The MPSImageThresholdTruncate filter applies a fixed-level threshold to each pixel in the image:              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdTruncate function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? thresholdValue : sourcePixelValue
--
-- Generated bindings for @MPSImageThresholdTruncate@.
module ObjC.MetalPerformanceShaders.MPSImageThresholdTruncate
  ( MPSImageThresholdTruncate
  , IsMPSImageThresholdTruncate(..)
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

-- | initialize a MPSImageThresholdTruncate filter
--
-- @device@ — The device the filter will run on
--
-- @thresholdValue@ — The threshold value to use
--
-- @transform@ — This matrix is an array of 3 floats.                              The default if no transform is specifed is BT.601/JPEG: {0.299f, 0.587f, 0.114f};
--
-- ObjC selector: @- initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransform :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> RawId -> CFloat -> Const (Ptr CFloat) -> IO (Id MPSImageThresholdTruncate)
initWithDevice_thresholdValue_linearGrayColorTransform mpsImageThresholdTruncate device thresholdValue transform =
  sendOwnedMessage mpsImageThresholdTruncate initWithDevice_thresholdValue_linearGrayColorTransformSelector device thresholdValue transform

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
initWithCoder_device :: (IsMPSImageThresholdTruncate mpsImageThresholdTruncate, IsNSCoder aDecoder) => mpsImageThresholdTruncate -> aDecoder -> RawId -> IO (Id MPSImageThresholdTruncate)
initWithCoder_device mpsImageThresholdTruncate aDecoder device =
  sendOwnedMessage mpsImageThresholdTruncate initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> RawId -> IO (Id MPSImageThresholdTruncate)
initWithDevice mpsImageThresholdTruncate device =
  sendOwnedMessage mpsImageThresholdTruncate initWithDeviceSelector device

-- | thresholdValue
--
-- The threshold value used to init the threshold filter
--
-- ObjC selector: @- thresholdValue@
thresholdValue :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> IO CFloat
thresholdValue mpsImageThresholdTruncate =
  sendMessage mpsImageThresholdTruncate thresholdValueSelector

-- | transform
--
-- The color transform used to init the threshold filter
--
-- ObjC selector: @- transform@
transform :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> IO (Const (Ptr CFloat))
transform mpsImageThresholdTruncate =
  sendMessage mpsImageThresholdTruncate transformSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransformSelector :: Selector '[RawId, CFloat, Const (Ptr CFloat)] (Id MPSImageThresholdTruncate)
initWithDevice_thresholdValue_linearGrayColorTransformSelector = mkSelector "initWithDevice:thresholdValue:linearGrayColorTransform:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageThresholdTruncate)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageThresholdTruncate)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @thresholdValue@
thresholdValueSelector :: Selector '[] CFloat
thresholdValueSelector = mkSelector "thresholdValue"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Const (Ptr CFloat))
transformSelector = mkSelector "transform"

