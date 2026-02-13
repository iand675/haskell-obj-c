{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageThresholdToZero
--
-- The MPSImageThresholdToZero filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdToZero function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? sourcePixelValue : 0
--
-- Generated bindings for @MPSImageThresholdToZero@.
module ObjC.MetalPerformanceShaders.MPSImageThresholdToZero
  ( MPSImageThresholdToZero
  , IsMPSImageThresholdToZero(..)
  , initWithDevice_thresholdValue_linearGrayColorTransform
  , initWithDevice
  , initWithCoder_device
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

-- | initialize a MPSImageThresholdToZero filter
--
-- @device@ — The device the filter will run on
--
-- @thresholdValue@ — The threshold value to use
--
-- @transform@ — This matrix is an array of 3 floats.                              The default if no transform is specifed is BT.601/JPEG: {0.299f, 0.587f, 0.114f};
--
-- ObjC selector: @- initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransform :: IsMPSImageThresholdToZero mpsImageThresholdToZero => mpsImageThresholdToZero -> RawId -> CFloat -> Const (Ptr CFloat) -> IO (Id MPSImageThresholdToZero)
initWithDevice_thresholdValue_linearGrayColorTransform mpsImageThresholdToZero device thresholdValue transform =
  sendOwnedMessage mpsImageThresholdToZero initWithDevice_thresholdValue_linearGrayColorTransformSelector device thresholdValue transform

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageThresholdToZero mpsImageThresholdToZero => mpsImageThresholdToZero -> RawId -> IO (Id MPSImageThresholdToZero)
initWithDevice mpsImageThresholdToZero device =
  sendOwnedMessage mpsImageThresholdToZero initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSImageThresholdToZero mpsImageThresholdToZero, IsNSCoder aDecoder) => mpsImageThresholdToZero -> aDecoder -> RawId -> IO (Id MPSImageThresholdToZero)
initWithCoder_device mpsImageThresholdToZero aDecoder device =
  sendOwnedMessage mpsImageThresholdToZero initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | thresholdValue
--
-- The threshold value used to init the threshold filter
--
-- ObjC selector: @- thresholdValue@
thresholdValue :: IsMPSImageThresholdToZero mpsImageThresholdToZero => mpsImageThresholdToZero -> IO CFloat
thresholdValue mpsImageThresholdToZero =
  sendMessage mpsImageThresholdToZero thresholdValueSelector

-- | transform
--
-- The color transform used to init the threshold filter
--
-- ObjC selector: @- transform@
transform :: IsMPSImageThresholdToZero mpsImageThresholdToZero => mpsImageThresholdToZero -> IO (Const (Ptr CFloat))
transform mpsImageThresholdToZero =
  sendMessage mpsImageThresholdToZero transformSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransformSelector :: Selector '[RawId, CFloat, Const (Ptr CFloat)] (Id MPSImageThresholdToZero)
initWithDevice_thresholdValue_linearGrayColorTransformSelector = mkSelector "initWithDevice:thresholdValue:linearGrayColorTransform:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageThresholdToZero)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageThresholdToZero)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @thresholdValue@
thresholdValueSelector :: Selector '[] CFloat
thresholdValueSelector = mkSelector "thresholdValue"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Const (Ptr CFloat))
transformSelector = mkSelector "transform"

