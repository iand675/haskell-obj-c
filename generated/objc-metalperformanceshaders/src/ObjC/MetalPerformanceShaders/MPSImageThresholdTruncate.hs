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
  , initWithDevice_thresholdValue_linearGrayColorTransformSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , thresholdValueSelector
  , transformSelector


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
initWithDevice_thresholdValue_linearGrayColorTransform mpsImageThresholdTruncate  device thresholdValue transform =
  sendMsg mpsImageThresholdTruncate (mkSelector "initWithDevice:thresholdValue:linearGrayColorTransform:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral thresholdValue), argPtr (unConst transform)] >>= ownedObject . castPtr

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
initWithCoder_device mpsImageThresholdTruncate  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageThresholdTruncate (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> RawId -> IO (Id MPSImageThresholdTruncate)
initWithDevice mpsImageThresholdTruncate  device =
  sendMsg mpsImageThresholdTruncate (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | thresholdValue
--
-- The threshold value used to init the threshold filter
--
-- ObjC selector: @- thresholdValue@
thresholdValue :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> IO CFloat
thresholdValue mpsImageThresholdTruncate  =
  sendMsg mpsImageThresholdTruncate (mkSelector "thresholdValue") retCFloat []

-- | transform
--
-- The color transform used to init the threshold filter
--
-- ObjC selector: @- transform@
transform :: IsMPSImageThresholdTruncate mpsImageThresholdTruncate => mpsImageThresholdTruncate -> IO (Const (Ptr CFloat))
transform mpsImageThresholdTruncate  =
  fmap Const $ fmap castPtr $ sendMsg mpsImageThresholdTruncate (mkSelector "transform") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:thresholdValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_linearGrayColorTransformSelector :: Selector
initWithDevice_thresholdValue_linearGrayColorTransformSelector = mkSelector "initWithDevice:thresholdValue:linearGrayColorTransform:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @thresholdValue@
thresholdValueSelector :: Selector
thresholdValueSelector = mkSelector "thresholdValue"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

