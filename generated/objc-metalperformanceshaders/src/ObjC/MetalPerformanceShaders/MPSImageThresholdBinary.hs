{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageThresholdBinary
--
-- The MPSThreshold filter applies a fixed-level threshold to each pixel in the image.              The threshold functions convert a single channel image to a binary image.              If the input image is not a single channel image, convert the inputimage to a single channel              luminance image using the linearGrayColorTransform and then apply the threshold.              The ThresholdBinary function is:                  destinationPixelValue = sourcePixelValue > thresholdValue ? maximumValue : 0
--
-- Generated bindings for @MPSImageThresholdBinary@.
module ObjC.MetalPerformanceShaders.MPSImageThresholdBinary
  ( MPSImageThresholdBinary
  , IsMPSImageThresholdBinary(..)
  , initWithDevice_thresholdValue_maximumValue_linearGrayColorTransform
  , initWithCoder_device
  , initWithDevice
  , thresholdValue
  , maximumValue
  , transform
  , initWithDevice_thresholdValue_maximumValue_linearGrayColorTransformSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , thresholdValueSelector
  , maximumValueSelector
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

-- | initialize a MPSImageThresholdBinary filter
--
-- @device@ — The device the filter will run on
--
-- @thresholdValue@ — The threshold value to use
--
-- @maximumValue@ — The maximum value to use
--
-- @transform@ — This matrix is an array of 3 floats.                              The default if no transform is specifed is BT.601/JPEG: {0.299f, 0.587f, 0.114f};
--
-- ObjC selector: @- initWithDevice:thresholdValue:maximumValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_maximumValue_linearGrayColorTransform :: IsMPSImageThresholdBinary mpsImageThresholdBinary => mpsImageThresholdBinary -> RawId -> CFloat -> CFloat -> Const (Ptr CFloat) -> IO (Id MPSImageThresholdBinary)
initWithDevice_thresholdValue_maximumValue_linearGrayColorTransform mpsImageThresholdBinary  device thresholdValue maximumValue transform =
  sendMsg mpsImageThresholdBinary (mkSelector "initWithDevice:thresholdValue:maximumValue:linearGrayColorTransform:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral thresholdValue), argCFloat (fromIntegral maximumValue), argPtr (unConst transform)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageThresholdBinary mpsImageThresholdBinary, IsNSCoder aDecoder) => mpsImageThresholdBinary -> aDecoder -> RawId -> IO (Id MPSImageThresholdBinary)
initWithCoder_device mpsImageThresholdBinary  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageThresholdBinary (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageThresholdBinary mpsImageThresholdBinary => mpsImageThresholdBinary -> RawId -> IO (Id MPSImageThresholdBinary)
initWithDevice mpsImageThresholdBinary  device =
  sendMsg mpsImageThresholdBinary (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | thresholdValue
--
-- The threshold value used to init the threshold filter
--
-- ObjC selector: @- thresholdValue@
thresholdValue :: IsMPSImageThresholdBinary mpsImageThresholdBinary => mpsImageThresholdBinary -> IO CFloat
thresholdValue mpsImageThresholdBinary  =
  sendMsg mpsImageThresholdBinary (mkSelector "thresholdValue") retCFloat []

-- | maximumValue
--
-- The maximum value used to init the threshold filter
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMPSImageThresholdBinary mpsImageThresholdBinary => mpsImageThresholdBinary -> IO CFloat
maximumValue mpsImageThresholdBinary  =
  sendMsg mpsImageThresholdBinary (mkSelector "maximumValue") retCFloat []

-- | transform
--
-- The color transform used to init the threshold filter
--
-- ObjC selector: @- transform@
transform :: IsMPSImageThresholdBinary mpsImageThresholdBinary => mpsImageThresholdBinary -> IO (Const (Ptr CFloat))
transform mpsImageThresholdBinary  =
  fmap Const $ fmap castPtr $ sendMsg mpsImageThresholdBinary (mkSelector "transform") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:thresholdValue:maximumValue:linearGrayColorTransform:@
initWithDevice_thresholdValue_maximumValue_linearGrayColorTransformSelector :: Selector
initWithDevice_thresholdValue_maximumValue_linearGrayColorTransformSelector = mkSelector "initWithDevice:thresholdValue:maximumValue:linearGrayColorTransform:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @thresholdValue@
thresholdValueSelector :: Selector
thresholdValueSelector = mkSelector "thresholdValue"

-- | @Selector@ for @maximumValue@
maximumValueSelector :: Selector
maximumValueSelector = mkSelector "maximumValue"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

