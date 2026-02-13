{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageCanny
--
-- The MPSImageCanny implements the Canny edge detection algorithm.              When the color model of the source and destination textures match, the              filter is applied to each channel seperately. If the destination is monochrome              but source multichannel, the source will be converted to grayscale using the              linear gray color transform vector (v).                Luminance = v[0] * pixel.x + v[1] * pixel.y + v[2] * pixel.z;
--
-- The canny edge detection algorithm consists of 5 steps:              1. Blur the source image using a Gaussian blur with a sigma parameter              2. Use horizontal and vertical Sobel filters to find a gradient magnitude and                direction.                  G = sqrt(Sx^2 + Sy^2)                  G_ang = arctan(Sy / Sx)              3. Perform non-maximum suppression to thin edges to single pixel widths.                A pixel is considered to be a maxium along the edge if it has the largest                gradient magnitude along the positive and negatve gradient direction. That                is, if the gradient direction is 90°, if the gradient magnitude of a pixel is                greater than its neighbors at -90° and 90° it is the maximum. Any pixel                which is not a maximum will have its value suppressed, by setting it's                magnitude to 0.              4. Double thresholding is preformed with two values ht and lt with ht > lt                to classify a pixel as part of a weak or strong edge. A pixel with gradient                value G is classified as:                  Strong edge: G > ht                  Weak edge: ht >= G > lt                  Not an edge: lt >= G              5. Edge tracking is performed along all weak edges to determine if they                are part of a strong edge. Any weak edges which are connected to a                strong edge are labelled true edges, along with strong edges themselves.                A pixel can be connected through any of its 8 neighbors. Any pixel marked                as a true edge is output with a high value, and all others are considered                background and output with a low value.
--
-- Generated bindings for @MPSImageCanny@.
module ObjC.MetalPerformanceShaders.MPSImageCanny
  ( MPSImageCanny
  , IsMPSImageCanny(..)
  , initWithDevice
  , initWithDevice_linearToGrayScaleTransform_sigma
  , initWithCoder_device
  , colorTransform
  , sigma
  , highThreshold
  , setHighThreshold
  , lowThreshold
  , setLowThreshold
  , useFastMode
  , setUseFastMode
  , colorTransformSelector
  , highThresholdSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_linearToGrayScaleTransform_sigmaSelector
  , lowThresholdSelector
  , setHighThresholdSelector
  , setLowThresholdSelector
  , setUseFastModeSelector
  , sigmaSelector
  , useFastModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a Canny filter on a given device using the default color              transform and default sigma value for Gaussian blur.              Default transform: BT.601/JPEG {0.299f, 0.587f, 0.114f}              Default sigma: sqrt(2)
--
-- For non-default parameters, use              -initWithDevice:linearGrayColorTransform:sigma:
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> RawId -> IO (Id MPSImageCanny)
initWithDevice mpsImageCanny device =
  sendOwnedMessage mpsImageCanny initWithDeviceSelector device

-- | Initialize a Canny filter on a given device with a non-default color transform and              non-default sigma.
--
-- @device@ — The device the filter will run on
--
-- @transform@ — Array of three floats describing the rgb to gray scale color transform.
--
-- Luminance = transform[0] * pixel.x +
-- transform[1] * pixel.y +
-- transform[2] * pixel.z;
--
-- @sigma@ — The standard deviation of gaussian blur filter.                          Gaussian weight, centered at 0, at integer grid n is given as
--
-- w(i) = 1/sqrt(2*pi*sigma) * exp(-n^2/2*sigma^2)
--
-- If we take cut off at 1% of w(0) (max weight) beyond which weights                          are considered 0, we have
--
-- ceil (sqrt(-log(0.01)*2)*sigma) ~ ceil(3.7*sigma)
--
-- as rough estimate of filter width
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:linearToGrayScaleTransform:sigma:@
initWithDevice_linearToGrayScaleTransform_sigma :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> RawId -> Const (Ptr CFloat) -> Const CFloat -> IO (Id MPSImageCanny)
initWithDevice_linearToGrayScaleTransform_sigma mpsImageCanny device transform sigma =
  sendOwnedMessage mpsImageCanny initWithDevice_linearToGrayScaleTransform_sigmaSelector device transform sigma

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
initWithCoder_device :: (IsMPSImageCanny mpsImageCanny, IsNSCoder aDecoder) => mpsImageCanny -> aDecoder -> RawId -> IO (Id MPSImageCanny)
initWithCoder_device mpsImageCanny aDecoder device =
  sendOwnedMessage mpsImageCanny initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | colorTransform
--
-- Returns a pointer to the array of three floats used to convert RGBA, RGB or RG images               to the destination format when the destination is monochrome.               Value is readonly and user should not modify or free.
--
-- ObjC selector: @- colorTransform@
colorTransform :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> IO (Const (Ptr CFloat))
colorTransform mpsImageCanny =
  sendMessage mpsImageCanny colorTransformSelector

-- | sigma
--
-- Read-only sigma value used in performing Gaussian blur of the image
--
-- ObjC selector: @- sigma@
sigma :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> IO CFloat
sigma mpsImageCanny =
  sendMessage mpsImageCanny sigmaSelector

-- | highThreshold
--
-- Read-write value used to set the high threshold for double thresholding, value is normalized.            Default is 0.4
--
-- ObjC selector: @- highThreshold@
highThreshold :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> IO CFloat
highThreshold mpsImageCanny =
  sendMessage mpsImageCanny highThresholdSelector

-- | highThreshold
--
-- Read-write value used to set the high threshold for double thresholding, value is normalized.            Default is 0.4
--
-- ObjC selector: @- setHighThreshold:@
setHighThreshold :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> CFloat -> IO ()
setHighThreshold mpsImageCanny value =
  sendMessage mpsImageCanny setHighThresholdSelector value

-- | lowThreshold
--
-- Read-write value used to set the low threshold for double thresholding, value is normalized.            Default is 0.2
--
-- ObjC selector: @- lowThreshold@
lowThreshold :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> IO CFloat
lowThreshold mpsImageCanny =
  sendMessage mpsImageCanny lowThresholdSelector

-- | lowThreshold
--
-- Read-write value used to set the low threshold for double thresholding, value is normalized.            Default is 0.2
--
-- ObjC selector: @- setLowThreshold:@
setLowThreshold :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> CFloat -> IO ()
setLowThreshold mpsImageCanny value =
  sendMessage mpsImageCanny setLowThresholdSelector value

-- | useFastMode
--
-- Read-write value used to change algorithm to an approximation of the true Canny Edge detection Algorithm.            When true, a limit is placed on how far a single strong edge can extend. The result will be similar to a true output            but some edges may terminate early, resulting in minor differences for cases with long, weak edges. The performance            for the approximate canny implementation is improved and should provide similar enough results for most cases.            Extra tuning of the high and low thresholds as well as sigma may help achieve a more similar output in this mode.            Default is YES
--
-- ObjC selector: @- useFastMode@
useFastMode :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> IO Bool
useFastMode mpsImageCanny =
  sendMessage mpsImageCanny useFastModeSelector

-- | useFastMode
--
-- Read-write value used to change algorithm to an approximation of the true Canny Edge detection Algorithm.            When true, a limit is placed on how far a single strong edge can extend. The result will be similar to a true output            but some edges may terminate early, resulting in minor differences for cases with long, weak edges. The performance            for the approximate canny implementation is improved and should provide similar enough results for most cases.            Extra tuning of the high and low thresholds as well as sigma may help achieve a more similar output in this mode.            Default is YES
--
-- ObjC selector: @- setUseFastMode:@
setUseFastMode :: IsMPSImageCanny mpsImageCanny => mpsImageCanny -> Bool -> IO ()
setUseFastMode mpsImageCanny value =
  sendMessage mpsImageCanny setUseFastModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageCanny)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:linearToGrayScaleTransform:sigma:@
initWithDevice_linearToGrayScaleTransform_sigmaSelector :: Selector '[RawId, Const (Ptr CFloat), Const CFloat] (Id MPSImageCanny)
initWithDevice_linearToGrayScaleTransform_sigmaSelector = mkSelector "initWithDevice:linearToGrayScaleTransform:sigma:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageCanny)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @colorTransform@
colorTransformSelector :: Selector '[] (Const (Ptr CFloat))
colorTransformSelector = mkSelector "colorTransform"

-- | @Selector@ for @sigma@
sigmaSelector :: Selector '[] CFloat
sigmaSelector = mkSelector "sigma"

-- | @Selector@ for @highThreshold@
highThresholdSelector :: Selector '[] CFloat
highThresholdSelector = mkSelector "highThreshold"

-- | @Selector@ for @setHighThreshold:@
setHighThresholdSelector :: Selector '[CFloat] ()
setHighThresholdSelector = mkSelector "setHighThreshold:"

-- | @Selector@ for @lowThreshold@
lowThresholdSelector :: Selector '[] CFloat
lowThresholdSelector = mkSelector "lowThreshold"

-- | @Selector@ for @setLowThreshold:@
setLowThresholdSelector :: Selector '[CFloat] ()
setLowThresholdSelector = mkSelector "setLowThreshold:"

-- | @Selector@ for @useFastMode@
useFastModeSelector :: Selector '[] Bool
useFastModeSelector = mkSelector "useFastMode"

-- | @Selector@ for @setUseFastMode:@
setUseFastModeSelector :: Selector '[Bool] ()
setUseFastModeSelector = mkSelector "setUseFastMode:"

