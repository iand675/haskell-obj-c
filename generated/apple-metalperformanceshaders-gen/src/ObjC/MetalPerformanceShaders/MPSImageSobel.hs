{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageSobel
--
-- The MPSImageSobel implements the Sobel filter.              When the color model (e.g. RGB, two-channel, grayscale, etc.) of source               and destination textures match, the filter is applied to each channel               separately. If the destination is monochrome (single channel) but source               multichannel, the pixel values are converted to grayscale before applying Sobel              operator using the linear gray color transform vector (v).
--
-- Luminance = v[0] * pixel.x + v[1] * pixel.y + v[2] * pixel.z;
--
-- Generated bindings for @MPSImageSobel@.
module ObjC.MetalPerformanceShaders.MPSImageSobel
  ( MPSImageSobel
  , IsMPSImageSobel(..)
  , initWithDevice
  , initWithDevice_linearGrayColorTransform
  , initWithCoder_device
  , colorTransform
  , colorTransformSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_linearGrayColorTransformSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a Sobel filter on a given device using the default color               transform. Default: BT.601/JPEG {0.299f, 0.587f, 0.114f}
--
-- For non-default conversion matrices, use -initWithDevice:linearGrayColorTransform:
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageSobel mpsImageSobel => mpsImageSobel -> RawId -> IO (Id MPSImageSobel)
initWithDevice mpsImageSobel device =
  sendOwnedMessage mpsImageSobel initWithDeviceSelector device

-- | Initialize a Sobel filter on a given device with a non-default color transform
--
-- @device@ — The device the filter will run on
--
-- @transform@ — Array of three floats describing the rgb to gray scale color transform.
--
-- Luminance = transform[0] * pixel.x +
-- transform[1] * pixel.y +
-- transform[2] * pixel.z;
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:linearGrayColorTransform:@
initWithDevice_linearGrayColorTransform :: IsMPSImageSobel mpsImageSobel => mpsImageSobel -> RawId -> Const (Ptr CFloat) -> IO (Id MPSImageSobel)
initWithDevice_linearGrayColorTransform mpsImageSobel device transform =
  sendOwnedMessage mpsImageSobel initWithDevice_linearGrayColorTransformSelector device transform

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
initWithCoder_device :: (IsMPSImageSobel mpsImageSobel, IsNSCoder aDecoder) => mpsImageSobel -> aDecoder -> RawId -> IO (Id MPSImageSobel)
initWithCoder_device mpsImageSobel aDecoder device =
  sendOwnedMessage mpsImageSobel initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | colorTransform
--
-- Returns a pointer to the array of three floats used to convert RGBA, RGB or RG images               to the destination format when the destination is monochrome.
--
-- ObjC selector: @- colorTransform@
colorTransform :: IsMPSImageSobel mpsImageSobel => mpsImageSobel -> IO (Const (Ptr CFloat))
colorTransform mpsImageSobel =
  sendMessage mpsImageSobel colorTransformSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageSobel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:linearGrayColorTransform:@
initWithDevice_linearGrayColorTransformSelector :: Selector '[RawId, Const (Ptr CFloat)] (Id MPSImageSobel)
initWithDevice_linearGrayColorTransformSelector = mkSelector "initWithDevice:linearGrayColorTransform:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageSobel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @colorTransform@
colorTransformSelector :: Selector '[] (Const (Ptr CFloat))
colorTransformSelector = mkSelector "colorTransform"

