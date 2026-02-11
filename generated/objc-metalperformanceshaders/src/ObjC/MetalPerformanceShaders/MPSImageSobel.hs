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
  , initWithDeviceSelector
  , initWithDevice_linearGrayColorTransformSelector
  , initWithCoder_deviceSelector
  , colorTransformSelector


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
initWithDevice mpsImageSobel  device =
  sendMsg mpsImageSobel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_linearGrayColorTransform mpsImageSobel  device transform =
  sendMsg mpsImageSobel (mkSelector "initWithDevice:linearGrayColorTransform:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (unConst transform)] >>= ownedObject . castPtr

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
initWithCoder_device mpsImageSobel  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageSobel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | colorTransform
--
-- Returns a pointer to the array of three floats used to convert RGBA, RGB or RG images               to the destination format when the destination is monochrome.
--
-- ObjC selector: @- colorTransform@
colorTransform :: IsMPSImageSobel mpsImageSobel => mpsImageSobel -> IO (Const (Ptr CFloat))
colorTransform mpsImageSobel  =
  fmap Const $ fmap castPtr $ sendMsg mpsImageSobel (mkSelector "colorTransform") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:linearGrayColorTransform:@
initWithDevice_linearGrayColorTransformSelector :: Selector
initWithDevice_linearGrayColorTransformSelector = mkSelector "initWithDevice:linearGrayColorTransform:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @colorTransform@
colorTransformSelector :: Selector
colorTransformSelector = mkSelector "colorTransform"

