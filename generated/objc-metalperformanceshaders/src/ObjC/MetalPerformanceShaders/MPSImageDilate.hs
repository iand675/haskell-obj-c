{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageDilate
--
-- The MPSImageDilate finds the maximum pixel value in a rectangular region centered around each pixel in the              source image. It is like the MPSImageAreaMax, except that the intensity at each position is calculated relative              to a different value before determining which is the maximum pixel value, allowing for shaped, non-rectangular              morphological probes.
--
-- for each pixel in the filter window:
-- value =  pixel[filterY][filterX] - filter[filterY*filter_width+filterX]
-- if( value > bestValue ){
-- result = value
-- bestValue = value;
-- }
--
-- A filter that contains all zeros and is identical to a MPSImageAreaMax filter.  The center filter element              is assumed to be 0 to avoid causing a general darkening of the image.
--
-- The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
--
-- Generated bindings for @MPSImageDilate@.
module ObjC.MetalPerformanceShaders.MPSImageDilate
  ( MPSImageDilate
  , IsMPSImageDilate(..)
  , initWithDevice_kernelWidth_kernelHeight_values
  , initWithDevice
  , initWithCoder_device
  , kernelHeight
  , kernelWidth
  , initWithDevice_kernelWidth_kernelHeight_valuesSelector
  , initWithDeviceSelector
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

-- | Init a object with kernel height, width and weight values.
--
-- Each dilate shape probe defines a 3D surface of values.              These are arranged in order left to right, then top to bottom              in a 1D array. (values[kernelWidth*y+x] = probe[y][x])              Values should be generally be in the range [0,1] with the center               pixel tending towards 0 and edges towards 1. However, any numerical              value is allowed. Calculations are subject to the usual floating-point              rounding error.
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel. Must be an odd number.
--
-- @kernelHeight@ — The height of the kernel. Must be an odd number.
--
-- @values@ — The set of values to use as the dilate probe.                                  The values are copied into the filter. To avoid                                   image ligthening or darkening, the center value should                                  be 0.0f.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:values:@
initWithDevice_kernelWidth_kernelHeight_values :: IsMPSImageDilate mpsImageDilate => mpsImageDilate -> RawId -> CULong -> CULong -> Const (Ptr CFloat) -> IO (Id MPSImageDilate)
initWithDevice_kernelWidth_kernelHeight_values mpsImageDilate  device kernelWidth kernelHeight values =
  sendMsg mpsImageDilate (mkSelector "initWithDevice:kernelWidth:kernelHeight:values:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argPtr (unConst values)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageDilate mpsImageDilate => mpsImageDilate -> RawId -> IO (Id MPSImageDilate)
initWithDevice mpsImageDilate  device =
  sendMsg mpsImageDilate (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageDilate mpsImageDilate, IsNSCoder aDecoder) => mpsImageDilate -> aDecoder -> RawId -> IO (Id MPSImageDilate)
initWithCoder_device mpsImageDilate  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageDilate (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | kernelHeight
--
-- The height of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSImageDilate mpsImageDilate => mpsImageDilate -> IO CULong
kernelHeight mpsImageDilate  =
  sendMsg mpsImageDilate (mkSelector "kernelHeight") retCULong []

-- | kernelWidth
--
-- The width of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSImageDilate mpsImageDilate => mpsImageDilate -> IO CULong
kernelWidth mpsImageDilate  =
  sendMsg mpsImageDilate (mkSelector "kernelWidth") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:values:@
initWithDevice_kernelWidth_kernelHeight_valuesSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_valuesSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:values:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

