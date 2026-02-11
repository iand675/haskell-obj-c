{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageMedian
--
-- The MPSImageMedian applies a median filter to an image.  A median filter finds the               median color value for each channel within a kernelDiameter x kernelDiameter               window surrounding the pixel of interest.  It is a common means of noise reduction              and also as a smoothing filter with edge preserving qualities.
--
-- NOTE: The MPSImageMedian filter currently only supports images with <= 8 bits/channel.
--
-- Generated bindings for @MPSImageMedian@.
module ObjC.MetalPerformanceShaders.MPSImageMedian
  ( MPSImageMedian
  , IsMPSImageMedian(..)
  , initWithDevice_kernelDiameter
  , initWithCoder_device
  , initWithDevice
  , maxKernelDiameter
  , minKernelDiameter
  , kernelDiameter
  , initWithDevice_kernelDiameterSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , maxKernelDiameterSelector
  , minKernelDiameterSelector
  , kernelDiameterSelector


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

-- | Initialize a filter for a particular kernel size and device
--
-- @device@ — The device the filter will run on
--
-- @kernelDiameter@ — Diameter of the median filter. Must be an odd number.
--
-- Returns: A valid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelDiameter:@
initWithDevice_kernelDiameter :: IsMPSImageMedian mpsImageMedian => mpsImageMedian -> RawId -> CULong -> IO (Id MPSImageMedian)
initWithDevice_kernelDiameter mpsImageMedian  device kernelDiameter =
  sendMsg mpsImageMedian (mkSelector "initWithDevice:kernelDiameter:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelDiameter)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageMedian mpsImageMedian, IsNSCoder aDecoder) => mpsImageMedian -> aDecoder -> RawId -> IO (Id MPSImageMedian)
initWithCoder_device mpsImageMedian  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageMedian (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageMedian mpsImageMedian => mpsImageMedian -> RawId -> IO (Id MPSImageMedian)
initWithDevice mpsImageMedian  device =
  sendMsg mpsImageMedian (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | The maximum diameter in pixels of the filter window supported by the median filter.
--
-- ObjC selector: @+ maxKernelDiameter@
maxKernelDiameter :: IO CULong
maxKernelDiameter  =
  do
    cls' <- getRequiredClass "MPSImageMedian"
    sendClassMsg cls' (mkSelector "maxKernelDiameter") retCULong []

-- | The minimum diameter in pixels of the filter window supported by the median filter.
--
-- ObjC selector: @+ minKernelDiameter@
minKernelDiameter :: IO CULong
minKernelDiameter  =
  do
    cls' <- getRequiredClass "MPSImageMedian"
    sendClassMsg cls' (mkSelector "minKernelDiameter") retCULong []

-- | kernelDiameter
--
-- The diameter in pixels of the filter window.
--
-- The median filter is applied to a kernelDiameter x kernelDiameter window              of pixels centered on the corresponding source pixel for each destination              pixel.  The kernel diameter must be an odd number.
--
-- ObjC selector: @- kernelDiameter@
kernelDiameter :: IsMPSImageMedian mpsImageMedian => mpsImageMedian -> IO CULong
kernelDiameter mpsImageMedian  =
  sendMsg mpsImageMedian (mkSelector "kernelDiameter") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelDiameter:@
initWithDevice_kernelDiameterSelector :: Selector
initWithDevice_kernelDiameterSelector = mkSelector "initWithDevice:kernelDiameter:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @maxKernelDiameter@
maxKernelDiameterSelector :: Selector
maxKernelDiameterSelector = mkSelector "maxKernelDiameter"

-- | @Selector@ for @minKernelDiameter@
minKernelDiameterSelector :: Selector
minKernelDiameterSelector = mkSelector "minKernelDiameter"

-- | @Selector@ for @kernelDiameter@
kernelDiameterSelector :: Selector
kernelDiameterSelector = mkSelector "kernelDiameter"

