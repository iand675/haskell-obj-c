{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageStatisticsMeanAndVariance
--
-- The MPSImageStatisticsMeanAndVariance computes the mean and variance for a given region of an image.              The mean and variance values are written to the destination image at the following pixel locations:                  - mean value is written at pixel location (0, 0)                  - variance value is written at pixel location (1, 0)
--
-- Generated bindings for @MPSImageStatisticsMeanAndVariance@.
module ObjC.MetalPerformanceShaders.MPSImageStatisticsMeanAndVariance
  ( MPSImageStatisticsMeanAndVariance
  , IsMPSImageStatisticsMeanAndVariance(..)
  , initWithDevice
  , initWithCoder_device
  , initWithDeviceSelector
  , initWithCoder_deviceSelector


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

-- | Specifies information to apply the statistics mean operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSImageStatisticsMeanAndVariance object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageStatisticsMeanAndVariance mpsImageStatisticsMeanAndVariance => mpsImageStatisticsMeanAndVariance -> RawId -> IO (Id MPSImageStatisticsMeanAndVariance)
initWithDevice mpsImageStatisticsMeanAndVariance  device =
  sendMsg mpsImageStatisticsMeanAndVariance (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageStatisticsMeanAndVariance mpsImageStatisticsMeanAndVariance, IsNSCoder aDecoder) => mpsImageStatisticsMeanAndVariance -> aDecoder -> RawId -> IO (Id MPSImageStatisticsMeanAndVariance)
initWithCoder_device mpsImageStatisticsMeanAndVariance  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageStatisticsMeanAndVariance (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

