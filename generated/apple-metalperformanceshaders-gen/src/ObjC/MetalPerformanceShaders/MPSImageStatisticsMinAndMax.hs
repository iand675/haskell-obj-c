{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageStatisticsMinAndMax
--
-- The MPSImageStatisticsMinAndMax computes the minimum and maximum pixel values for a given region of an image.              The min and max values are written to the destination image at the following pixel locations:                  - min value is written at pixel location (0, 0)                  - max value is written at pixel location (1, 0)
--
-- Generated bindings for @MPSImageStatisticsMinAndMax@.
module ObjC.MetalPerformanceShaders.MPSImageStatisticsMinAndMax
  ( MPSImageStatisticsMinAndMax
  , IsMPSImageStatisticsMinAndMax(..)
  , initWithDevice
  , initWithCoder_device
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies information to apply the statistics min-max operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSImageStatisticsMinAndMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageStatisticsMinAndMax mpsImageStatisticsMinAndMax => mpsImageStatisticsMinAndMax -> RawId -> IO (Id MPSImageStatisticsMinAndMax)
initWithDevice mpsImageStatisticsMinAndMax device =
  sendOwnedMessage mpsImageStatisticsMinAndMax initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSImageStatisticsMinAndMax mpsImageStatisticsMinAndMax, IsNSCoder aDecoder) => mpsImageStatisticsMinAndMax -> aDecoder -> RawId -> IO (Id MPSImageStatisticsMinAndMax)
initWithCoder_device mpsImageStatisticsMinAndMax aDecoder device =
  sendOwnedMessage mpsImageStatisticsMinAndMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageStatisticsMinAndMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageStatisticsMinAndMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

