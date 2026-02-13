{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageStatisticsMean
--
-- The MPSImageStatisticsMean computes the mean for a given region of an image.
--
-- Generated bindings for @MPSImageStatisticsMean@.
module ObjC.MetalPerformanceShaders.MPSImageStatisticsMean
  ( MPSImageStatisticsMean
  , IsMPSImageStatisticsMean(..)
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

-- | Specifies information to apply the statistics mean operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSImageStatisticsMean object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageStatisticsMean mpsImageStatisticsMean => mpsImageStatisticsMean -> RawId -> IO (Id MPSImageStatisticsMean)
initWithDevice mpsImageStatisticsMean device =
  sendOwnedMessage mpsImageStatisticsMean initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSImageStatisticsMean mpsImageStatisticsMean, IsNSCoder aDecoder) => mpsImageStatisticsMean -> aDecoder -> RawId -> IO (Id MPSImageStatisticsMean)
initWithCoder_device mpsImageStatisticsMean aDecoder device =
  sendOwnedMessage mpsImageStatisticsMean initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageStatisticsMean)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageStatisticsMean)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

