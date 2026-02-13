{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsArgumentMax
--
-- The MPSNNReduceFeatureChannelsArgumentMax performs returns the argument index that is the              location of the maximum value for feature channels of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsArgumentMax@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsArgumentMax
  ( MPSNNReduceFeatureChannelsArgumentMax
  , IsMPSNNReduceFeatureChannelsArgumentMax(..)
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

-- | Specifies information to apply the reduction operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNReduceFeatureChannelsArgumentMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsArgumentMax mpsnnReduceFeatureChannelsArgumentMax => mpsnnReduceFeatureChannelsArgumentMax -> RawId -> IO (Id MPSNNReduceFeatureChannelsArgumentMax)
initWithDevice mpsnnReduceFeatureChannelsArgumentMax device =
  sendOwnedMessage mpsnnReduceFeatureChannelsArgumentMax initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsArgumentMax object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsArgumentMax mpsnnReduceFeatureChannelsArgumentMax, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsArgumentMax -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsArgumentMax)
initWithCoder_device mpsnnReduceFeatureChannelsArgumentMax aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsArgumentMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsArgumentMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsArgumentMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

