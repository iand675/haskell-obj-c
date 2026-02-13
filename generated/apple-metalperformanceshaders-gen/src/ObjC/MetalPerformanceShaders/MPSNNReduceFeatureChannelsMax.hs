{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsMax
--
-- The MPSNNReduceFeatureChannelsMax performs a reduction operation returning the maximum value for feature channels of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsMax@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsMax
  ( MPSNNReduceFeatureChannelsMax
  , IsMPSNNReduceFeatureChannelsMax(..)
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
-- Returns: A valid MPSNNReduceFeatureChannelsMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsMax mpsnnReduceFeatureChannelsMax => mpsnnReduceFeatureChannelsMax -> RawId -> IO (Id MPSNNReduceFeatureChannelsMax)
initWithDevice mpsnnReduceFeatureChannelsMax device =
  sendOwnedMessage mpsnnReduceFeatureChannelsMax initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsMax object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsMax mpsnnReduceFeatureChannelsMax, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsMax -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsMax)
initWithCoder_device mpsnnReduceFeatureChannelsMax aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

