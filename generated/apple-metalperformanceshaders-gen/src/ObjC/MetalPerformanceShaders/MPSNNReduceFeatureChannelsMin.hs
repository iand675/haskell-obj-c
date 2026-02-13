{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsMin
--
-- The MPSNNReduceFeatureChannelsMin performs a reduction operation returning the mininmum value for feature channels of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsMin@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsMin
  ( MPSNNReduceFeatureChannelsMin
  , IsMPSNNReduceFeatureChannelsMin(..)
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
-- Returns: A valid MPSNNReduceFeatureChannelsMin object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsMin mpsnnReduceFeatureChannelsMin => mpsnnReduceFeatureChannelsMin -> RawId -> IO (Id MPSNNReduceFeatureChannelsMin)
initWithDevice mpsnnReduceFeatureChannelsMin device =
  sendOwnedMessage mpsnnReduceFeatureChannelsMin initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsMin object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsMin mpsnnReduceFeatureChannelsMin, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsMin -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsMin)
initWithCoder_device mpsnnReduceFeatureChannelsMin aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsMin initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsMin)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsMin)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

