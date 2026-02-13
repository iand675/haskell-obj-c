{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsArgumentMin
--
-- The MPSNNReduceFeatureChannelsArgumentMin returns the argument index that is the              location of the minimum value for feature channels of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsArgumentMin@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsArgumentMin
  ( MPSNNReduceFeatureChannelsArgumentMin
  , IsMPSNNReduceFeatureChannelsArgumentMin(..)
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
-- Returns: A valid MPSNNReduceFeatureChannelsArgumentMin object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsArgumentMin mpsnnReduceFeatureChannelsArgumentMin => mpsnnReduceFeatureChannelsArgumentMin -> RawId -> IO (Id MPSNNReduceFeatureChannelsArgumentMin)
initWithDevice mpsnnReduceFeatureChannelsArgumentMin device =
  sendOwnedMessage mpsnnReduceFeatureChannelsArgumentMin initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsArgumentMin object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsArgumentMin mpsnnReduceFeatureChannelsArgumentMin, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsArgumentMin -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsArgumentMin)
initWithCoder_device mpsnnReduceFeatureChannelsArgumentMin aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsArgumentMin initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsArgumentMin)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsArgumentMin)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

