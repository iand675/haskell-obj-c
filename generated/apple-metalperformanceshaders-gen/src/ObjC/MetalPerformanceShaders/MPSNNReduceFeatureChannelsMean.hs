{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsMean
--
-- The MPSNNReduceFeatureChannelsMean performs a reduction operation returning the mean value for each column of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsMean@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsMean
  ( MPSNNReduceFeatureChannelsMean
  , IsMPSNNReduceFeatureChannelsMean(..)
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
-- Returns: A valid MPSNNReduceFeatureChannelsMean object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsMean mpsnnReduceFeatureChannelsMean => mpsnnReduceFeatureChannelsMean -> RawId -> IO (Id MPSNNReduceFeatureChannelsMean)
initWithDevice mpsnnReduceFeatureChannelsMean device =
  sendOwnedMessage mpsnnReduceFeatureChannelsMean initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsMean object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsMean mpsnnReduceFeatureChannelsMean, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsMean -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsMean)
initWithCoder_device mpsnnReduceFeatureChannelsMean aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsMean initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsMean)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsMean)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

