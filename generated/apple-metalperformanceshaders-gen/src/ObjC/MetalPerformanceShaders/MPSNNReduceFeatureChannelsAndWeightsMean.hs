{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNReduceFeatureChannelsAndWeightsMean@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsAndWeightsMean
  ( MPSNNReduceFeatureChannelsAndWeightsMean
  , IsMPSNNReduceFeatureChannelsAndWeightsMean(..)
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
-- Returns: A valid MPSNNReduceFeatureChannelsAndWeightsMean object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsAndWeightsMean mpsnnReduceFeatureChannelsAndWeightsMean => mpsnnReduceFeatureChannelsAndWeightsMean -> RawId -> IO (Id MPSNNReduceFeatureChannelsAndWeightsMean)
initWithDevice mpsnnReduceFeatureChannelsAndWeightsMean device =
  sendOwnedMessage mpsnnReduceFeatureChannelsAndWeightsMean initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSCNNPooling object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsAndWeightsMean mpsnnReduceFeatureChannelsAndWeightsMean, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsAndWeightsMean -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsAndWeightsMean)
initWithCoder_device mpsnnReduceFeatureChannelsAndWeightsMean aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsAndWeightsMean initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsAndWeightsMean)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsAndWeightsMean)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

