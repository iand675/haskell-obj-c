{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsSum
--
-- The MPSNNReduceFeatureChannelsSum performs a reduction operation returning the sum for each column of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsSum@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsSum
  ( MPSNNReduceFeatureChannelsSum
  , IsMPSNNReduceFeatureChannelsSum(..)
  , initWithDevice
  , initWithCoder_device
  , weight
  , setWeight
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , setWeightSelector
  , weightSelector


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
-- Returns: A valid MPSNNReduceFeatureChannelsSum object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum => mpsnnReduceFeatureChannelsSum -> RawId -> IO (Id MPSNNReduceFeatureChannelsSum)
initWithDevice mpsnnReduceFeatureChannelsSum device =
  sendOwnedMessage mpsnnReduceFeatureChannelsSum initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsSum object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsSum -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsSum)
initWithCoder_device mpsnnReduceFeatureChannelsSum aDecoder device =
  sendOwnedMessage mpsnnReduceFeatureChannelsSum initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | weight
--
-- The scale factor to apply to each feature channel value
--
-- Each feature channel is multiplied by the weight value to compute a weighted sum or mean across feature channels              The default value is 1.0.
--
-- ObjC selector: @- weight@
weight :: IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum => mpsnnReduceFeatureChannelsSum -> IO CFloat
weight mpsnnReduceFeatureChannelsSum =
  sendMessage mpsnnReduceFeatureChannelsSum weightSelector

-- | weight
--
-- The scale factor to apply to each feature channel value
--
-- Each feature channel is multiplied by the weight value to compute a weighted sum or mean across feature channels              The default value is 1.0.
--
-- ObjC selector: @- setWeight:@
setWeight :: IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum => mpsnnReduceFeatureChannelsSum -> CFloat -> IO ()
setWeight mpsnnReduceFeatureChannelsSum value =
  sendMessage mpsnnReduceFeatureChannelsSum setWeightSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceFeatureChannelsSum)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceFeatureChannelsSum)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CFloat
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector '[CFloat] ()
setWeightSelector = mkSelector "setWeight:"

