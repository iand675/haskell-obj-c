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
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , weightSelector
  , setWeightSelector


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

-- | Specifies information to apply the reduction operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNReduceFeatureChannelsSum object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum => mpsnnReduceFeatureChannelsSum -> RawId -> IO (Id MPSNNReduceFeatureChannelsSum)
initWithDevice mpsnnReduceFeatureChannelsSum  device =
  sendMsg mpsnnReduceFeatureChannelsSum (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device mpsnnReduceFeatureChannelsSum  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnReduceFeatureChannelsSum (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | weight
--
-- The scale factor to apply to each feature channel value
--
-- Each feature channel is multiplied by the weight value to compute a weighted sum or mean across feature channels              The default value is 1.0.
--
-- ObjC selector: @- weight@
weight :: IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum => mpsnnReduceFeatureChannelsSum -> IO CFloat
weight mpsnnReduceFeatureChannelsSum  =
  sendMsg mpsnnReduceFeatureChannelsSum (mkSelector "weight") retCFloat []

-- | weight
--
-- The scale factor to apply to each feature channel value
--
-- Each feature channel is multiplied by the weight value to compute a weighted sum or mean across feature channels              The default value is 1.0.
--
-- ObjC selector: @- setWeight:@
setWeight :: IsMPSNNReduceFeatureChannelsSum mpsnnReduceFeatureChannelsSum => mpsnnReduceFeatureChannelsSum -> CFloat -> IO ()
setWeight mpsnnReduceFeatureChannelsSum  value =
  sendMsg mpsnnReduceFeatureChannelsSum (mkSelector "setWeight:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector
setWeightSelector = mkSelector "setWeight:"

