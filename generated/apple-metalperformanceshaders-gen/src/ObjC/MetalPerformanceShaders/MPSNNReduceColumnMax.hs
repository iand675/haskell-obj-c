{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceColumnMax
--
-- The MPSNNReduceColumnMax performs a reduction operation returning the maximum value for each column of an image
--
-- Generated bindings for @MPSNNReduceColumnMax@.
module ObjC.MetalPerformanceShaders.MPSNNReduceColumnMax
  ( MPSNNReduceColumnMax
  , IsMPSNNReduceColumnMax(..)
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
-- Returns: A valid MPSNNReduceColumnMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceColumnMax mpsnnReduceColumnMax => mpsnnReduceColumnMax -> RawId -> IO (Id MPSNNReduceColumnMax)
initWithDevice mpsnnReduceColumnMax device =
  sendOwnedMessage mpsnnReduceColumnMax initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceColumnMax object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceColumnMax mpsnnReduceColumnMax, IsNSCoder aDecoder) => mpsnnReduceColumnMax -> aDecoder -> RawId -> IO (Id MPSNNReduceColumnMax)
initWithCoder_device mpsnnReduceColumnMax aDecoder device =
  sendOwnedMessage mpsnnReduceColumnMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceColumnMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceColumnMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

