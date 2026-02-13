{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceRowMax
--
-- The MPSNNReduceRowMax performs a reduction operation returning the maximum value for each row of an image
--
-- Generated bindings for @MPSNNReduceRowMax@.
module ObjC.MetalPerformanceShaders.MPSNNReduceRowMax
  ( MPSNNReduceRowMax
  , IsMPSNNReduceRowMax(..)
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
-- Returns: A valid MPSNNReduceRowMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceRowMax mpsnnReduceRowMax => mpsnnReduceRowMax -> RawId -> IO (Id MPSNNReduceRowMax)
initWithDevice mpsnnReduceRowMax device =
  sendOwnedMessage mpsnnReduceRowMax initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceRowMax object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceRowMax mpsnnReduceRowMax, IsNSCoder aDecoder) => mpsnnReduceRowMax -> aDecoder -> RawId -> IO (Id MPSNNReduceRowMax)
initWithCoder_device mpsnnReduceRowMax aDecoder device =
  sendOwnedMessage mpsnnReduceRowMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceRowMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceRowMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

