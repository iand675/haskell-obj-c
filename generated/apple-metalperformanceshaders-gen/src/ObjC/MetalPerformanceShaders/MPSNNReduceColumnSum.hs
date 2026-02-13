{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceColumnSum
--
-- The MPSNNReduceColumnSum performs a reduction operation returning the sum for each column of an image
--
-- Generated bindings for @MPSNNReduceColumnSum@.
module ObjC.MetalPerformanceShaders.MPSNNReduceColumnSum
  ( MPSNNReduceColumnSum
  , IsMPSNNReduceColumnSum(..)
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
-- Returns: A valid MPSNNReduceColumnSum object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceColumnSum mpsnnReduceColumnSum => mpsnnReduceColumnSum -> RawId -> IO (Id MPSNNReduceColumnSum)
initWithDevice mpsnnReduceColumnSum device =
  sendOwnedMessage mpsnnReduceColumnSum initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceColumnSum object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceColumnSum mpsnnReduceColumnSum, IsNSCoder aDecoder) => mpsnnReduceColumnSum -> aDecoder -> RawId -> IO (Id MPSNNReduceColumnSum)
initWithCoder_device mpsnnReduceColumnSum aDecoder device =
  sendOwnedMessage mpsnnReduceColumnSum initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceColumnSum)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceColumnSum)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

