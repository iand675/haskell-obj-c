{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceRowSum
--
-- The MPSNNReduceRowSum performs a reduction operation returning the sum for each row of an image
--
-- Generated bindings for @MPSNNReduceRowSum@.
module ObjC.MetalPerformanceShaders.MPSNNReduceRowSum
  ( MPSNNReduceRowSum
  , IsMPSNNReduceRowSum(..)
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
-- Returns: A valid MPSNNReduceRowSum object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceRowSum mpsnnReduceRowSum => mpsnnReduceRowSum -> RawId -> IO (Id MPSNNReduceRowSum)
initWithDevice mpsnnReduceRowSum device =
  sendOwnedMessage mpsnnReduceRowSum initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceRowSum object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceRowSum mpsnnReduceRowSum, IsNSCoder aDecoder) => mpsnnReduceRowSum -> aDecoder -> RawId -> IO (Id MPSNNReduceRowSum)
initWithCoder_device mpsnnReduceRowSum aDecoder device =
  sendOwnedMessage mpsnnReduceRowSum initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceRowSum)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceRowSum)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

