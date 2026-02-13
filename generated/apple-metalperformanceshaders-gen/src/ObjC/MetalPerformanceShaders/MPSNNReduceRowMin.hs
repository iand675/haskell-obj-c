{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceRowMin
--
-- The MPSNNReduceRowMin performs a reduction operation returning the mininmum value for each row of an image
--
-- Generated bindings for @MPSNNReduceRowMin@.
module ObjC.MetalPerformanceShaders.MPSNNReduceRowMin
  ( MPSNNReduceRowMin
  , IsMPSNNReduceRowMin(..)
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
-- Returns: A valid MPSNNReduceRowMin object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceRowMin mpsnnReduceRowMin => mpsnnReduceRowMin -> RawId -> IO (Id MPSNNReduceRowMin)
initWithDevice mpsnnReduceRowMin device =
  sendOwnedMessage mpsnnReduceRowMin initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceRowMin object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceRowMin mpsnnReduceRowMin, IsNSCoder aDecoder) => mpsnnReduceRowMin -> aDecoder -> RawId -> IO (Id MPSNNReduceRowMin)
initWithCoder_device mpsnnReduceRowMin aDecoder device =
  sendOwnedMessage mpsnnReduceRowMin initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceRowMin)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceRowMin)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

