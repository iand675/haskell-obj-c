{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNSlice@.
module ObjC.MetalPerformanceShaders.MPSNNSlice
  ( MPSNNSlice
  , IsMPSNNSlice(..)
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

-- | Initialize a MPSNNSlice kernel
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNSlice object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNSlice mpsnnSlice => mpsnnSlice -> RawId -> IO (Id MPSNNSlice)
initWithDevice mpsnnSlice device =
  sendOwnedMessage mpsnnSlice initWithDeviceSelector device

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNSlice mpsnnSlice, IsNSCoder aDecoder) => mpsnnSlice -> aDecoder -> RawId -> IO (Id MPSNNSlice)
initWithCoder_device mpsnnSlice aDecoder device =
  sendOwnedMessage mpsnnSlice initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNSlice)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNSlice)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

