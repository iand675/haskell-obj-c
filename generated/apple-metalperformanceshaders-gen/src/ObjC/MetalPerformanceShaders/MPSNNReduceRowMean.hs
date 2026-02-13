{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceRowMean
--
-- The MPSNNReduceRowMean performs a reduction operation returning the mean value for each row of an image
--
-- Generated bindings for @MPSNNReduceRowMean@.
module ObjC.MetalPerformanceShaders.MPSNNReduceRowMean
  ( MPSNNReduceRowMean
  , IsMPSNNReduceRowMean(..)
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
-- Returns: A valid MPSNNReduceRowMean object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceRowMean mpsnnReduceRowMean => mpsnnReduceRowMean -> RawId -> IO (Id MPSNNReduceRowMean)
initWithDevice mpsnnReduceRowMean device =
  sendOwnedMessage mpsnnReduceRowMean initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceRowMean object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceRowMean mpsnnReduceRowMean, IsNSCoder aDecoder) => mpsnnReduceRowMean -> aDecoder -> RawId -> IO (Id MPSNNReduceRowMean)
initWithCoder_device mpsnnReduceRowMean aDecoder device =
  sendOwnedMessage mpsnnReduceRowMean initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceRowMean)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceRowMean)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

