{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceColumnMean
--
-- The MPSNNReduceColumnMean performs a reduction operation returning the mean value for each column of an image
--
-- Generated bindings for @MPSNNReduceColumnMean@.
module ObjC.MetalPerformanceShaders.MPSNNReduceColumnMean
  ( MPSNNReduceColumnMean
  , IsMPSNNReduceColumnMean(..)
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
-- Returns: A valid MPSNNReduceColumnMean object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceColumnMean mpsnnReduceColumnMean => mpsnnReduceColumnMean -> RawId -> IO (Id MPSNNReduceColumnMean)
initWithDevice mpsnnReduceColumnMean device =
  sendOwnedMessage mpsnnReduceColumnMean initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceColumnMean object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceColumnMean mpsnnReduceColumnMean, IsNSCoder aDecoder) => mpsnnReduceColumnMean -> aDecoder -> RawId -> IO (Id MPSNNReduceColumnMean)
initWithCoder_device mpsnnReduceColumnMean aDecoder device =
  sendOwnedMessage mpsnnReduceColumnMean initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceColumnMean)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceColumnMean)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

