{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceColumnMin
--
-- The MPSNNReduceColumnMin performs a reduction operation returning the mininmum value for each column of an image
--
-- Generated bindings for @MPSNNReduceColumnMin@.
module ObjC.MetalPerformanceShaders.MPSNNReduceColumnMin
  ( MPSNNReduceColumnMin
  , IsMPSNNReduceColumnMin(..)
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
-- Returns: A valid MPSNNReduceColumnMin object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceColumnMin mpsnnReduceColumnMin => mpsnnReduceColumnMin -> RawId -> IO (Id MPSNNReduceColumnMin)
initWithDevice mpsnnReduceColumnMin device =
  sendOwnedMessage mpsnnReduceColumnMin initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceColumnMin object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceColumnMin mpsnnReduceColumnMin, IsNSCoder aDecoder) => mpsnnReduceColumnMin -> aDecoder -> RawId -> IO (Id MPSNNReduceColumnMin)
initWithCoder_device mpsnnReduceColumnMin aDecoder device =
  sendOwnedMessage mpsnnReduceColumnMin initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceColumnMin)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReduceColumnMin)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

