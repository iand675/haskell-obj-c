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
  , initWithDeviceSelector
  , initWithCoder_deviceSelector


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
-- Returns: A valid MPSNNReduceColumnMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceColumnMax mpsnnReduceColumnMax => mpsnnReduceColumnMax -> RawId -> IO (Id MPSNNReduceColumnMax)
initWithDevice mpsnnReduceColumnMax  device =
  sendMsg mpsnnReduceColumnMax (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device mpsnnReduceColumnMax  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnReduceColumnMax (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

