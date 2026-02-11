{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceFeatureChannelsArgumentMin
--
-- The MPSNNReduceFeatureChannelsArgumentMin returns the argument index that is the              location of the minimum value for feature channels of an image
--
-- Generated bindings for @MPSNNReduceFeatureChannelsArgumentMin@.
module ObjC.MetalPerformanceShaders.MPSNNReduceFeatureChannelsArgumentMin
  ( MPSNNReduceFeatureChannelsArgumentMin
  , IsMPSNNReduceFeatureChannelsArgumentMin(..)
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
-- Returns: A valid MPSNNReduceFeatureChannelsArgumentMin object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReduceFeatureChannelsArgumentMin mpsnnReduceFeatureChannelsArgumentMin => mpsnnReduceFeatureChannelsArgumentMin -> RawId -> IO (Id MPSNNReduceFeatureChannelsArgumentMin)
initWithDevice mpsnnReduceFeatureChannelsArgumentMin  device =
  sendMsg mpsnnReduceFeatureChannelsArgumentMin (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSNNReduceFeatureChannelsArgumentMin object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReduceFeatureChannelsArgumentMin mpsnnReduceFeatureChannelsArgumentMin, IsNSCoder aDecoder) => mpsnnReduceFeatureChannelsArgumentMin -> aDecoder -> RawId -> IO (Id MPSNNReduceFeatureChannelsArgumentMin)
initWithCoder_device mpsnnReduceFeatureChannelsArgumentMin  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnReduceFeatureChannelsArgumentMin (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

