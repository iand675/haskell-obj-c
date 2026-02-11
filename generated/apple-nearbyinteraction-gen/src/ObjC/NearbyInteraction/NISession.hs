{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Nearby interaction session.
--
-- Generated bindings for @NISession@.
module ObjC.NearbyInteraction.NISession
  ( NISession
  , IsNISession(..)
  , runWithConfiguration
  , pause
  , invalidate
  , setARSession
  , supported
  , deviceCapabilities
  , delegate
  , setDelegate
  , delegateQueue
  , setDelegateQueue
  , discoveryToken
  , configuration
  , runWithConfigurationSelector
  , pauseSelector
  , invalidateSelector
  , setARSessionSelector
  , supportedSelector
  , deviceCapabilitiesSelector
  , delegateSelector
  , setDelegateSelector
  , delegateQueueSelector
  , setDelegateQueueSelector
  , discoveryTokenSelector
  , configurationSelector


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

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Start a nearby interaction session.
--
-- @configuration@ — Nearby interaction configuration for this session. Both devices must call -runWithConfiguration: with a valid configuration identifying the other device in order to receive nearby object updates.
--
-- ObjC selector: @- runWithConfiguration:@
runWithConfiguration :: (IsNISession niSession, IsNIConfiguration configuration) => niSession -> configuration -> IO ()
runWithConfiguration niSession  configuration =
  withObjCPtr configuration $ \raw_configuration ->
      sendMsg niSession (mkSelector "runWithConfiguration:") retVoid [argPtr (castPtr raw_configuration :: Ptr ())]

-- | Pause an ongoing nearby interaction session.
--
-- Paused sessions may be restarted by calling -runWithConfiguration:. The same local discoveryToken will be used.
--
-- ObjC selector: @- pause@
pause :: IsNISession niSession => niSession -> IO ()
pause niSession  =
    sendMsg niSession (mkSelector "pause") retVoid []

-- | Invalidate an ongoing nearby interaction session.
--
-- Invalidate sessions you wish to terminate and do not intend to restart. A peer device in a nearby interaction session will receive a callback to -didRemoveNearbyObject:withReason: some time after a call to invalidate (see NINearbyObjectRemovalReason). calling -runWithConfiguration: after invalidation will result in an error.
--
-- ObjC selector: @- invalidate@
invalidate :: IsNISession niSession => niSession -> IO ()
invalidate niSession  =
    sendMsg niSession (mkSelector "invalidate") retVoid []

-- | Provide an ARSession object for use with the NISession
--
-- @session@ — The ARSession to use for camera assistance
--
-- If not provided, an ARSession will be created automatically if the cameraAssistanceEnabled property on the configuration is YES
--
-- The developer is responsible for running the ARSession if provided.
--
-- If the ARConfiguration used to run the session is not compatible with the NISession, the NISession will invalidate with error
--
-- If the platform does not support camera assistance or an ARSession is provided without enabling cameraAssistanceEnabled property in the NIConfiguration, the NISession will invalidate with error (see NIError.h)
--
-- ObjC selector: @- setARSession:@
setARSession :: IsNISession niSession => niSession -> RawId -> IO ()
setARSession niSession  session =
    sendMsg niSession (mkSelector "setARSession:") retVoid [argPtr (castPtr (unRawId session) :: Ptr ())]

-- | Whether or not this device is capable of participating in a nearby interaction session.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "NISession"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | Get the protocol that describes nearby interaction capabilities on this device.
--
-- Detailed description on the capability protocol is in NIDeviceCapability.h.
--
-- ObjC selector: @+ deviceCapabilities@
deviceCapabilities :: IO RawId
deviceCapabilities  =
  do
    cls' <- getRequiredClass "NISession"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "deviceCapabilities") (retPtr retVoid) []

-- | A delegate for receiving NISession updates.
--
-- ObjC selector: @- delegate@
delegate :: IsNISession niSession => niSession -> IO RawId
delegate niSession  =
    fmap (RawId . castPtr) $ sendMsg niSession (mkSelector "delegate") (retPtr retVoid) []

-- | A delegate for receiving NISession updates.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNISession niSession => niSession -> RawId -> IO ()
setDelegate niSession  value =
    sendMsg niSession (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The dispatch queue on which the delegate calls are performed.
--
-- If not provided or nil, delegate calls will be performed on the main queue.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsNISession niSession => niSession -> IO (Id NSObject)
delegateQueue niSession  =
    sendMsg niSession (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The dispatch queue on which the delegate calls are performed.
--
-- If not provided or nil, delegate calls will be performed on the main queue.
--
-- ObjC selector: @- setDelegateQueue:@
setDelegateQueue :: (IsNISession niSession, IsNSObject value) => niSession -> value -> IO ()
setDelegateQueue niSession  value =
  withObjCPtr value $ \raw_value ->
      sendMsg niSession (mkSelector "setDelegateQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A unique nearby interaction identifier for this session.
--
-- Copy this discoveryToken and share it with a peer device. The discoveryToken is unique to this device and this session.
--
-- ObjC selector: @- discoveryToken@
discoveryToken :: IsNISession niSession => niSession -> IO (Id NIDiscoveryToken)
discoveryToken niSession  =
    sendMsg niSession (mkSelector "discoveryToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The nearby interaction configuration currently being used by the session.
--
-- ObjC selector: @- configuration@
configuration :: IsNISession niSession => niSession -> IO (Id NIConfiguration)
configuration niSession  =
    sendMsg niSession (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWithConfiguration:@
runWithConfigurationSelector :: Selector
runWithConfigurationSelector = mkSelector "runWithConfiguration:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @setARSession:@
setARSessionSelector :: Selector
setARSessionSelector = mkSelector "setARSession:"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @deviceCapabilities@
deviceCapabilitiesSelector :: Selector
deviceCapabilitiesSelector = mkSelector "deviceCapabilities"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @setDelegateQueue:@
setDelegateQueueSelector :: Selector
setDelegateQueueSelector = mkSelector "setDelegateQueue:"

-- | @Selector@ for @discoveryToken@
discoveryTokenSelector :: Selector
discoveryTokenSelector = mkSelector "discoveryToken"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

