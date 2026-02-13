{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object to describe and configure parameters to be used in a nearby interaction session for mutual relative positional measurements.
--
-- Devices engaged in a session run with an NINearbyPeerConfiguration are able to continuously generate positional measurements relative to one another.
--
-- Generated bindings for @NINearbyPeerConfiguration@.
module ObjC.NearbyInteraction.NINearbyPeerConfiguration
  ( NINearbyPeerConfiguration
  , IsNINearbyPeerConfiguration(..)
  , initWithPeerToken
  , init_
  , new
  , peerDiscoveryToken
  , cameraAssistanceEnabled
  , setCameraAssistanceEnabled
  , extendedDistanceMeasurementEnabled
  , setExtendedDistanceMeasurementEnabled
  , cameraAssistanceEnabledSelector
  , extendedDistanceMeasurementEnabledSelector
  , initSelector
  , initWithPeerTokenSelector
  , newSelector
  , peerDiscoveryTokenSelector
  , setCameraAssistanceEnabledSelector
  , setExtendedDistanceMeasurementEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new configuration with the provided peer token.
--
-- @peerToken@ — A discovery token received from the peer for this session.
--
-- ObjC selector: @- initWithPeerToken:@
initWithPeerToken :: (IsNINearbyPeerConfiguration niNearbyPeerConfiguration, IsNIDiscoveryToken peerToken) => niNearbyPeerConfiguration -> peerToken -> IO (Id NINearbyPeerConfiguration)
initWithPeerToken niNearbyPeerConfiguration peerToken =
  sendOwnedMessage niNearbyPeerConfiguration initWithPeerTokenSelector (toNIDiscoveryToken peerToken)

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO (Id NINearbyPeerConfiguration)
init_ niNearbyPeerConfiguration =
  sendOwnedMessage niNearbyPeerConfiguration initSelector

-- | @+ new@
new :: IO (Id NINearbyPeerConfiguration)
new  =
  do
    cls' <- getRequiredClass "NINearbyPeerConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | The discovery token identifying the peer device for this session configuration.
--
-- ObjC selector: @- peerDiscoveryToken@
peerDiscoveryToken :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO (Id NIDiscoveryToken)
peerDiscoveryToken niNearbyPeerConfiguration =
  sendMessage niNearbyPeerConfiguration peerDiscoveryTokenSelector

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If true, optionally call setARSession: on the NISession before calling runWithConfiguration: If true and setARSession: is not called, an ARSession will automatically be created If true and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- cameraAssistanceEnabled@
cameraAssistanceEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO Bool
cameraAssistanceEnabled niNearbyPeerConfiguration =
  sendMessage niNearbyPeerConfiguration cameraAssistanceEnabledSelector

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If true, optionally call setARSession: on the NISession before calling runWithConfiguration: If true and setARSession: is not called, an ARSession will automatically be created If true and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- setCameraAssistanceEnabled:@
setCameraAssistanceEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> Bool -> IO ()
setCameraAssistanceEnabled niNearbyPeerConfiguration value =
  sendMessage niNearbyPeerConfiguration setCameraAssistanceEnabledSelector value

-- | If both peers are capable, enables extended distance measurement for the NISession that runs with this configuration
--
-- :If true, the NISession will use extended distance measurement capabilities while ranging with a peer that is also capable of extended distance measurementThis property is compatible with the cameraAssistanceEnabled property
--
-- Note: : Check supportsExtendedDistanceMeasurement property from deviceCapabilities properties on NISession and the deviceCapabilities property on the NIDiscoveryToken generated by the peer device to understand mutual capabilities
--
-- ObjC selector: @- extendedDistanceMeasurementEnabled@
extendedDistanceMeasurementEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> IO Bool
extendedDistanceMeasurementEnabled niNearbyPeerConfiguration =
  sendMessage niNearbyPeerConfiguration extendedDistanceMeasurementEnabledSelector

-- | If both peers are capable, enables extended distance measurement for the NISession that runs with this configuration
--
-- :If true, the NISession will use extended distance measurement capabilities while ranging with a peer that is also capable of extended distance measurementThis property is compatible with the cameraAssistanceEnabled property
--
-- Note: : Check supportsExtendedDistanceMeasurement property from deviceCapabilities properties on NISession and the deviceCapabilities property on the NIDiscoveryToken generated by the peer device to understand mutual capabilities
--
-- ObjC selector: @- setExtendedDistanceMeasurementEnabled:@
setExtendedDistanceMeasurementEnabled :: IsNINearbyPeerConfiguration niNearbyPeerConfiguration => niNearbyPeerConfiguration -> Bool -> IO ()
setExtendedDistanceMeasurementEnabled niNearbyPeerConfiguration value =
  sendMessage niNearbyPeerConfiguration setExtendedDistanceMeasurementEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeerToken:@
initWithPeerTokenSelector :: Selector '[Id NIDiscoveryToken] (Id NINearbyPeerConfiguration)
initWithPeerTokenSelector = mkSelector "initWithPeerToken:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NINearbyPeerConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NINearbyPeerConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @peerDiscoveryToken@
peerDiscoveryTokenSelector :: Selector '[] (Id NIDiscoveryToken)
peerDiscoveryTokenSelector = mkSelector "peerDiscoveryToken"

-- | @Selector@ for @cameraAssistanceEnabled@
cameraAssistanceEnabledSelector :: Selector '[] Bool
cameraAssistanceEnabledSelector = mkSelector "cameraAssistanceEnabled"

-- | @Selector@ for @setCameraAssistanceEnabled:@
setCameraAssistanceEnabledSelector :: Selector '[Bool] ()
setCameraAssistanceEnabledSelector = mkSelector "setCameraAssistanceEnabled:"

-- | @Selector@ for @extendedDistanceMeasurementEnabled@
extendedDistanceMeasurementEnabledSelector :: Selector '[] Bool
extendedDistanceMeasurementEnabledSelector = mkSelector "extendedDistanceMeasurementEnabled"

-- | @Selector@ for @setExtendedDistanceMeasurementEnabled:@
setExtendedDistanceMeasurementEnabledSelector :: Selector '[Bool] ()
setExtendedDistanceMeasurementEnabledSelector = mkSelector "setExtendedDistanceMeasurementEnabled:"

