{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A session configuration that enables interaction with supported accessories.
--
-- Generated bindings for @NINearbyAccessoryConfiguration@.
module ObjC.NearbyInteraction.NINearbyAccessoryConfiguration
  ( NINearbyAccessoryConfiguration
  , IsNINearbyAccessoryConfiguration(..)
  , initWithData_error
  , initWithAccessoryData_bluetoothPeerIdentifier_error
  , init_
  , new
  , accessoryDiscoveryToken
  , cameraAssistanceEnabled
  , setCameraAssistanceEnabled
  , accessoryDiscoveryTokenSelector
  , cameraAssistanceEnabledSelector
  , initSelector
  , initWithAccessoryData_bluetoothPeerIdentifier_errorSelector
  , initWithData_errorSelector
  , newSelector
  , setCameraAssistanceEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new nearby accessory configuration using data received from the accessory.
--
-- @data@ — Configuration data received from the accessory.
--
-- @error@ — An optional out error parameter that will be populated with an error if the provided data is invalid or unsupported.
--
-- ObjC selector: @- initWithData:error:@
initWithData_error :: (IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration, IsNSData data_, IsNSError error_) => niNearbyAccessoryConfiguration -> data_ -> error_ -> IO (Id NINearbyAccessoryConfiguration)
initWithData_error niNearbyAccessoryConfiguration data_ error_ =
  sendOwnedMessage niNearbyAccessoryConfiguration initWithData_errorSelector (toNSData data_) (toNSError error_)

-- | Create a new nearby accessory configuration for an accessory that is also a paired Bluetooth device
--
-- @accessoryData@ — Configuration data received from the accessory
--
-- @bluetoothPeerIdentifier@ — The accessory's Bluetooth identifier
--
-- @error@ — An optional out error parameter that will be populated with an error if the provided inputs are invalid or unsupported.
--
-- The accessory must be a Bluetooth LE peripheral that is paired, actively connected, and implements the Nearby Interaction Service and Accessory Configuration Characteristic.
--
-- ObjC selector: @- initWithAccessoryData:bluetoothPeerIdentifier:error:@
initWithAccessoryData_bluetoothPeerIdentifier_error :: (IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration, IsNSData accessoryData, IsNSUUID identifier, IsNSError error_) => niNearbyAccessoryConfiguration -> accessoryData -> identifier -> error_ -> IO (Id NINearbyAccessoryConfiguration)
initWithAccessoryData_bluetoothPeerIdentifier_error niNearbyAccessoryConfiguration accessoryData identifier error_ =
  sendOwnedMessage niNearbyAccessoryConfiguration initWithAccessoryData_bluetoothPeerIdentifier_errorSelector (toNSData accessoryData) (toNSUUID identifier) (toNSError error_)

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> IO (Id NINearbyAccessoryConfiguration)
init_ niNearbyAccessoryConfiguration =
  sendOwnedMessage niNearbyAccessoryConfiguration initSelector

-- | @+ new@
new :: IO (Id NINearbyAccessoryConfiguration)
new  =
  do
    cls' <- getRequiredClass "NINearbyAccessoryConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | The discovery token identifying the accessory device for this session configuration.
--
-- NINearbyObject updates for this accessory will contain this discovery token.
--
-- ObjC selector: @- accessoryDiscoveryToken@
accessoryDiscoveryToken :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> IO (Id NIDiscoveryToken)
accessoryDiscoveryToken niNearbyAccessoryConfiguration =
  sendMessage niNearbyAccessoryConfiguration accessoryDiscoveryTokenSelector

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If YES, optionally call -setARSession: on the NISession before calling -runWithConfiguration: If YES and setARSession: is not called, an ARSession will automatically be created If YES  and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- cameraAssistanceEnabled@
cameraAssistanceEnabled :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> IO Bool
cameraAssistanceEnabled niNearbyAccessoryConfiguration =
  sendMessage niNearbyAccessoryConfiguration cameraAssistanceEnabledSelector

-- | Enables camera assistance during the NISession run with this configuration
--
-- : If YES, optionally call -setARSession: on the NISession before calling -runWithConfiguration: If YES and setARSession: is not called, an ARSession will automatically be created If YES  and the platform does not support camera assistance, the NISession will generate an error when runWithConfiguration: is called
--
-- Note: : Check supportsCameraAssistance property in NIDeviceCapability returned from deviceCapabilities properties on NISession
--
-- ObjC selector: @- setCameraAssistanceEnabled:@
setCameraAssistanceEnabled :: IsNINearbyAccessoryConfiguration niNearbyAccessoryConfiguration => niNearbyAccessoryConfiguration -> Bool -> IO ()
setCameraAssistanceEnabled niNearbyAccessoryConfiguration value =
  sendMessage niNearbyAccessoryConfiguration setCameraAssistanceEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id NINearbyAccessoryConfiguration)
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @initWithAccessoryData:bluetoothPeerIdentifier:error:@
initWithAccessoryData_bluetoothPeerIdentifier_errorSelector :: Selector '[Id NSData, Id NSUUID, Id NSError] (Id NINearbyAccessoryConfiguration)
initWithAccessoryData_bluetoothPeerIdentifier_errorSelector = mkSelector "initWithAccessoryData:bluetoothPeerIdentifier:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NINearbyAccessoryConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NINearbyAccessoryConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @accessoryDiscoveryToken@
accessoryDiscoveryTokenSelector :: Selector '[] (Id NIDiscoveryToken)
accessoryDiscoveryTokenSelector = mkSelector "accessoryDiscoveryToken"

-- | @Selector@ for @cameraAssistanceEnabled@
cameraAssistanceEnabledSelector :: Selector '[] Bool
cameraAssistanceEnabledSelector = mkSelector "cameraAssistanceEnabled"

-- | @Selector@ for @setCameraAssistanceEnabled:@
setCameraAssistanceEnabledSelector :: Selector '[Bool] ()
setCameraAssistanceEnabledSelector = mkSelector "setCameraAssistanceEnabled:"

