{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccessory@.
module ObjC.AccessorySetupKit.ASAccessory
  ( ASAccessory
  , IsASAccessory(..)
  , state
  , bluetoothIdentifier
  , bluetoothTransportBridgingIdentifier
  , displayName
  , ssid
  , wifiAwarePairedDeviceID
  , descriptor
  , bluetoothIdentifierSelector
  , bluetoothTransportBridgingIdentifierSelector
  , descriptorSelector
  , displayNameSelector
  , ssidSelector
  , stateSelector
  , wifiAwarePairedDeviceIDSelector

  -- * Enum types
  , ASAccessoryState(ASAccessoryState)
  , pattern ASAccessoryStateUnauthorized
  , pattern ASAccessoryStateAwaitingAuthorization
  , pattern ASAccessoryStateAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The current authorization state of the accessory.
--
-- ObjC selector: @- state@
state :: IsASAccessory asAccessory => asAccessory -> IO ASAccessoryState
state asAccessory =
  sendMessage asAccessory stateSelector

-- | The accessory's unique Bluetooth identifier, if any.
--
-- Use this identifier to establish a connection to the accessory.
--
-- ObjC selector: @- bluetoothIdentifier@
bluetoothIdentifier :: IsASAccessory asAccessory => asAccessory -> IO (Id NSUUID)
bluetoothIdentifier asAccessory =
  sendMessage asAccessory bluetoothIdentifierSelector

-- | The accessory's Bluetooth identifier, if any, for use when bridging classic transport profiles.
--
-- ObjC selector: @- bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifier :: IsASAccessory asAccessory => asAccessory -> IO (Id NSData)
bluetoothTransportBridgingIdentifier asAccessory =
  sendMessage asAccessory bluetoothTransportBridgingIdentifierSelector

-- | The accessory's name, suitable for displaying to someone using your app.
--
-- ObjC selector: @- displayName@
displayName :: IsASAccessory asAccessory => asAccessory -> IO (Id NSString)
displayName asAccessory =
  sendMessage asAccessory displayNameSelector

-- | The accessory's Wi-Fi SSID, if any.
--
-- Use this identifier to establish a connection to the accessory.
--
-- ObjC selector: @- SSID@
ssid :: IsASAccessory asAccessory => asAccessory -> IO (Id NSString)
ssid asAccessory =
  sendMessage asAccessory ssidSelector

-- | The accessory's Wi-Fi Aware Pairing Identifier.
--
-- Use this identifier to establish a connection to the accessory using Wi-Fi Aware Framework.
--
-- ObjC selector: @- wifiAwarePairedDeviceID@
wifiAwarePairedDeviceID :: IsASAccessory asAccessory => asAccessory -> IO CULong
wifiAwarePairedDeviceID asAccessory =
  sendMessage asAccessory wifiAwarePairedDeviceIDSelector

-- | The descriptor used to discover the accessory.
--
-- ObjC selector: @- descriptor@
descriptor :: IsASAccessory asAccessory => asAccessory -> IO (Id ASDiscoveryDescriptor)
descriptor asAccessory =
  sendMessage asAccessory descriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @state@
stateSelector :: Selector '[] ASAccessoryState
stateSelector = mkSelector "state"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector '[] (Id NSUUID)
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

-- | @Selector@ for @bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifierSelector :: Selector '[] (Id NSData)
bluetoothTransportBridgingIdentifierSelector = mkSelector "bluetoothTransportBridgingIdentifier"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @SSID@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @wifiAwarePairedDeviceID@
wifiAwarePairedDeviceIDSelector :: Selector '[] CULong
wifiAwarePairedDeviceIDSelector = mkSelector "wifiAwarePairedDeviceID"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id ASDiscoveryDescriptor)
descriptorSelector = mkSelector "descriptor"

