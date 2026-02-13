{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccessorySettings@.
module ObjC.AccessorySetupKit.ASAccessorySettings
  ( ASAccessorySettings
  , IsASAccessorySettings(..)
  , defaultSettings
  , ssid
  , setSSID
  , bluetoothTransportBridgingIdentifier
  , setBluetoothTransportBridgingIdentifier
  , bluetoothTransportBridgingIdentifierSelector
  , defaultSettingsSelector
  , setBluetoothTransportBridgingIdentifierSelector
  , setSSIDSelector
  , ssidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An empty settings object.
--
-- ObjC selector: @+ defaultSettings@
defaultSettings :: IO (Id ASAccessorySettings)
defaultSettings  =
  do
    cls' <- getRequiredClass "ASAccessorySettings"
    sendClassMessage cls' defaultSettingsSelector

-- | A hotspot identifier that clients can use to connect to an accessory's hotspot.
--
-- ObjC selector: @- SSID@
ssid :: IsASAccessorySettings asAccessorySettings => asAccessorySettings -> IO (Id NSString)
ssid asAccessorySettings =
  sendMessage asAccessorySettings ssidSelector

-- | A hotspot identifier that clients can use to connect to an accessory's hotspot.
--
-- ObjC selector: @- setSSID:@
setSSID :: (IsASAccessorySettings asAccessorySettings, IsNSString value) => asAccessorySettings -> value -> IO ()
setSSID asAccessorySettings value =
  sendMessage asAccessorySettings setSSIDSelector (toNSString value)

-- | A 6-byte identifier for bridging classic transport profiles.
--
-- AccessorySetupKit ignores this property if another app already authorized and bridged the accessory.
--
-- ObjC selector: @- bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifier :: IsASAccessorySettings asAccessorySettings => asAccessorySettings -> IO (Id NSData)
bluetoothTransportBridgingIdentifier asAccessorySettings =
  sendMessage asAccessorySettings bluetoothTransportBridgingIdentifierSelector

-- | A 6-byte identifier for bridging classic transport profiles.
--
-- AccessorySetupKit ignores this property if another app already authorized and bridged the accessory.
--
-- ObjC selector: @- setBluetoothTransportBridgingIdentifier:@
setBluetoothTransportBridgingIdentifier :: (IsASAccessorySettings asAccessorySettings, IsNSData value) => asAccessorySettings -> value -> IO ()
setBluetoothTransportBridgingIdentifier asAccessorySettings value =
  sendMessage asAccessorySettings setBluetoothTransportBridgingIdentifierSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSettings@
defaultSettingsSelector :: Selector '[] (Id ASAccessorySettings)
defaultSettingsSelector = mkSelector "defaultSettings"

-- | @Selector@ for @SSID@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @setSSID:@
setSSIDSelector :: Selector '[Id NSString] ()
setSSIDSelector = mkSelector "setSSID:"

-- | @Selector@ for @bluetoothTransportBridgingIdentifier@
bluetoothTransportBridgingIdentifierSelector :: Selector '[] (Id NSData)
bluetoothTransportBridgingIdentifierSelector = mkSelector "bluetoothTransportBridgingIdentifier"

-- | @Selector@ for @setBluetoothTransportBridgingIdentifier:@
setBluetoothTransportBridgingIdentifierSelector :: Selector '[Id NSData] ()
setBluetoothTransportBridgingIdentifierSelector = mkSelector "setBluetoothTransportBridgingIdentifier:"

