{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A discovered accessory, for use in creating a customized picker display item.
--
-- When your app's picker uses the ``ASPickerDisplaySettings/Options/filterDiscoveryResults`` option, you receive ``ASAccessoryEventType/accessoryDiscovered`` events that contain this type. Use the discovered accessory's Bluetooth properties to create a new ``ASDiscoveredDisplayItem``, incorporating traits like a custom accessory name or a newly downloaded product image. You can then add this item to the picker to allow the person using the app to set up the accessory.
--
-- Generated bindings for @ASDiscoveredAccessory@.
module ObjC.AccessorySetupKit.ASDiscoveredAccessory
  ( ASDiscoveredAccessory
  , IsASDiscoveredAccessory(..)
  , bluetoothAdvertisementData
  , bluetoothRSSI
  , bluetoothAdvertisementDataSelector
  , bluetoothRSSISelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The Bluetooth advertisement data from the discovered accessory.
--
-- ObjC selector: @- bluetoothAdvertisementData@
bluetoothAdvertisementData :: IsASDiscoveredAccessory asDiscoveredAccessory => asDiscoveredAccessory -> IO (Id NSDictionary)
bluetoothAdvertisementData asDiscoveredAccessory =
  sendMessage asDiscoveredAccessory bluetoothAdvertisementDataSelector

-- | The Bluetooth RSSI (Received Signal Strength Indicator) value from the discovered accessory.
--
-- This value represents the signal strength in dBm when the session discovered the accessory.
--
-- ObjC selector: @- bluetoothRSSI@
bluetoothRSSI :: IsASDiscoveredAccessory asDiscoveredAccessory => asDiscoveredAccessory -> IO (Id NSNumber)
bluetoothRSSI asDiscoveredAccessory =
  sendMessage asDiscoveredAccessory bluetoothRSSISelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bluetoothAdvertisementData@
bluetoothAdvertisementDataSelector :: Selector '[] (Id NSDictionary)
bluetoothAdvertisementDataSelector = mkSelector "bluetoothAdvertisementData"

-- | @Selector@ for @bluetoothRSSI@
bluetoothRSSISelector :: Selector '[] (Id NSNumber)
bluetoothRSSISelector = mkSelector "bluetoothRSSI"

