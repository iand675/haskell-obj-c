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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The Bluetooth advertisement data from the discovered accessory.
--
-- ObjC selector: @- bluetoothAdvertisementData@
bluetoothAdvertisementData :: IsASDiscoveredAccessory asDiscoveredAccessory => asDiscoveredAccessory -> IO (Id NSDictionary)
bluetoothAdvertisementData asDiscoveredAccessory  =
    sendMsg asDiscoveredAccessory (mkSelector "bluetoothAdvertisementData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Bluetooth RSSI (Received Signal Strength Indicator) value from the discovered accessory.
--
-- This value represents the signal strength in dBm when the session discovered the accessory.
--
-- ObjC selector: @- bluetoothRSSI@
bluetoothRSSI :: IsASDiscoveredAccessory asDiscoveredAccessory => asDiscoveredAccessory -> IO (Id NSNumber)
bluetoothRSSI asDiscoveredAccessory  =
    sendMsg asDiscoveredAccessory (mkSelector "bluetoothRSSI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bluetoothAdvertisementData@
bluetoothAdvertisementDataSelector :: Selector
bluetoothAdvertisementDataSelector = mkSelector "bluetoothAdvertisementData"

-- | @Selector@ for @bluetoothRSSI@
bluetoothRSSISelector :: Selector
bluetoothRSSISelector = mkSelector "bluetoothRSSI"

