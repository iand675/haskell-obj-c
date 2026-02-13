{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASMigrationDisplayItem@.
module ObjC.AccessorySetupKit.ASMigrationDisplayItem
  ( ASMigrationDisplayItem
  , IsASMigrationDisplayItem(..)
  , peripheralIdentifier
  , setPeripheralIdentifier
  , hotspotSSID
  , setHotspotSSID
  , wifiAwarePairedDeviceID
  , setWifiAwarePairedDeviceID
  , hotspotSSIDSelector
  , peripheralIdentifierSelector
  , setHotspotSSIDSelector
  , setPeripheralIdentifierSelector
  , setWifiAwarePairedDeviceIDSelector
  , wifiAwarePairedDeviceIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The Bluetooth identifier of the accessory to migrate.
--
-- ObjC selector: @- peripheralIdentifier@
peripheralIdentifier :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> IO (Id NSUUID)
peripheralIdentifier asMigrationDisplayItem =
  sendMessage asMigrationDisplayItem peripheralIdentifierSelector

-- | The Bluetooth identifier of the accessory to migrate.
--
-- ObjC selector: @- setPeripheralIdentifier:@
setPeripheralIdentifier :: (IsASMigrationDisplayItem asMigrationDisplayItem, IsNSUUID value) => asMigrationDisplayItem -> value -> IO ()
setPeripheralIdentifier asMigrationDisplayItem value =
  sendMessage asMigrationDisplayItem setPeripheralIdentifierSelector (toNSUUID value)

-- | The Wi-Fi hotspot SSID of the accessory to migrate.
--
-- ObjC selector: @- hotspotSSID@
hotspotSSID :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> IO (Id NSString)
hotspotSSID asMigrationDisplayItem =
  sendMessage asMigrationDisplayItem hotspotSSIDSelector

-- | The Wi-Fi hotspot SSID of the accessory to migrate.
--
-- ObjC selector: @- setHotspotSSID:@
setHotspotSSID :: (IsASMigrationDisplayItem asMigrationDisplayItem, IsNSString value) => asMigrationDisplayItem -> value -> IO ()
setHotspotSSID asMigrationDisplayItem value =
  sendMessage asMigrationDisplayItem setHotspotSSIDSelector (toNSString value)

-- | The Wi-Fi Aware paired device identififer of the accessory to migrate.
--
-- ObjC selector: @- wifiAwarePairedDeviceID@
wifiAwarePairedDeviceID :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> IO CULong
wifiAwarePairedDeviceID asMigrationDisplayItem =
  sendMessage asMigrationDisplayItem wifiAwarePairedDeviceIDSelector

-- | The Wi-Fi Aware paired device identififer of the accessory to migrate.
--
-- ObjC selector: @- setWifiAwarePairedDeviceID:@
setWifiAwarePairedDeviceID :: IsASMigrationDisplayItem asMigrationDisplayItem => asMigrationDisplayItem -> CULong -> IO ()
setWifiAwarePairedDeviceID asMigrationDisplayItem value =
  sendMessage asMigrationDisplayItem setWifiAwarePairedDeviceIDSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peripheralIdentifier@
peripheralIdentifierSelector :: Selector '[] (Id NSUUID)
peripheralIdentifierSelector = mkSelector "peripheralIdentifier"

-- | @Selector@ for @setPeripheralIdentifier:@
setPeripheralIdentifierSelector :: Selector '[Id NSUUID] ()
setPeripheralIdentifierSelector = mkSelector "setPeripheralIdentifier:"

-- | @Selector@ for @hotspotSSID@
hotspotSSIDSelector :: Selector '[] (Id NSString)
hotspotSSIDSelector = mkSelector "hotspotSSID"

-- | @Selector@ for @setHotspotSSID:@
setHotspotSSIDSelector :: Selector '[Id NSString] ()
setHotspotSSIDSelector = mkSelector "setHotspotSSID:"

-- | @Selector@ for @wifiAwarePairedDeviceID@
wifiAwarePairedDeviceIDSelector :: Selector '[] CULong
wifiAwarePairedDeviceIDSelector = mkSelector "wifiAwarePairedDeviceID"

-- | @Selector@ for @setWifiAwarePairedDeviceID:@
setWifiAwarePairedDeviceIDSelector :: Selector '[CULong] ()
setWifiAwarePairedDeviceIDSelector = mkSelector "setWifiAwarePairedDeviceID:"

