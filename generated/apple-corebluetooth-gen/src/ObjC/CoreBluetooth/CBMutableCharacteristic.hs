{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBMutableCharacteristic
--
-- Used to create a local characteristic, which can be added to the local database via CBPeripheralManager. Once a characteristic				is published, it is cached and can no longer be changed. 				If a characteristic value is specified, it will be cached and marked CBCharacteristicPropertyRead and 				CBAttributePermissionsReadable. If a characteristic value needs to be writeable, or may change during the lifetime of the				published CBService, it is considered a dynamic value and will be requested on-demand. Dynamic values are identified by a				value of nil.
--
-- Generated bindings for @CBMutableCharacteristic@.
module ObjC.CoreBluetooth.CBMutableCharacteristic
  ( CBMutableCharacteristic
  , IsCBMutableCharacteristic(..)
  , initWithType_properties_value_permissions
  , permissions
  , setPermissions
  , properties
  , setProperties
  , value
  , setValue
  , descriptors
  , setDescriptors
  , descriptorsSelector
  , initWithType_properties_value_permissionsSelector
  , permissionsSelector
  , propertiesSelector
  , setDescriptorsSelector
  , setPermissionsSelector
  , setPropertiesSelector
  , setValueSelector
  , valueSelector

  -- * Enum types
  , CBAttributePermissions(CBAttributePermissions)
  , pattern CBAttributePermissionsReadable
  , pattern CBAttributePermissionsWriteable
  , pattern CBAttributePermissionsReadEncryptionRequired
  , pattern CBAttributePermissionsWriteEncryptionRequired
  , CBCharacteristicProperties(CBCharacteristicProperties)
  , pattern CBCharacteristicPropertyBroadcast
  , pattern CBCharacteristicPropertyRead
  , pattern CBCharacteristicPropertyWriteWithoutResponse
  , pattern CBCharacteristicPropertyWrite
  , pattern CBCharacteristicPropertyNotify
  , pattern CBCharacteristicPropertyIndicate
  , pattern CBCharacteristicPropertyAuthenticatedSignedWrites
  , pattern CBCharacteristicPropertyExtendedProperties
  , pattern CBCharacteristicPropertyNotifyEncryptionRequired
  , pattern CBCharacteristicPropertyIndicateEncryptionRequired

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.CoreBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithType:properties:value:permissions
--
-- @UUID@ — The Bluetooth UUID of the characteristic.
--
-- @properties@ — The properties of the characteristic.
--
-- @value@ — The characteristic value to be cached. If nil, the value will be dynamic and requested on-demand.
--
-- @permissions@ — The permissions of the characteristic value.
--
-- Returns an initialized characteristic.
--
-- ObjC selector: @- initWithType:properties:value:permissions:@
initWithType_properties_value_permissions :: (IsCBMutableCharacteristic cbMutableCharacteristic, IsCBUUID uuid, IsNSData value) => cbMutableCharacteristic -> uuid -> CBCharacteristicProperties -> value -> CBAttributePermissions -> IO (Id CBMutableCharacteristic)
initWithType_properties_value_permissions cbMutableCharacteristic uuid properties value permissions =
  sendOwnedMessage cbMutableCharacteristic initWithType_properties_value_permissionsSelector (toCBUUID uuid) properties (toNSData value) permissions

-- | permissions
--
-- The permissions of the characteristic value.
--
-- See: CBAttributePermissions
--
-- ObjC selector: @- permissions@
permissions :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO CBAttributePermissions
permissions cbMutableCharacteristic =
  sendMessage cbMutableCharacteristic permissionsSelector

-- | permissions
--
-- The permissions of the characteristic value.
--
-- See: CBAttributePermissions
--
-- ObjC selector: @- setPermissions:@
setPermissions :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> CBAttributePermissions -> IO ()
setPermissions cbMutableCharacteristic value =
  sendMessage cbMutableCharacteristic setPermissionsSelector value

-- | @- properties@
properties :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO CBCharacteristicProperties
properties cbMutableCharacteristic =
  sendMessage cbMutableCharacteristic propertiesSelector

-- | @- setProperties:@
setProperties :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> CBCharacteristicProperties -> IO ()
setProperties cbMutableCharacteristic value =
  sendMessage cbMutableCharacteristic setPropertiesSelector value

-- | @- value@
value :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO (Id NSData)
value cbMutableCharacteristic =
  sendMessage cbMutableCharacteristic valueSelector

-- | @- setValue:@
setValue :: (IsCBMutableCharacteristic cbMutableCharacteristic, IsNSData value) => cbMutableCharacteristic -> value -> IO ()
setValue cbMutableCharacteristic value =
  sendMessage cbMutableCharacteristic setValueSelector (toNSData value)

-- | @- descriptors@
descriptors :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO (Id NSArray)
descriptors cbMutableCharacteristic =
  sendMessage cbMutableCharacteristic descriptorsSelector

-- | @- setDescriptors:@
setDescriptors :: (IsCBMutableCharacteristic cbMutableCharacteristic, IsNSArray value) => cbMutableCharacteristic -> value -> IO ()
setDescriptors cbMutableCharacteristic value =
  sendMessage cbMutableCharacteristic setDescriptorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:properties:value:permissions:@
initWithType_properties_value_permissionsSelector :: Selector '[Id CBUUID, CBCharacteristicProperties, Id NSData, CBAttributePermissions] (Id CBMutableCharacteristic)
initWithType_properties_value_permissionsSelector = mkSelector "initWithType:properties:value:permissions:"

-- | @Selector@ for @permissions@
permissionsSelector :: Selector '[] CBAttributePermissions
permissionsSelector = mkSelector "permissions"

-- | @Selector@ for @setPermissions:@
setPermissionsSelector :: Selector '[CBAttributePermissions] ()
setPermissionsSelector = mkSelector "setPermissions:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] CBCharacteristicProperties
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector '[CBCharacteristicProperties] ()
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSData)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSData] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector '[] (Id NSArray)
descriptorsSelector = mkSelector "descriptors"

-- | @Selector@ for @setDescriptors:@
setDescriptorsSelector :: Selector '[Id NSArray] ()
setDescriptorsSelector = mkSelector "setDescriptors:"

