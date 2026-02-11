{-# LANGUAGE PatternSynonyms #-}
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
  , initWithType_properties_value_permissionsSelector
  , permissionsSelector
  , setPermissionsSelector
  , propertiesSelector
  , setPropertiesSelector
  , valueSelector
  , setValueSelector
  , descriptorsSelector
  , setDescriptorsSelector

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
initWithType_properties_value_permissions cbMutableCharacteristic  uuid properties value permissions =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr value $ \raw_value ->
      sendMsg cbMutableCharacteristic (mkSelector "initWithType:properties:value:permissions:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCULong (coerce properties), argPtr (castPtr raw_value :: Ptr ()), argCULong (coerce permissions)] >>= ownedObject . castPtr

-- | permissions
--
-- The permissions of the characteristic value.
--
-- See: CBAttributePermissions
--
-- ObjC selector: @- permissions@
permissions :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO CBAttributePermissions
permissions cbMutableCharacteristic  =
  fmap (coerce :: CULong -> CBAttributePermissions) $ sendMsg cbMutableCharacteristic (mkSelector "permissions") retCULong []

-- | permissions
--
-- The permissions of the characteristic value.
--
-- See: CBAttributePermissions
--
-- ObjC selector: @- setPermissions:@
setPermissions :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> CBAttributePermissions -> IO ()
setPermissions cbMutableCharacteristic  value =
  sendMsg cbMutableCharacteristic (mkSelector "setPermissions:") retVoid [argCULong (coerce value)]

-- | @- properties@
properties :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO CBCharacteristicProperties
properties cbMutableCharacteristic  =
  fmap (coerce :: CULong -> CBCharacteristicProperties) $ sendMsg cbMutableCharacteristic (mkSelector "properties") retCULong []

-- | @- setProperties:@
setProperties :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> CBCharacteristicProperties -> IO ()
setProperties cbMutableCharacteristic  value =
  sendMsg cbMutableCharacteristic (mkSelector "setProperties:") retVoid [argCULong (coerce value)]

-- | @- value@
value :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO (Id NSData)
value cbMutableCharacteristic  =
  sendMsg cbMutableCharacteristic (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsCBMutableCharacteristic cbMutableCharacteristic, IsNSData value) => cbMutableCharacteristic -> value -> IO ()
setValue cbMutableCharacteristic  value =
withObjCPtr value $ \raw_value ->
    sendMsg cbMutableCharacteristic (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptors@
descriptors :: IsCBMutableCharacteristic cbMutableCharacteristic => cbMutableCharacteristic -> IO (Id NSArray)
descriptors cbMutableCharacteristic  =
  sendMsg cbMutableCharacteristic (mkSelector "descriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptors:@
setDescriptors :: (IsCBMutableCharacteristic cbMutableCharacteristic, IsNSArray value) => cbMutableCharacteristic -> value -> IO ()
setDescriptors cbMutableCharacteristic  value =
withObjCPtr value $ \raw_value ->
    sendMsg cbMutableCharacteristic (mkSelector "setDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:properties:value:permissions:@
initWithType_properties_value_permissionsSelector :: Selector
initWithType_properties_value_permissionsSelector = mkSelector "initWithType:properties:value:permissions:"

-- | @Selector@ for @permissions@
permissionsSelector :: Selector
permissionsSelector = mkSelector "permissions"

-- | @Selector@ for @setPermissions:@
setPermissionsSelector :: Selector
setPermissionsSelector = mkSelector "setPermissions:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector
descriptorsSelector = mkSelector "descriptors"

-- | @Selector@ for @setDescriptors:@
setDescriptorsSelector :: Selector
setDescriptorsSelector = mkSelector "setDescriptors:"

