{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBPeripheral
--
-- Represents a peripheral.
--
-- Generated bindings for @CBPeripheral@.
module ObjC.CoreBluetooth.CBPeripheral
  ( CBPeripheral
  , IsCBPeripheral(..)
  , readRSSI
  , discoverServices
  , discoverIncludedServices_forService
  , discoverCharacteristics_forService
  , readValueForCharacteristic
  , maximumWriteValueLengthForType
  , writeValue_forCharacteristic_type
  , setNotifyValue_forCharacteristic
  , discoverDescriptorsForCharacteristic
  , readValueForDescriptor
  , writeValue_forDescriptor
  , openL2CAPChannel
  , name
  , state
  , services
  , canSendWriteWithoutResponse
  , ancsAuthorized
  , readRSSISelector
  , discoverServicesSelector
  , discoverIncludedServices_forServiceSelector
  , discoverCharacteristics_forServiceSelector
  , readValueForCharacteristicSelector
  , maximumWriteValueLengthForTypeSelector
  , writeValue_forCharacteristic_typeSelector
  , setNotifyValue_forCharacteristicSelector
  , discoverDescriptorsForCharacteristicSelector
  , readValueForDescriptorSelector
  , writeValue_forDescriptorSelector
  , openL2CAPChannelSelector
  , nameSelector
  , stateSelector
  , servicesSelector
  , canSendWriteWithoutResponseSelector
  , ancsAuthorizedSelector

  -- * Enum types
  , CBCharacteristicWriteType(CBCharacteristicWriteType)
  , pattern CBCharacteristicWriteWithResponse
  , pattern CBCharacteristicWriteWithoutResponse
  , CBPeripheralState(CBPeripheralState)
  , pattern CBPeripheralStateDisconnected
  , pattern CBPeripheralStateConnecting
  , pattern CBPeripheralStateConnected
  , pattern CBPeripheralStateDisconnecting

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

-- | readRSSI
--
-- While connected, retrieves the current RSSI of the link.
--
-- See: peripheral:didReadRSSI:error:
--
-- ObjC selector: @- readRSSI@
readRSSI :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO ()
readRSSI cbPeripheral  =
  sendMsg cbPeripheral (mkSelector "readRSSI") retVoid []

-- | discoverServices:
--
-- @serviceUUIDs@ — A list of CBUUID objects representing the service types to be discovered. If nil,						all services will be discovered.
--
-- Discovers available service(s) on the peripheral.
--
-- See: peripheral:didDiscoverServices:
--
-- ObjC selector: @- discoverServices:@
discoverServices :: (IsCBPeripheral cbPeripheral, IsNSArray serviceUUIDs) => cbPeripheral -> serviceUUIDs -> IO ()
discoverServices cbPeripheral  serviceUUIDs =
withObjCPtr serviceUUIDs $ \raw_serviceUUIDs ->
    sendMsg cbPeripheral (mkSelector "discoverServices:") retVoid [argPtr (castPtr raw_serviceUUIDs :: Ptr ())]

-- | discoverIncludedServices:forService:
--
-- @includedServiceUUIDs@ — A list of CBUUID objects representing the included service types to be discovered. If nil,								all of services included services will be discovered, which is considerably slower and not recommended.
--
-- @service@ — A GATT service.
--
-- Discovers the specified included service(s) of service.
--
-- See: peripheral:didDiscoverIncludedServicesForService:error:
--
-- ObjC selector: @- discoverIncludedServices:forService:@
discoverIncludedServices_forService :: (IsCBPeripheral cbPeripheral, IsNSArray includedServiceUUIDs, IsCBService service) => cbPeripheral -> includedServiceUUIDs -> service -> IO ()
discoverIncludedServices_forService cbPeripheral  includedServiceUUIDs service =
withObjCPtr includedServiceUUIDs $ \raw_includedServiceUUIDs ->
  withObjCPtr service $ \raw_service ->
      sendMsg cbPeripheral (mkSelector "discoverIncludedServices:forService:") retVoid [argPtr (castPtr raw_includedServiceUUIDs :: Ptr ()), argPtr (castPtr raw_service :: Ptr ())]

-- | discoverCharacteristics:forService:
--
-- @characteristicUUIDs@ — A list of CBUUID objects representing the characteristic types to be discovered. If nil,								all characteristics of service will be discovered.
--
-- @service@ — A GATT service.
--
-- Discovers the specified characteristic(s) of service.
--
-- See: peripheral:didDiscoverCharacteristicsForService:error:
--
-- ObjC selector: @- discoverCharacteristics:forService:@
discoverCharacteristics_forService :: (IsCBPeripheral cbPeripheral, IsNSArray characteristicUUIDs, IsCBService service) => cbPeripheral -> characteristicUUIDs -> service -> IO ()
discoverCharacteristics_forService cbPeripheral  characteristicUUIDs service =
withObjCPtr characteristicUUIDs $ \raw_characteristicUUIDs ->
  withObjCPtr service $ \raw_service ->
      sendMsg cbPeripheral (mkSelector "discoverCharacteristics:forService:") retVoid [argPtr (castPtr raw_characteristicUUIDs :: Ptr ()), argPtr (castPtr raw_service :: Ptr ())]

-- | readValueForCharacteristic:
--
-- @characteristic@ — A GATT characteristic.
--
-- Reads the characteristic value for characteristic.
--
-- See: peripheral:didUpdateValueForCharacteristic:error:
--
-- ObjC selector: @- readValueForCharacteristic:@
readValueForCharacteristic :: (IsCBPeripheral cbPeripheral, IsCBCharacteristic characteristic) => cbPeripheral -> characteristic -> IO ()
readValueForCharacteristic cbPeripheral  characteristic =
withObjCPtr characteristic $ \raw_characteristic ->
    sendMsg cbPeripheral (mkSelector "readValueForCharacteristic:") retVoid [argPtr (castPtr raw_characteristic :: Ptr ())]

-- | maximumWriteValueLengthForType:
--
-- The maximum amount of data, in bytes, that can be sent to a characteristic in a single write type.
--
-- See: writeValue:forCharacteristic:type:
--
-- ObjC selector: @- maximumWriteValueLengthForType:@
maximumWriteValueLengthForType :: IsCBPeripheral cbPeripheral => cbPeripheral -> CBCharacteristicWriteType -> IO CULong
maximumWriteValueLengthForType cbPeripheral  type_ =
  sendMsg cbPeripheral (mkSelector "maximumWriteValueLengthForType:") retCULong [argCLong (coerce type_)]

-- | writeValue:forCharacteristic:type:
--
-- @data@ — The value to write.
--
-- @characteristic@ — The characteristic whose characteristic value will be written.
--
-- @type@ — The type of write to be executed.
--
-- Writes value to characteristic's characteristic value.							If the CBCharacteristicWriteWithResponse type is specified, {
--
-- peripheral:didWriteValueForCharacteristic:error:}
-- is called with the result of the write request.
-- If the <code>CBCharacteristicWriteWithoutResponse</code> type is specified, and canSendWriteWithoutResponse is false, the delivery
-- of the data is best-effort and may not be guaranteed.
--
-- @see					peripheral:didWriteValueForCharacteristic:error:
-- @see					peripheralIsReadyToSendWriteWithoutResponse:
-- @see					canSendWriteWithoutResponse
-- @see					CBCharacteristicWriteType
--
-- ObjC selector: @- writeValue:forCharacteristic:type:@
writeValue_forCharacteristic_type :: (IsCBPeripheral cbPeripheral, IsNSData data_, IsCBCharacteristic characteristic) => cbPeripheral -> data_ -> characteristic -> CBCharacteristicWriteType -> IO ()
writeValue_forCharacteristic_type cbPeripheral  data_ characteristic type_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr characteristic $ \raw_characteristic ->
      sendMsg cbPeripheral (mkSelector "writeValue:forCharacteristic:type:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_characteristic :: Ptr ()), argCLong (coerce type_)]

-- | setNotifyValue:forCharacteristic:
--
-- @enabled@ — Whether or not notifications/indications should be enabled.
--
-- @characteristic@ — The characteristic containing the client characteristic configuration descriptor.
--
-- Enables or disables notifications/indications for the characteristic value of characteristic. If characteristic							allows both, notifications will be used.                          When notifications/indications are enabled, updates to the characteristic value will be received via delegate method
--
-- peripheral:didUpdateValueForCharacteristic:error:
--
-- . Since it is the peripheral that chooses when to send an update,                          the application should be prepared to handle them as long as notifications/indications remain enabled.
--
-- See: peripheral:didUpdateNotificationStateForCharacteristic:error:
--
-- CBConnectPeripheralOptionNotifyOnNotificationKey
--
-- ObjC selector: @- setNotifyValue:forCharacteristic:@
setNotifyValue_forCharacteristic :: (IsCBPeripheral cbPeripheral, IsCBCharacteristic characteristic) => cbPeripheral -> Bool -> characteristic -> IO ()
setNotifyValue_forCharacteristic cbPeripheral  enabled characteristic =
withObjCPtr characteristic $ \raw_characteristic ->
    sendMsg cbPeripheral (mkSelector "setNotifyValue:forCharacteristic:") retVoid [argCULong (if enabled then 1 else 0), argPtr (castPtr raw_characteristic :: Ptr ())]

-- | discoverDescriptorsForCharacteristic:
--
-- @characteristic@ — A GATT characteristic.
--
-- Discovers the characteristic descriptor(s) of characteristic.
--
-- See: peripheral:didDiscoverDescriptorsForCharacteristic:error:
--
-- ObjC selector: @- discoverDescriptorsForCharacteristic:@
discoverDescriptorsForCharacteristic :: (IsCBPeripheral cbPeripheral, IsCBCharacteristic characteristic) => cbPeripheral -> characteristic -> IO ()
discoverDescriptorsForCharacteristic cbPeripheral  characteristic =
withObjCPtr characteristic $ \raw_characteristic ->
    sendMsg cbPeripheral (mkSelector "discoverDescriptorsForCharacteristic:") retVoid [argPtr (castPtr raw_characteristic :: Ptr ())]

-- | readValueForDescriptor:
--
-- @descriptor@ — A GATT characteristic descriptor.
--
-- Reads the value of descriptor.
--
-- See: peripheral:didUpdateValueForDescriptor:error:
--
-- ObjC selector: @- readValueForDescriptor:@
readValueForDescriptor :: (IsCBPeripheral cbPeripheral, IsCBDescriptor descriptor) => cbPeripheral -> descriptor -> IO ()
readValueForDescriptor cbPeripheral  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg cbPeripheral (mkSelector "readValueForDescriptor:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ())]

-- | writeValue:forDescriptor:
--
-- @data@ — The value to write.
--
-- @descriptor@ — A GATT characteristic descriptor.
--
-- Writes data to descriptor's value. Client characteristic configuration descriptors cannot be written using						this method, and should instead use
--
-- setNotifyValue:forCharacteristic:
--
-- .
--
-- See: peripheral:didWriteValueForCharacteristic:error:
--
-- ObjC selector: @- writeValue:forDescriptor:@
writeValue_forDescriptor :: (IsCBPeripheral cbPeripheral, IsNSData data_, IsCBDescriptor descriptor) => cbPeripheral -> data_ -> descriptor -> IO ()
writeValue_forDescriptor cbPeripheral  data_ descriptor =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg cbPeripheral (mkSelector "writeValue:forDescriptor:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())]

-- | openL2CAPChannel:
--
-- @PSM@ — The PSM of the channel to open
--
-- Attempt to open an L2CAP channel to the peripheral using the supplied PSM.
--
-- See: peripheral:didWriteValueForCharacteristic:error:
--
-- ObjC selector: @- openL2CAPChannel:@
openL2CAPChannel :: IsCBPeripheral cbPeripheral => cbPeripheral -> CUShort -> IO ()
openL2CAPChannel cbPeripheral  psm =
  sendMsg cbPeripheral (mkSelector "openL2CAPChannel:") retVoid [argCUInt (fromIntegral psm)]

-- | name
--
-- The name of the peripheral.
--
-- ObjC selector: @- name@
name :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO (Id NSString)
name cbPeripheral  =
  sendMsg cbPeripheral (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | state
--
-- The current connection state of the peripheral.
--
-- ObjC selector: @- state@
state :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO CBPeripheralState
state cbPeripheral  =
  fmap (coerce :: CLong -> CBPeripheralState) $ sendMsg cbPeripheral (mkSelector "state") retCLong []

-- | services
--
-- A list of CBService objects that have been discovered on the peripheral.
--
-- ObjC selector: @- services@
services :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO (Id NSArray)
services cbPeripheral  =
  sendMsg cbPeripheral (mkSelector "services") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | canSendWriteWithoutResponse
--
-- YES if the remote device has space to send a write without response. If this value is NO,				the value will be set to YES after the current writes have been flushed, and				<link>peripheralIsReadyToSendWriteWithoutResponse:</link> will be called.
--
-- ObjC selector: @- canSendWriteWithoutResponse@
canSendWriteWithoutResponse :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO Bool
canSendWriteWithoutResponse cbPeripheral  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbPeripheral (mkSelector "canSendWriteWithoutResponse") retCULong []

-- | ancsAuthorized
--
-- YES if the remote device has been authorized to receive data over ANCS (Apple Notification Service Center) protocol.  If this value is NO,                the value will be set to YES after a user authorization occurs and                <link>didUpdateANCSAuthorizationForPeripheral:</link> will be called.
--
-- ObjC selector: @- ancsAuthorized@
ancsAuthorized :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO Bool
ancsAuthorized cbPeripheral  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbPeripheral (mkSelector "ancsAuthorized") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readRSSI@
readRSSISelector :: Selector
readRSSISelector = mkSelector "readRSSI"

-- | @Selector@ for @discoverServices:@
discoverServicesSelector :: Selector
discoverServicesSelector = mkSelector "discoverServices:"

-- | @Selector@ for @discoverIncludedServices:forService:@
discoverIncludedServices_forServiceSelector :: Selector
discoverIncludedServices_forServiceSelector = mkSelector "discoverIncludedServices:forService:"

-- | @Selector@ for @discoverCharacteristics:forService:@
discoverCharacteristics_forServiceSelector :: Selector
discoverCharacteristics_forServiceSelector = mkSelector "discoverCharacteristics:forService:"

-- | @Selector@ for @readValueForCharacteristic:@
readValueForCharacteristicSelector :: Selector
readValueForCharacteristicSelector = mkSelector "readValueForCharacteristic:"

-- | @Selector@ for @maximumWriteValueLengthForType:@
maximumWriteValueLengthForTypeSelector :: Selector
maximumWriteValueLengthForTypeSelector = mkSelector "maximumWriteValueLengthForType:"

-- | @Selector@ for @writeValue:forCharacteristic:type:@
writeValue_forCharacteristic_typeSelector :: Selector
writeValue_forCharacteristic_typeSelector = mkSelector "writeValue:forCharacteristic:type:"

-- | @Selector@ for @setNotifyValue:forCharacteristic:@
setNotifyValue_forCharacteristicSelector :: Selector
setNotifyValue_forCharacteristicSelector = mkSelector "setNotifyValue:forCharacteristic:"

-- | @Selector@ for @discoverDescriptorsForCharacteristic:@
discoverDescriptorsForCharacteristicSelector :: Selector
discoverDescriptorsForCharacteristicSelector = mkSelector "discoverDescriptorsForCharacteristic:"

-- | @Selector@ for @readValueForDescriptor:@
readValueForDescriptorSelector :: Selector
readValueForDescriptorSelector = mkSelector "readValueForDescriptor:"

-- | @Selector@ for @writeValue:forDescriptor:@
writeValue_forDescriptorSelector :: Selector
writeValue_forDescriptorSelector = mkSelector "writeValue:forDescriptor:"

-- | @Selector@ for @openL2CAPChannel:@
openL2CAPChannelSelector :: Selector
openL2CAPChannelSelector = mkSelector "openL2CAPChannel:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @services@
servicesSelector :: Selector
servicesSelector = mkSelector "services"

-- | @Selector@ for @canSendWriteWithoutResponse@
canSendWriteWithoutResponseSelector :: Selector
canSendWriteWithoutResponseSelector = mkSelector "canSendWriteWithoutResponse"

-- | @Selector@ for @ancsAuthorized@
ancsAuthorizedSelector :: Selector
ancsAuthorizedSelector = mkSelector "ancsAuthorized"

