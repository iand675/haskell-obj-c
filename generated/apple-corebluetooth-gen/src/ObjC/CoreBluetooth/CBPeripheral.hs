{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , name
  , rssi
  , state
  , services
  , canSendWriteWithoutResponse
  , ancsAuthorized
  , ancsAuthorizedSelector
  , canSendWriteWithoutResponseSelector
  , delegateSelector
  , discoverCharacteristics_forServiceSelector
  , discoverDescriptorsForCharacteristicSelector
  , discoverIncludedServices_forServiceSelector
  , discoverServicesSelector
  , maximumWriteValueLengthForTypeSelector
  , nameSelector
  , openL2CAPChannelSelector
  , readRSSISelector
  , readValueForCharacteristicSelector
  , readValueForDescriptorSelector
  , rssiSelector
  , servicesSelector
  , setDelegateSelector
  , setNotifyValue_forCharacteristicSelector
  , stateSelector
  , writeValue_forCharacteristic_typeSelector
  , writeValue_forDescriptorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
readRSSI cbPeripheral =
  sendMessage cbPeripheral readRSSISelector

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
discoverServices cbPeripheral serviceUUIDs =
  sendMessage cbPeripheral discoverServicesSelector (toNSArray serviceUUIDs)

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
discoverIncludedServices_forService cbPeripheral includedServiceUUIDs service =
  sendMessage cbPeripheral discoverIncludedServices_forServiceSelector (toNSArray includedServiceUUIDs) (toCBService service)

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
discoverCharacteristics_forService cbPeripheral characteristicUUIDs service =
  sendMessage cbPeripheral discoverCharacteristics_forServiceSelector (toNSArray characteristicUUIDs) (toCBService service)

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
readValueForCharacteristic cbPeripheral characteristic =
  sendMessage cbPeripheral readValueForCharacteristicSelector (toCBCharacteristic characteristic)

-- | maximumWriteValueLengthForType:
--
-- The maximum amount of data, in bytes, that can be sent to a characteristic in a single write type.
--
-- See: writeValue:forCharacteristic:type:
--
-- ObjC selector: @- maximumWriteValueLengthForType:@
maximumWriteValueLengthForType :: IsCBPeripheral cbPeripheral => cbPeripheral -> CBCharacteristicWriteType -> IO CULong
maximumWriteValueLengthForType cbPeripheral type_ =
  sendMessage cbPeripheral maximumWriteValueLengthForTypeSelector type_

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
writeValue_forCharacteristic_type cbPeripheral data_ characteristic type_ =
  sendMessage cbPeripheral writeValue_forCharacteristic_typeSelector (toNSData data_) (toCBCharacteristic characteristic) type_

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
setNotifyValue_forCharacteristic cbPeripheral enabled characteristic =
  sendMessage cbPeripheral setNotifyValue_forCharacteristicSelector enabled (toCBCharacteristic characteristic)

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
discoverDescriptorsForCharacteristic cbPeripheral characteristic =
  sendMessage cbPeripheral discoverDescriptorsForCharacteristicSelector (toCBCharacteristic characteristic)

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
readValueForDescriptor cbPeripheral descriptor =
  sendMessage cbPeripheral readValueForDescriptorSelector (toCBDescriptor descriptor)

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
writeValue_forDescriptor cbPeripheral data_ descriptor =
  sendMessage cbPeripheral writeValue_forDescriptorSelector (toNSData data_) (toCBDescriptor descriptor)

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
openL2CAPChannel cbPeripheral psm =
  sendMessage cbPeripheral openL2CAPChannelSelector psm

-- | delegate
--
-- The delegate object that will receive peripheral events.
--
-- ObjC selector: @- delegate@
delegate :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO RawId
delegate cbPeripheral =
  sendMessage cbPeripheral delegateSelector

-- | delegate
--
-- The delegate object that will receive peripheral events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCBPeripheral cbPeripheral => cbPeripheral -> RawId -> IO ()
setDelegate cbPeripheral value =
  sendMessage cbPeripheral setDelegateSelector value

-- | name
--
-- The name of the peripheral.
--
-- ObjC selector: @- name@
name :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO (Id NSString)
name cbPeripheral =
  sendMessage cbPeripheral nameSelector

-- | RSSI
--
-- The most recently read RSSI, in decibels.
--
-- Use {
--
-- peripheral:didReadRSSI:error:} instead.
--
-- ObjC selector: @- RSSI@
rssi :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO RawId
rssi cbPeripheral =
  sendMessage cbPeripheral rssiSelector

-- | state
--
-- The current connection state of the peripheral.
--
-- ObjC selector: @- state@
state :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO CBPeripheralState
state cbPeripheral =
  sendMessage cbPeripheral stateSelector

-- | services
--
-- A list of CBService objects that have been discovered on the peripheral.
--
-- ObjC selector: @- services@
services :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO (Id NSArray)
services cbPeripheral =
  sendMessage cbPeripheral servicesSelector

-- | canSendWriteWithoutResponse
--
-- YES if the remote device has space to send a write without response. If this value is NO,				the value will be set to YES after the current writes have been flushed, and				<link>peripheralIsReadyToSendWriteWithoutResponse:</link> will be called.
--
-- ObjC selector: @- canSendWriteWithoutResponse@
canSendWriteWithoutResponse :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO Bool
canSendWriteWithoutResponse cbPeripheral =
  sendMessage cbPeripheral canSendWriteWithoutResponseSelector

-- | ancsAuthorized
--
-- YES if the remote device has been authorized to receive data over ANCS (Apple Notification Service Center) protocol.  If this value is NO,                the value will be set to YES after a user authorization occurs and                <link>didUpdateANCSAuthorizationForPeripheral:</link> will be called.
--
-- ObjC selector: @- ancsAuthorized@
ancsAuthorized :: IsCBPeripheral cbPeripheral => cbPeripheral -> IO Bool
ancsAuthorized cbPeripheral =
  sendMessage cbPeripheral ancsAuthorizedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readRSSI@
readRSSISelector :: Selector '[] ()
readRSSISelector = mkSelector "readRSSI"

-- | @Selector@ for @discoverServices:@
discoverServicesSelector :: Selector '[Id NSArray] ()
discoverServicesSelector = mkSelector "discoverServices:"

-- | @Selector@ for @discoverIncludedServices:forService:@
discoverIncludedServices_forServiceSelector :: Selector '[Id NSArray, Id CBService] ()
discoverIncludedServices_forServiceSelector = mkSelector "discoverIncludedServices:forService:"

-- | @Selector@ for @discoverCharacteristics:forService:@
discoverCharacteristics_forServiceSelector :: Selector '[Id NSArray, Id CBService] ()
discoverCharacteristics_forServiceSelector = mkSelector "discoverCharacteristics:forService:"

-- | @Selector@ for @readValueForCharacteristic:@
readValueForCharacteristicSelector :: Selector '[Id CBCharacteristic] ()
readValueForCharacteristicSelector = mkSelector "readValueForCharacteristic:"

-- | @Selector@ for @maximumWriteValueLengthForType:@
maximumWriteValueLengthForTypeSelector :: Selector '[CBCharacteristicWriteType] CULong
maximumWriteValueLengthForTypeSelector = mkSelector "maximumWriteValueLengthForType:"

-- | @Selector@ for @writeValue:forCharacteristic:type:@
writeValue_forCharacteristic_typeSelector :: Selector '[Id NSData, Id CBCharacteristic, CBCharacteristicWriteType] ()
writeValue_forCharacteristic_typeSelector = mkSelector "writeValue:forCharacteristic:type:"

-- | @Selector@ for @setNotifyValue:forCharacteristic:@
setNotifyValue_forCharacteristicSelector :: Selector '[Bool, Id CBCharacteristic] ()
setNotifyValue_forCharacteristicSelector = mkSelector "setNotifyValue:forCharacteristic:"

-- | @Selector@ for @discoverDescriptorsForCharacteristic:@
discoverDescriptorsForCharacteristicSelector :: Selector '[Id CBCharacteristic] ()
discoverDescriptorsForCharacteristicSelector = mkSelector "discoverDescriptorsForCharacteristic:"

-- | @Selector@ for @readValueForDescriptor:@
readValueForDescriptorSelector :: Selector '[Id CBDescriptor] ()
readValueForDescriptorSelector = mkSelector "readValueForDescriptor:"

-- | @Selector@ for @writeValue:forDescriptor:@
writeValue_forDescriptorSelector :: Selector '[Id NSData, Id CBDescriptor] ()
writeValue_forDescriptorSelector = mkSelector "writeValue:forDescriptor:"

-- | @Selector@ for @openL2CAPChannel:@
openL2CAPChannelSelector :: Selector '[CUShort] ()
openL2CAPChannelSelector = mkSelector "openL2CAPChannel:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @RSSI@
rssiSelector :: Selector '[] RawId
rssiSelector = mkSelector "RSSI"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CBPeripheralState
stateSelector = mkSelector "state"

-- | @Selector@ for @services@
servicesSelector :: Selector '[] (Id NSArray)
servicesSelector = mkSelector "services"

-- | @Selector@ for @canSendWriteWithoutResponse@
canSendWriteWithoutResponseSelector :: Selector '[] Bool
canSendWriteWithoutResponseSelector = mkSelector "canSendWriteWithoutResponse"

-- | @Selector@ for @ancsAuthorized@
ancsAuthorizedSelector :: Selector '[] Bool
ancsAuthorizedSelector = mkSelector "ancsAuthorized"

