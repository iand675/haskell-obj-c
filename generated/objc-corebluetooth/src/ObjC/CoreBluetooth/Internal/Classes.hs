{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreBluetooth.Internal.Classes (
    module ObjC.CoreBluetooth.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CBATTRequest ----------

-- | CBATTRequest
--
-- Represents a read or write request from a central.
-- 
-- Phantom type for @CBATTRequest@.
data CBATTRequest

instance IsObjCObject (Id CBATTRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBATTRequest"

class IsNSObject a => IsCBATTRequest a where
  toCBATTRequest :: a -> Id CBATTRequest

instance IsCBATTRequest (Id CBATTRequest) where
  toCBATTRequest = unsafeCastId

instance IsNSObject (Id CBATTRequest) where
  toNSObject = unsafeCastId

-- ---------- CBAttribute ----------

-- | Phantom type for @CBAttribute@.
data CBAttribute

instance IsObjCObject (Id CBAttribute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBAttribute"

class IsNSObject a => IsCBAttribute a where
  toCBAttribute :: a -> Id CBAttribute

instance IsCBAttribute (Id CBAttribute) where
  toCBAttribute = unsafeCastId

instance IsNSObject (Id CBAttribute) where
  toNSObject = unsafeCastId

-- ---------- CBL2CAPChannel ----------

-- | CBL2CAPChannel
--
-- A CBL2CAPChannel represents a live L2CAP connection to a remote device
-- 
-- Phantom type for @CBL2CAPChannel@.
data CBL2CAPChannel

instance IsObjCObject (Id CBL2CAPChannel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBL2CAPChannel"

class IsNSObject a => IsCBL2CAPChannel a where
  toCBL2CAPChannel :: a -> Id CBL2CAPChannel

instance IsCBL2CAPChannel (Id CBL2CAPChannel) where
  toCBL2CAPChannel = unsafeCastId

instance IsNSObject (Id CBL2CAPChannel) where
  toNSObject = unsafeCastId

-- ---------- CBManager ----------

-- | Phantom type for @CBManager@.
data CBManager

instance IsObjCObject (Id CBManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBManager"

class IsNSObject a => IsCBManager a where
  toCBManager :: a -> Id CBManager

instance IsCBManager (Id CBManager) where
  toCBManager = unsafeCastId

instance IsNSObject (Id CBManager) where
  toNSObject = unsafeCastId

-- ---------- CBPeer ----------

-- | Phantom type for @CBPeer@.
data CBPeer

instance IsObjCObject (Id CBPeer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBPeer"

class IsNSObject a => IsCBPeer a where
  toCBPeer :: a -> Id CBPeer

instance IsCBPeer (Id CBPeer) where
  toCBPeer = unsafeCastId

instance IsNSObject (Id CBPeer) where
  toNSObject = unsafeCastId

-- ---------- CBUUID ----------

-- | CBUUID
--
-- A 16-bit, 32-bit, or 128-bit Bluetooth UUID.      16-bit and 32-bit UUIDs are implicitly pre-filled with the Bluetooth Base UUID.
-- 
-- Phantom type for @CBUUID@.
data CBUUID

instance IsObjCObject (Id CBUUID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBUUID"

class IsNSObject a => IsCBUUID a where
  toCBUUID :: a -> Id CBUUID

instance IsCBUUID (Id CBUUID) where
  toCBUUID = unsafeCastId

instance IsNSObject (Id CBUUID) where
  toNSObject = unsafeCastId

-- ---------- CBCharacteristic ----------

-- | CBCharacteristic
--
-- Represents a service's characteristic.
-- 
-- Phantom type for @CBCharacteristic@.
data CBCharacteristic

instance IsObjCObject (Id CBCharacteristic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBCharacteristic"

class IsCBAttribute a => IsCBCharacteristic a where
  toCBCharacteristic :: a -> Id CBCharacteristic

instance IsCBCharacteristic (Id CBCharacteristic) where
  toCBCharacteristic = unsafeCastId

instance IsCBAttribute (Id CBCharacteristic) where
  toCBAttribute = unsafeCastId

instance IsNSObject (Id CBCharacteristic) where
  toNSObject = unsafeCastId

-- ---------- CBDescriptor ----------

-- | CBDescriptor
--
-- Represents a characteristic's descriptor.
-- 
-- Phantom type for @CBDescriptor@.
data CBDescriptor

instance IsObjCObject (Id CBDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBDescriptor"

class IsCBAttribute a => IsCBDescriptor a where
  toCBDescriptor :: a -> Id CBDescriptor

instance IsCBDescriptor (Id CBDescriptor) where
  toCBDescriptor = unsafeCastId

instance IsCBAttribute (Id CBDescriptor) where
  toCBAttribute = unsafeCastId

instance IsNSObject (Id CBDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CBService ----------

-- | CBService
--
-- Represents a peripheral's service or a service's included service.
-- 
-- Phantom type for @CBService@.
data CBService

instance IsObjCObject (Id CBService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBService"

class IsCBAttribute a => IsCBService a where
  toCBService :: a -> Id CBService

instance IsCBService (Id CBService) where
  toCBService = unsafeCastId

instance IsCBAttribute (Id CBService) where
  toCBAttribute = unsafeCastId

instance IsNSObject (Id CBService) where
  toNSObject = unsafeCastId

-- ---------- CBCentralManager ----------

-- | CBCentralManager
--
-- Entry point to the central role. Commands should only be issued when its state is CBCentralManagerStatePoweredOn.
-- 
-- Phantom type for @CBCentralManager@.
data CBCentralManager

instance IsObjCObject (Id CBCentralManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBCentralManager"

class IsCBManager a => IsCBCentralManager a where
  toCBCentralManager :: a -> Id CBCentralManager

instance IsCBCentralManager (Id CBCentralManager) where
  toCBCentralManager = unsafeCastId

instance IsCBManager (Id CBCentralManager) where
  toCBManager = unsafeCastId

instance IsNSObject (Id CBCentralManager) where
  toNSObject = unsafeCastId

-- ---------- CBPeripheralManager ----------

-- | CBPeripheralManager
--
-- The CBPeripheralManager class is an abstraction of the Peripheral and Broadcaster GAP roles, and the GATT Server              role. Its primary function is to allow you to manage published services within the GATT database, and to advertise these services              to other devices.              Each application has sandboxed access to the shared GATT database. You can add services to the database by calling {
--
-- addService:};
-- they can be removed via {@link removeService:} and {@link removeAllServices}, as appropriate. While a service is in the database,
-- it is visible to and can be accessed by any connected GATT Client. However, applications that have not specified the "bluetooth-peripheral"
-- background mode will have the contents of their service(s) "disabled" when in the background. Any remote device trying to access
-- characteristic values or descriptors during this time will receive an error response.
-- Once you've published services that you want to share, you can ask to advertise their availability and allow other devices to connect
-- to you by calling {@link startAdvertising:}. Like the GATT database, advertisement is managed at the system level and shared by all
-- applications. This means that even if you aren't advertising at the moment, someone else might be!
-- 
-- Phantom type for @CBPeripheralManager@.
data CBPeripheralManager

instance IsObjCObject (Id CBPeripheralManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBPeripheralManager"

class IsCBManager a => IsCBPeripheralManager a where
  toCBPeripheralManager :: a -> Id CBPeripheralManager

instance IsCBPeripheralManager (Id CBPeripheralManager) where
  toCBPeripheralManager = unsafeCastId

instance IsCBManager (Id CBPeripheralManager) where
  toCBManager = unsafeCastId

instance IsNSObject (Id CBPeripheralManager) where
  toNSObject = unsafeCastId

-- ---------- CBCentral ----------

-- | CBCentral
--
-- Represents a remote central.
-- 
-- Phantom type for @CBCentral@.
data CBCentral

instance IsObjCObject (Id CBCentral) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBCentral"

class IsCBPeer a => IsCBCentral a where
  toCBCentral :: a -> Id CBCentral

instance IsCBCentral (Id CBCentral) where
  toCBCentral = unsafeCastId

instance IsCBPeer (Id CBCentral) where
  toCBPeer = unsafeCastId

instance IsNSObject (Id CBCentral) where
  toNSObject = unsafeCastId

-- ---------- CBPeripheral ----------

-- | CBPeripheral
--
-- Represents a peripheral.
-- 
-- Phantom type for @CBPeripheral@.
data CBPeripheral

instance IsObjCObject (Id CBPeripheral) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBPeripheral"

class IsCBPeer a => IsCBPeripheral a where
  toCBPeripheral :: a -> Id CBPeripheral

instance IsCBPeripheral (Id CBPeripheral) where
  toCBPeripheral = unsafeCastId

instance IsCBPeer (Id CBPeripheral) where
  toCBPeer = unsafeCastId

instance IsNSObject (Id CBPeripheral) where
  toNSObject = unsafeCastId

-- ---------- CBMutableCharacteristic ----------

-- | CBMutableCharacteristic
--
-- Used to create a local characteristic, which can be added to the local database via CBPeripheralManager. Once a characteristic				is published, it is cached and can no longer be changed. 				If a characteristic value is specified, it will be cached and marked CBCharacteristicPropertyRead and 				CBAttributePermissionsReadable. If a characteristic value needs to be writeable, or may change during the lifetime of the				published CBService, it is considered a dynamic value and will be requested on-demand. Dynamic values are identified by a				value of nil.
-- 
-- Phantom type for @CBMutableCharacteristic@.
data CBMutableCharacteristic

instance IsObjCObject (Id CBMutableCharacteristic) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBMutableCharacteristic"

class IsCBCharacteristic a => IsCBMutableCharacteristic a where
  toCBMutableCharacteristic :: a -> Id CBMutableCharacteristic

instance IsCBMutableCharacteristic (Id CBMutableCharacteristic) where
  toCBMutableCharacteristic = unsafeCastId

instance IsCBAttribute (Id CBMutableCharacteristic) where
  toCBAttribute = unsafeCastId

instance IsCBCharacteristic (Id CBMutableCharacteristic) where
  toCBCharacteristic = unsafeCastId

instance IsNSObject (Id CBMutableCharacteristic) where
  toNSObject = unsafeCastId

-- ---------- CBMutableDescriptor ----------

-- | CBMutableDescriptor
--
-- Used to create a local characteristic descriptor, which can be added to the local database via CBPeripheralManager.		Once a descriptor is published, it is cached and can no longer be changed.		Descriptor types are detailed in
--
-- CBUUID.h
--
-- , but only the Characteristic User Description and Characteristic Presentation		Format descriptors are currently supported. The Characteristic Extended Properties and Client Characteristic 		Configuration descriptors will be created automatically upon publication of the parent service, depending on the properties of the characteristic itself.
-- 
-- Phantom type for @CBMutableDescriptor@.
data CBMutableDescriptor

instance IsObjCObject (Id CBMutableDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBMutableDescriptor"

class IsCBDescriptor a => IsCBMutableDescriptor a where
  toCBMutableDescriptor :: a -> Id CBMutableDescriptor

instance IsCBMutableDescriptor (Id CBMutableDescriptor) where
  toCBMutableDescriptor = unsafeCastId

instance IsCBAttribute (Id CBMutableDescriptor) where
  toCBAttribute = unsafeCastId

instance IsCBDescriptor (Id CBMutableDescriptor) where
  toCBDescriptor = unsafeCastId

instance IsNSObject (Id CBMutableDescriptor) where
  toNSObject = unsafeCastId

-- ---------- CBMutableService ----------

-- | CBMutableService
--
-- Used to create a local service or included service, which can be added to the local database via CBPeripheralManager.		Once a service is published, it is cached and can no longer be changed. This class adds write access to all properties in the
--
-- CBService
--
-- class.
-- 
-- Phantom type for @CBMutableService@.
data CBMutableService

instance IsObjCObject (Id CBMutableService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CBMutableService"

class IsCBService a => IsCBMutableService a where
  toCBMutableService :: a -> Id CBMutableService

instance IsCBMutableService (Id CBMutableService) where
  toCBMutableService = unsafeCastId

instance IsCBAttribute (Id CBMutableService) where
  toCBAttribute = unsafeCastId

instance IsCBService (Id CBMutableService) where
  toCBService = unsafeCastId

instance IsNSObject (Id CBMutableService) where
  toNSObject = unsafeCastId
