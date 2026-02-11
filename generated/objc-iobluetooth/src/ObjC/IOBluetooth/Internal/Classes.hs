{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.IOBluetooth.Internal.Classes (
    module ObjC.IOBluetooth.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- BluetoothFileReference ----------

-- | OBEXFileTransferServices
--
-- Implements advanced OBEX operations in addition to simple PUT and GET.
--
-- All operations are asynchronous and will callback over a respective delegate 					method if the initial return value is successful.  The initial return value 					usually concerns the state of this object where as the delegate return value					reflects the response of the remote device.
-- 
-- Phantom type for @BluetoothFileReference@.
data BluetoothFileReference

instance IsObjCObject (Id BluetoothFileReference) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BluetoothFileReference"

class IsObjCObject a => IsBluetoothFileReference a where
  toBluetoothFileReference :: a -> Id BluetoothFileReference

instance IsBluetoothFileReference (Id BluetoothFileReference) where
  toBluetoothFileReference = unsafeCastId

-- ---------- IOBluetoothDeviceInquiry ----------

-- | IOBluetoothDeviceInquiry
--
-- Object representing a device inquiry that finds Bluetooth devices in-range of the computer,					and (optionally) retrieves name information for them.
--
-- You should only use this object if your application needs to know about in-range devices and cannot					use the GUI provided by the IOBluetoothUI framework. It will not let you perform unlimited back-to-back					inquiries, but will instead throttle the number of attempted inquiries if too many are attempted within					a small window of time.					Important Note: DO NOT perform remote name requests on devices from delegate methods or while this					object is in use. If you wish to do your own remote name requests on devices, do them after you have					stopped this object. If you do not heed this warning, you could potentially deadlock your process.
-- 
-- Phantom type for @IOBluetoothDeviceInquiry@.
data IOBluetoothDeviceInquiry

instance IsObjCObject (Id IOBluetoothDeviceInquiry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothDeviceInquiry"

class IsNSObject a => IsIOBluetoothDeviceInquiry a where
  toIOBluetoothDeviceInquiry :: a -> Id IOBluetoothDeviceInquiry

instance IsIOBluetoothDeviceInquiry (Id IOBluetoothDeviceInquiry) where
  toIOBluetoothDeviceInquiry = unsafeCastId

instance IsNSObject (Id IOBluetoothDeviceInquiry) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothDevicePair ----------

-- | IOBluetoothDevicePair
--
-- An instance of IOBluetoothDevicePair represents a pairing attempt to a remote Bluetooth device.
--
-- Use the IOBluetoothDevicePair object to attempt to pair with any Bluetooth device. Once -start is invoked				on it, progress is returned to the delegate via the messages defined below. This object enables you to				pair with devices within your application without having to use the standard panels provided by the				IOBluetoothUI framework, allowing you to write custom UI to select devices, and still handle the ability				to perform device pairings.
--
-- Of note is that this object MAY attempt to perform two low-level pairings, depending on the type of device				you are attempting to pair. This is inconsequential to your code, however, as it occurs automatically and				does not change the messaging.
--
-- Once started, the pairing can be stopped. This will set the delegate to nil and then attempt to disconnect 				from the device if already connected.
-- 
-- Phantom type for @IOBluetoothDevicePair@.
data IOBluetoothDevicePair

instance IsObjCObject (Id IOBluetoothDevicePair) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothDevicePair"

class IsNSObject a => IsIOBluetoothDevicePair a where
  toIOBluetoothDevicePair :: a -> Id IOBluetoothDevicePair

instance IsIOBluetoothDevicePair (Id IOBluetoothDevicePair) where
  toIOBluetoothDevicePair = unsafeCastId

instance IsNSObject (Id IOBluetoothDevicePair) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothHandsFree ----------

-- | IOBluetoothHandsFree
--
-- Hands free profile class.
--
-- Superclass of IOBluetoothHandsFreeDevice and IOBluetoothHandsFreeAudioGateway classes. Contains the common code used to support the bluetoooth hands free profile.
--
-- IOBluetoothHandsFreeDevice
--
-- IOBluetoothHandsFreeAudioGateway
-- 
-- Phantom type for @IOBluetoothHandsFree@.
data IOBluetoothHandsFree

instance IsObjCObject (Id IOBluetoothHandsFree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothHandsFree"

class IsNSObject a => IsIOBluetoothHandsFree a where
  toIOBluetoothHandsFree :: a -> Id IOBluetoothHandsFree

instance IsIOBluetoothHandsFree (Id IOBluetoothHandsFree) where
  toIOBluetoothHandsFree = unsafeCastId

instance IsNSObject (Id IOBluetoothHandsFree) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothHostController ----------

-- | IOBluetoothHostController
--
-- This class is a representation of a Bluetooth Host Controller Interface that is present on the					local computer (either plugged in externally or available internally).
--
-- This object can be used to ask a Bluetooth HCI for certain pieces of information, and be used to make					it perform certain functions.
-- 
-- Phantom type for @IOBluetoothHostController@.
data IOBluetoothHostController

instance IsObjCObject (Id IOBluetoothHostController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothHostController"

class IsNSObject a => IsIOBluetoothHostController a where
  toIOBluetoothHostController :: a -> Id IOBluetoothHostController

instance IsIOBluetoothHostController (Id IOBluetoothHostController) where
  toIOBluetoothHostController = unsafeCastId

instance IsNSObject (Id IOBluetoothHostController) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothObject ----------

-- | Phantom type for @IOBluetoothObject@.
data IOBluetoothObject

instance IsObjCObject (Id IOBluetoothObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothObject"

class IsNSObject a => IsIOBluetoothObject a where
  toIOBluetoothObject :: a -> Id IOBluetoothObject

instance IsIOBluetoothObject (Id IOBluetoothObject) where
  toIOBluetoothObject = unsafeCastId

instance IsNSObject (Id IOBluetoothObject) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothSDPDataElement ----------

-- | IOBluetoothSDPDataElement
--
-- An instance of this class represents a single SDP data element as defined by the Bluetooth SDP spec.
--
-- The data types described by the spec have been mapped onto the base Foundation classes NSNumber,                 NSArray, NSData as well as IOBluetoothSDPUUID.  The number and boolean types (type descriptor 1, 2                 and 5) are represented as NSNumber objects with the exception of 128-bit numbers which are                 represented as NSData objects in their raw format.  The UUID type (type descriptor 3) is                 represented by IOBluetoothSDPUUID.  The string and URL types (type descriptor 4 and 8) are                 represented by NSString.  The sequence types (type descriptor 6 and 7) are represented by NSArray.
--
-- Typically, you will not need to create an IOBluetoothSDPDataElement directly, the system will                do that automatically for both client and server operations.  However, the current API for adding                 SDP services to the system does allow the use of an NSDictionary based format for creating new                 services.  The purpose for that is to allow a service to be built up completely in a text file                (a plist for example) and then easily imported into an app and added to the system without a                 lot of tedious code to build up the entire SDP service record.
--
-- The basis for that NSDictionary structure comes from the IOBluetoothSDPDataElement.  At its                simplest, a data element is made up of three parts: the type descriptor, the size (from which                the size descriptor is generated) and the actual value.  To provide a complete representation                of a data element, an NSDictionary with three entries can be used.  Each of the three entries                has a key/value pair representing one of the three attributes of a data element.  The first                key/value pair has a key 'DataElementType' that contains a number value with the actual                type descriptor for the data element.  The second pair has a key 'DataElementSize' that                contains the actual size of the element in bytes.  The size descriptor will be calculated                based on the size and type of the element.  The third pair is the value itself whose key is                'DataElementValue' and whose type corresponds to the type mapping above.
--
-- In addition to this complete description of a data element, their are some shortcuts that                can be used for some of the common types and sizes.
--
-- If the 'DataElementType' value is one of the numeric types (1, 2), the 'DataElementValue'                 can be an NSData instead of an NSNumber.  In that case, the numeric data is taken in network                 byte order (MSB first).  Additionally, the 'DataElementSize' parameter may be omitted and the                 size will be taken from the length of the data object.
--
-- If the 'DataElementType' value is the nil type (0), no 'DataElementSize' or 'DataElementValue'                entries are needed.
--
-- If the 'DataElementType' value is any of the other types, the 'DataElementSize' entry is not                needed since the size will be taken directly from the value (data, array, string).
--
-- In the case where the element is an unsigned, 32-bit integer (type descriptor 1, size descriptor                4), the value itself may simply be a number (instead of a dictionary as in the previous examples).
--
-- In the case where the element is a UUID (type descriptor 3), the value itself may be a data object.                  The UUID type will be inferred and the size taken from the length of the data object.
--
-- In the case where the element is a text string (type descriptor 4), the value may be a string object.                The text string type will be inferred and the size taken from the length of the string.
--
-- In the case where the element is a data element sequence, the value may be an array object.  The                type will be inferred and the size taken from the length of the array.  Additionally, the array                must contain sub-elements that will be parsed out individually.
-- 
-- Phantom type for @IOBluetoothSDPDataElement@.
data IOBluetoothSDPDataElement

instance IsObjCObject (Id IOBluetoothSDPDataElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothSDPDataElement"

class IsNSObject a => IsIOBluetoothSDPDataElement a where
  toIOBluetoothSDPDataElement :: a -> Id IOBluetoothSDPDataElement

instance IsIOBluetoothSDPDataElement (Id IOBluetoothSDPDataElement) where
  toIOBluetoothSDPDataElement = unsafeCastId

instance IsNSObject (Id IOBluetoothSDPDataElement) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothSDPServiceAttribute ----------

-- | IOBluetoothSDPServiceAttribute
--
-- IOBluetoothSDPServiceAttribute represents a single SDP service attribute.
--
-- A service attribute contains two components: an attribute ID and a data element.
-- 
-- Phantom type for @IOBluetoothSDPServiceAttribute@.
data IOBluetoothSDPServiceAttribute

instance IsObjCObject (Id IOBluetoothSDPServiceAttribute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothSDPServiceAttribute"

class IsNSObject a => IsIOBluetoothSDPServiceAttribute a where
  toIOBluetoothSDPServiceAttribute :: a -> Id IOBluetoothSDPServiceAttribute

instance IsIOBluetoothSDPServiceAttribute (Id IOBluetoothSDPServiceAttribute) where
  toIOBluetoothSDPServiceAttribute = unsafeCastId

instance IsNSObject (Id IOBluetoothSDPServiceAttribute) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothSDPServiceRecord ----------

-- | IOBluetoothSDPServiceRecord
--
-- An instance of this class represents a single SDP service record.
--
-- As a service record, an instance of this class has an NSDictionary of service attributes.                It also has a link to the IOBluetoothDevice that the service belongs to.  The service                dictionary is keyed off of the attribute ID of each attribute represented as an NSNumber.
-- 
-- Phantom type for @IOBluetoothSDPServiceRecord@.
data IOBluetoothSDPServiceRecord

instance IsObjCObject (Id IOBluetoothSDPServiceRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothSDPServiceRecord"

class IsNSObject a => IsIOBluetoothSDPServiceRecord a where
  toIOBluetoothSDPServiceRecord :: a -> Id IOBluetoothSDPServiceRecord

instance IsIOBluetoothSDPServiceRecord (Id IOBluetoothSDPServiceRecord) where
  toIOBluetoothSDPServiceRecord = unsafeCastId

instance IsNSObject (Id IOBluetoothSDPServiceRecord) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothUserNotification ----------

-- | IOBluetoothUserNotification
--
-- Represents a registered notification.
--
-- When registering for various notifications in the system, an IOBluetoothUserNotification				object is returned.  To unregister from the notification, call -unregister on the				IOBluetoothUserNotification object.  Once -unregister is called, the object will no				longer be valid.
-- 
-- Phantom type for @IOBluetoothUserNotification@.
data IOBluetoothUserNotification

instance IsObjCObject (Id IOBluetoothUserNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothUserNotification"

class IsNSObject a => IsIOBluetoothUserNotification a where
  toIOBluetoothUserNotification :: a -> Id IOBluetoothUserNotification

instance IsIOBluetoothUserNotification (Id IOBluetoothUserNotification) where
  toIOBluetoothUserNotification = unsafeCastId

instance IsNSObject (Id IOBluetoothUserNotification) where
  toNSObject = unsafeCastId

-- ---------- OBEXFileTransferServices ----------

-- | Phantom type for @OBEXFileTransferServices@.
data OBEXFileTransferServices

instance IsObjCObject (Id OBEXFileTransferServices) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OBEXFileTransferServices"

class IsNSObject a => IsOBEXFileTransferServices a where
  toOBEXFileTransferServices :: a -> Id OBEXFileTransferServices

instance IsOBEXFileTransferServices (Id OBEXFileTransferServices) where
  toOBEXFileTransferServices = unsafeCastId

instance IsNSObject (Id OBEXFileTransferServices) where
  toNSObject = unsafeCastId

-- ---------- OBEXSession ----------

-- | Phantom type for @OBEXSession@.
data OBEXSession

instance IsObjCObject (Id OBEXSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OBEXSession"

class IsNSObject a => IsOBEXSession a where
  toOBEXSession :: a -> Id OBEXSession

instance IsOBEXSession (Id OBEXSession) where
  toOBEXSession = unsafeCastId

instance IsNSObject (Id OBEXSession) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothHandsFreeAudioGateway ----------

-- | APIs for managing a hands free audio gateway
--
-- (c) 2010 by Apple Inc. All rights reserved.
-- 
-- Phantom type for @IOBluetoothHandsFreeAudioGateway@.
data IOBluetoothHandsFreeAudioGateway

instance IsObjCObject (Id IOBluetoothHandsFreeAudioGateway) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothHandsFreeAudioGateway"

class IsIOBluetoothHandsFree a => IsIOBluetoothHandsFreeAudioGateway a where
  toIOBluetoothHandsFreeAudioGateway :: a -> Id IOBluetoothHandsFreeAudioGateway

instance IsIOBluetoothHandsFreeAudioGateway (Id IOBluetoothHandsFreeAudioGateway) where
  toIOBluetoothHandsFreeAudioGateway = unsafeCastId

instance IsIOBluetoothHandsFree (Id IOBluetoothHandsFreeAudioGateway) where
  toIOBluetoothHandsFree = unsafeCastId

instance IsNSObject (Id IOBluetoothHandsFreeAudioGateway) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothHandsFreeDevice ----------

-- | APIs for managing a hands free device
--
-- (c) 2010 by Apple Inc. All rights reserved.
-- 
-- Phantom type for @IOBluetoothHandsFreeDevice@.
data IOBluetoothHandsFreeDevice

instance IsObjCObject (Id IOBluetoothHandsFreeDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothHandsFreeDevice"

class IsIOBluetoothHandsFree a => IsIOBluetoothHandsFreeDevice a where
  toIOBluetoothHandsFreeDevice :: a -> Id IOBluetoothHandsFreeDevice

instance IsIOBluetoothHandsFreeDevice (Id IOBluetoothHandsFreeDevice) where
  toIOBluetoothHandsFreeDevice = unsafeCastId

instance IsIOBluetoothHandsFree (Id IOBluetoothHandsFreeDevice) where
  toIOBluetoothHandsFree = unsafeCastId

instance IsNSObject (Id IOBluetoothHandsFreeDevice) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothDevice ----------

-- | IOBluetoothDevice
--
-- An instance of IOBluetoothDevice represents a single remote Bluetooth device.
--
-- An IOBluetoothDevice object may exist independent of the existence of a baseband connection                with the target device.  Using this object, a client can request creation and destruction of baseband                connections, and request the opening of L2CAP and RFCOMM channels on the remote device.  Many of the other                APIs in the IOBluetooth framework will return this object, or it's C counterpart (IOBluetoothDeviceRef).
-- 
-- Phantom type for @IOBluetoothDevice@.
data IOBluetoothDevice

instance IsObjCObject (Id IOBluetoothDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothDevice"

class IsIOBluetoothObject a => IsIOBluetoothDevice a where
  toIOBluetoothDevice :: a -> Id IOBluetoothDevice

instance IsIOBluetoothDevice (Id IOBluetoothDevice) where
  toIOBluetoothDevice = unsafeCastId

instance IsIOBluetoothObject (Id IOBluetoothDevice) where
  toIOBluetoothObject = unsafeCastId

instance IsNSObject (Id IOBluetoothDevice) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothL2CAPChannel ----------

-- | IOBluetoothL2CAPChannel
--
-- An instance of IOBluetoothL2CAPChannel represents a single open L2CAP channel.
--
-- A client won't create IOBluetoothL2CAPChannel objects directly.  Instead, the IOBluetoothDevice's                 L2CAP channel open API is responsible for opening a new L2CAP channel and returning an                IOBluetoothL2CAPChannel instance representing that newly opened channel.  Additionally, the IOBluetooth                notification system will send notifications when new L2CAP channels are open (if requested).
--
-- After a new L2CAP channel is opened, the L2CAP configuration process will not be completed until an                incoming data listener is registered with the IOBluetoothL2CAPChannel object.  The reason for this is                to due to the limited buffering done of incoming L2CAP data.  This way, we avoid the situation where                 incoming data is received before the client is ready for it.  Once a client is done with an                IOBluetoothL2CAPChannel that it opened, it should call -closeChannel.  Additionally, if the client                does not intend to use the connection to the remote device any further, it should call -closeConnection                on the IOBluetoothDevice object.
-- 
-- Phantom type for @IOBluetoothL2CAPChannel@.
data IOBluetoothL2CAPChannel

instance IsObjCObject (Id IOBluetoothL2CAPChannel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothL2CAPChannel"

class IsIOBluetoothObject a => IsIOBluetoothL2CAPChannel a where
  toIOBluetoothL2CAPChannel :: a -> Id IOBluetoothL2CAPChannel

instance IsIOBluetoothL2CAPChannel (Id IOBluetoothL2CAPChannel) where
  toIOBluetoothL2CAPChannel = unsafeCastId

instance IsIOBluetoothObject (Id IOBluetoothL2CAPChannel) where
  toIOBluetoothObject = unsafeCastId

instance IsNSObject (Id IOBluetoothL2CAPChannel) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothRFCOMMChannel ----------

-- | IOBluetoothRFCOMMChannel
--
-- An instance of this class represents an rfcomm channel as defined by the Bluetooth SDP spec..
--
-- An RFCOMM channel object can be obtained by opening an rfcomm channel in a device, or    by requesting a notification when a channel is created (this is commonly used to provide services).
-- 
-- Phantom type for @IOBluetoothRFCOMMChannel@.
data IOBluetoothRFCOMMChannel

instance IsObjCObject (Id IOBluetoothRFCOMMChannel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothRFCOMMChannel"

class IsIOBluetoothObject a => IsIOBluetoothRFCOMMChannel a where
  toIOBluetoothRFCOMMChannel :: a -> Id IOBluetoothRFCOMMChannel

instance IsIOBluetoothRFCOMMChannel (Id IOBluetoothRFCOMMChannel) where
  toIOBluetoothRFCOMMChannel = unsafeCastId

instance IsIOBluetoothObject (Id IOBluetoothRFCOMMChannel) where
  toIOBluetoothObject = unsafeCastId

instance IsNSObject (Id IOBluetoothRFCOMMChannel) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothSDPUUID ----------

-- | IOBluetoothSDPUUID
--
-- An NSData subclass that represents a UUID as defined in the Bluetooth SDP spec.
--
-- The IOBluetoothSDPUUID class can represent a UUID of any valid size (16, 32 or 128 bits).            It provides the ability to compare two UUIDs no matter what their size as well as the ability            to promote the size of a UUID to a larger one.
-- 
-- Phantom type for @IOBluetoothSDPUUID@.
data IOBluetoothSDPUUID

instance IsObjCObject (Id IOBluetoothSDPUUID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothSDPUUID"

class IsNSData a => IsIOBluetoothSDPUUID a where
  toIOBluetoothSDPUUID :: a -> Id IOBluetoothSDPUUID

instance IsIOBluetoothSDPUUID (Id IOBluetoothSDPUUID) where
  toIOBluetoothSDPUUID = unsafeCastId

instance IsNSData (Id IOBluetoothSDPUUID) where
  toNSData = unsafeCastId

instance IsNSObject (Id IOBluetoothSDPUUID) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothOBEXSession ----------

-- | Phantom type for @IOBluetoothOBEXSession@.
data IOBluetoothOBEXSession

instance IsObjCObject (Id IOBluetoothOBEXSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothOBEXSession"

class IsOBEXSession a => IsIOBluetoothOBEXSession a where
  toIOBluetoothOBEXSession :: a -> Id IOBluetoothOBEXSession

instance IsIOBluetoothOBEXSession (Id IOBluetoothOBEXSession) where
  toIOBluetoothOBEXSession = unsafeCastId

instance IsNSObject (Id IOBluetoothOBEXSession) where
  toNSObject = unsafeCastId

instance IsOBEXSession (Id IOBluetoothOBEXSession) where
  toOBEXSession = unsafeCastId
