{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AudioVideoBridging.Internal.Classes (
    module ObjC.AudioVideoBridging.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- AVB17221ACMPMessage ----------

-- | AVB17221ACMPMessage
--
-- AVB17221ACMPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol message.
--
-- AVB17221ACMPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol (ACMP) message.
-- 
-- Phantom type for @AVB17221ACMPMessage@.
data AVB17221ACMPMessage

instance IsObjCObject (Id AVB17221ACMPMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221ACMPMessage"

class IsNSObject a => IsAVB17221ACMPMessage a where
  toAVB17221ACMPMessage :: a -> Id AVB17221ACMPMessage

instance IsAVB17221ACMPMessage (Id AVB17221ACMPMessage) where
  toAVB17221ACMPMessage = unsafeCastId

instance IsNSObject (Id AVB17221ACMPMessage) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPAddressAccessTLV ----------

-- | AVB17221AECPAddressAccessTLV
--
-- AVB17221AECPAddressAccessTLV encapsulates a TLV from an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Address Access message.
--
-- AVB17221AECPAddressAccessTLV encapsulates a TLV from an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Address Access message.
-- 
-- Phantom type for @AVB17221AECPAddressAccessTLV@.
data AVB17221AECPAddressAccessTLV

instance IsObjCObject (Id AVB17221AECPAddressAccessTLV) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPAddressAccessTLV"

class IsNSObject a => IsAVB17221AECPAddressAccessTLV a where
  toAVB17221AECPAddressAccessTLV :: a -> Id AVB17221AECPAddressAccessTLV

instance IsAVB17221AECPAddressAccessTLV (Id AVB17221AECPAddressAccessTLV) where
  toAVB17221AECPAddressAccessTLV = unsafeCastId

instance IsNSObject (Id AVB17221AECPAddressAccessTLV) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPMessage ----------

-- | AVB17221AECPMessage
--
-- AVB17221AECPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol message.
--
-- AVB17221AECPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP) message.				This class is a abstract class providing the support for the common format shared between the different				AECP message types.
-- 
-- Phantom type for @AVB17221AECPMessage@.
data AVB17221AECPMessage

instance IsObjCObject (Id AVB17221AECPMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPMessage"

class IsNSObject a => IsAVB17221AECPMessage a where
  toAVB17221AECPMessage :: a -> Id AVB17221AECPMessage

instance IsAVB17221AECPMessage (Id AVB17221AECPMessage) where
  toAVB17221AECPMessage = unsafeCastId

instance IsNSObject (Id AVB17221AECPMessage) where
  toNSObject = unsafeCastId

-- ---------- AVB17221Entity ----------

-- | AVB17221Entity
--
-- AVB17221Entity class represents an entity that has been discovered on the network.
--
-- AVB17221Entity class represents an entity that has been discovered on the network.				AVB17221Entity objects are created by the AVB17221EntityDiscovery object as they are discovered, 				and passed around to the discovery delegates when notifying them of changes in the state of the network.				Changes include an entity being added, removed or rediscovered. Entities register themselves to receive				automatic updates when any of the discovery values change.
-- 
-- Phantom type for @AVB17221Entity@.
data AVB17221Entity

instance IsObjCObject (Id AVB17221Entity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221Entity"

class IsNSObject a => IsAVB17221Entity a where
  toAVB17221Entity :: a -> Id AVB17221Entity

instance IsAVB17221Entity (Id AVB17221Entity) where
  toAVB17221Entity = unsafeCastId

instance IsNSObject (Id AVB17221Entity) where
  toNSObject = unsafeCastId

-- ---------- AVB17221EntityDiscovery ----------

-- | AVB17221EntityDiscovery
--
-- AVB17221EntityDiscovery provides access to the IEEE Std 1722.1™-2013 AVDECC Discovery Protocol (ADP) interface.
--
-- AVB17221EntityDiscovery provides access to the IEEE Std 1722.1™-2013 AVDECC Discovery Protocol (ADP) interface.				It provides a delegate with callbacks whenever an entity is added or removed, 				either locally or remotely. AVB17221EntityDiscovery objects are typically not created 				directly but are created indirectly and accessed via the entityDiscovery property of 				the AVBInterface object.
--
-- The AVBInterface object does not register a delegate with the AVB17221EntityDiscovery object				which is allocated. Immediately after obtaining the entityDiscovery value for the first time,				the discoveryDelegate should be set and the primeIterators method should be called. Until 				primeIterators is called, no entities will be discovered.
-- 
-- Phantom type for @AVB17221EntityDiscovery@.
data AVB17221EntityDiscovery

instance IsObjCObject (Id AVB17221EntityDiscovery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221EntityDiscovery"

class IsNSObject a => IsAVB17221EntityDiscovery a where
  toAVB17221EntityDiscovery :: a -> Id AVB17221EntityDiscovery

instance IsAVB17221EntityDiscovery (Id AVB17221EntityDiscovery) where
  toAVB17221EntityDiscovery = unsafeCastId

instance IsNSObject (Id AVB17221EntityDiscovery) where
  toNSObject = unsafeCastId

-- ---------- AVB1722ControlInterface ----------

-- | AVB1722ControlInterface
--
-- AVB1722ControlInterface is an abstract class providing the common API for utilizing control services based on IEEE 1722-2011 control frames.
--
-- AVB1722ControlInterface is an abstract class providing the common API for utilizing control services based on IEEE 1722-2011 control frames.				It provides the API for the basic IOKit interactions to talk to the kernel driver.
-- 
-- Phantom type for @AVB1722ControlInterface@.
data AVB1722ControlInterface

instance IsObjCObject (Id AVB1722ControlInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB1722ControlInterface"

class IsNSObject a => IsAVB1722ControlInterface a where
  toAVB1722ControlInterface :: a -> Id AVB1722ControlInterface

instance IsAVB1722ControlInterface (Id AVB1722ControlInterface) where
  toAVB1722ControlInterface = unsafeCastId

instance IsNSObject (Id AVB1722ControlInterface) where
  toNSObject = unsafeCastId

-- ---------- AVBCentralManager ----------

-- | AVBCentralManager
--
-- AVBCentralManager provides centralized management of AVBInterface objects for the network interfaces of the computer.
--
-- AVBCentralManager provides centralized management of the AVBInterface subclasses for the network interfaces of the computer.				Subclasses override the didAddInterface: and didRemoveInterface: methods to be notified when an 				interface is added to or removed from the computer. Addition and removal can happen for any interface but is typically associated				with the Thunderbolt Ethernet Adapter.
-- 
-- Phantom type for @AVBCentralManager@.
data AVBCentralManager

instance IsObjCObject (Id AVBCentralManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVBCentralManager"

class IsNSObject a => IsAVBCentralManager a where
  toAVBCentralManager :: a -> Id AVBCentralManager

instance IsAVBCentralManager (Id AVBCentralManager) where
  toAVBCentralManager = unsafeCastId

instance IsNSObject (Id AVBCentralManager) where
  toNSObject = unsafeCastId

-- ---------- AVBIPAddress ----------

-- | AVBIPAddress
--
-- AVBIPAddress is a class for holding and representing an IP Address, either IPv4 or IPv6.
-- 
-- Phantom type for @AVBIPAddress@.
data AVBIPAddress

instance IsObjCObject (Id AVBIPAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVBIPAddress"

class IsNSObject a => IsAVBIPAddress a where
  toAVBIPAddress :: a -> Id AVBIPAddress

instance IsAVBIPAddress (Id AVBIPAddress) where
  toAVBIPAddress = unsafeCastId

instance IsNSObject (Id AVBIPAddress) where
  toNSObject = unsafeCastId

-- ---------- AVBInterface ----------

-- | AVBInterface
--
-- AVBInterface is an abstract class providing a central access point for the AVB functionality of an interface.
--
-- AVBInterface is an abstract class providing a central access point for the AVB functionality of an interface.				AVBInterface objects should not be directly created as they cannot provide full functionality, instead a concrete				subclass should be instantiated.
-- 
-- Phantom type for @AVBInterface@.
data AVBInterface

instance IsObjCObject (Id AVBInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVBInterface"

class IsNSObject a => IsAVBInterface a where
  toAVBInterface :: a -> Id AVBInterface

instance IsAVBInterface (Id AVBInterface) where
  toAVBInterface = unsafeCastId

instance IsNSObject (Id AVBInterface) where
  toNSObject = unsafeCastId

-- ---------- AVBMACAddress ----------

-- | AVBMACAddress
--
-- AVBMACAddress is a class for holding and representing an Ethernet MAC Address.
-- 
-- Phantom type for @AVBMACAddress@.
data AVBMACAddress

instance IsObjCObject (Id AVBMACAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVBMACAddress"

class IsNSObject a => IsAVBMACAddress a where
  toAVBMACAddress :: a -> Id AVBMACAddress

instance IsAVBMACAddress (Id AVBMACAddress) where
  toAVBMACAddress = unsafeCastId

instance IsNSObject (Id AVBMACAddress) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPAEMMessage ----------

-- | AVB17221AECPAEMMessage
--
-- AVB17221AECPAEMMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, AVDECC Entity Model message.
--
-- AVB17221AECPAEMMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), AVDECC Entity Model (AEM) message. This class is a concrete subclass of AVB17221AECPMessage which provides support for the AEM messages.
-- 
-- Phantom type for @AVB17221AECPAEMMessage@.
data AVB17221AECPAEMMessage

instance IsObjCObject (Id AVB17221AECPAEMMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPAEMMessage"

class IsAVB17221AECPMessage a => IsAVB17221AECPAEMMessage a where
  toAVB17221AECPAEMMessage :: a -> Id AVB17221AECPAEMMessage

instance IsAVB17221AECPAEMMessage (Id AVB17221AECPAEMMessage) where
  toAVB17221AECPAEMMessage = unsafeCastId

instance IsAVB17221AECPMessage (Id AVB17221AECPAEMMessage) where
  toAVB17221AECPMessage = unsafeCastId

instance IsNSObject (Id AVB17221AECPAEMMessage) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPAVCMessage ----------

-- | AVB17221AECPAVCMessage
--
-- AVB17221AECPAVCMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Legacy AV/C message.
--
-- AVB17221AECPAVCMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Legacy AV/C message.				This class is a concrete subclass of AVB17221AECPMessage which provides support for the Legacy AV/C messages.
-- 
-- Phantom type for @AVB17221AECPAVCMessage@.
data AVB17221AECPAVCMessage

instance IsObjCObject (Id AVB17221AECPAVCMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPAVCMessage"

class IsAVB17221AECPMessage a => IsAVB17221AECPAVCMessage a where
  toAVB17221AECPAVCMessage :: a -> Id AVB17221AECPAVCMessage

instance IsAVB17221AECPAVCMessage (Id AVB17221AECPAVCMessage) where
  toAVB17221AECPAVCMessage = unsafeCastId

instance IsAVB17221AECPMessage (Id AVB17221AECPAVCMessage) where
  toAVB17221AECPMessage = unsafeCastId

instance IsNSObject (Id AVB17221AECPAVCMessage) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPAddressAccessMessage ----------

-- | AVB17221AECPAddressAccessMessage
--
-- AVB17221AECPAddressAccessMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Address Access message.
--
-- AVB17221AECPAddressAccessMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Address Access message.				This class is a concrete subclass of AVB17221AECPMessage which provides support for the Address Access messages.
-- 
-- Phantom type for @AVB17221AECPAddressAccessMessage@.
data AVB17221AECPAddressAccessMessage

instance IsObjCObject (Id AVB17221AECPAddressAccessMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPAddressAccessMessage"

class IsAVB17221AECPMessage a => IsAVB17221AECPAddressAccessMessage a where
  toAVB17221AECPAddressAccessMessage :: a -> Id AVB17221AECPAddressAccessMessage

instance IsAVB17221AECPAddressAccessMessage (Id AVB17221AECPAddressAccessMessage) where
  toAVB17221AECPAddressAccessMessage = unsafeCastId

instance IsAVB17221AECPMessage (Id AVB17221AECPAddressAccessMessage) where
  toAVB17221AECPMessage = unsafeCastId

instance IsNSObject (Id AVB17221AECPAddressAccessMessage) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPVendorMessage ----------

-- | AVB17221AECPVendorMessage
--
-- AVB17221AECPVendorMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol, Vendor Unique message.
--
-- AVB17221AECPVendorMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP), Vendor Unique message.				This class is a concrete subclass of AVB17221AECPMessage which provides support for the AEM messages.
-- 
-- Phantom type for @AVB17221AECPVendorMessage@.
data AVB17221AECPVendorMessage

instance IsObjCObject (Id AVB17221AECPVendorMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPVendorMessage"

class IsAVB17221AECPMessage a => IsAVB17221AECPVendorMessage a where
  toAVB17221AECPVendorMessage :: a -> Id AVB17221AECPVendorMessage

instance IsAVB17221AECPVendorMessage (Id AVB17221AECPVendorMessage) where
  toAVB17221AECPVendorMessage = unsafeCastId

instance IsAVB17221AECPMessage (Id AVB17221AECPVendorMessage) where
  toAVB17221AECPMessage = unsafeCastId

instance IsNSObject (Id AVB17221AECPVendorMessage) where
  toNSObject = unsafeCastId

-- ---------- AVB17221ACMPInterface ----------

-- | AVB17221ACMPInterface
--
-- AVB17221ACMPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol interface.
--
-- AVB17221ACMPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol (ACMP) interface.				It provides callbacks per entity EntityID via a handler object implementing the AVB17221ACMPClient protocol. AVB17221ACMPInterface objects				are typically not created directly but are created indirectly and accessed via the acmp property of the AVBInterface object.
-- 
-- Phantom type for @AVB17221ACMPInterface@.
data AVB17221ACMPInterface

instance IsObjCObject (Id AVB17221ACMPInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221ACMPInterface"

class IsAVB1722ControlInterface a => IsAVB17221ACMPInterface a where
  toAVB17221ACMPInterface :: a -> Id AVB17221ACMPInterface

instance IsAVB17221ACMPInterface (Id AVB17221ACMPInterface) where
  toAVB17221ACMPInterface = unsafeCastId

instance IsAVB1722ControlInterface (Id AVB17221ACMPInterface) where
  toAVB1722ControlInterface = unsafeCastId

instance IsNSObject (Id AVB17221ACMPInterface) where
  toNSObject = unsafeCastId

-- ---------- AVB17221AECPInterface ----------

-- | AVB17221AECPInterface
--
-- AVB17221AECPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol interface.
--
-- AVB17221AECPInterface is a concrete subclass of AVB1722ControlInterface providing the access to the IEEE Std 1722.1™-2013 AVDECC Enumeration and Control Protocol (AECP) interface.				It provides callbacks per entity EntityID via a handler object implementing the AVB17221AECPClient protocol. AVB17221AECPInterface objects				are typically not created directly but are created indirectly and accessed via the aecp property of the AVBInterface object.
-- 
-- Phantom type for @AVB17221AECPInterface@.
data AVB17221AECPInterface

instance IsObjCObject (Id AVB17221AECPInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVB17221AECPInterface"

class IsAVB1722ControlInterface a => IsAVB17221AECPInterface a where
  toAVB17221AECPInterface :: a -> Id AVB17221AECPInterface

instance IsAVB17221AECPInterface (Id AVB17221AECPInterface) where
  toAVB17221AECPInterface = unsafeCastId

instance IsAVB1722ControlInterface (Id AVB17221AECPInterface) where
  toAVB1722ControlInterface = unsafeCastId

instance IsNSObject (Id AVB17221AECPInterface) where
  toNSObject = unsafeCastId

-- ---------- AVBEthernetInterface ----------

-- | AVBEthernetInterface
--
-- AVBEthernetInterface is a concrete subclass of AVBInterface providing access to the AVB services of the interface.
--
-- AVBEthernetInterface is a concrete subclass of AVBInterface providing access to the AVB services of the interface.				AVBEthernetInterface objects should be created for an IEEE 802.3 ethernet based interface on which AVB functionality 				is being used.
-- 
-- Phantom type for @AVBEthernetInterface@.
data AVBEthernetInterface

instance IsObjCObject (Id AVBEthernetInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVBEthernetInterface"

class IsAVBInterface a => IsAVBEthernetInterface a where
  toAVBEthernetInterface :: a -> Id AVBEthernetInterface

instance IsAVBEthernetInterface (Id AVBEthernetInterface) where
  toAVBEthernetInterface = unsafeCastId

instance IsAVBInterface (Id AVBEthernetInterface) where
  toAVBInterface = unsafeCastId

instance IsNSObject (Id AVBEthernetInterface) where
  toNSObject = unsafeCastId
