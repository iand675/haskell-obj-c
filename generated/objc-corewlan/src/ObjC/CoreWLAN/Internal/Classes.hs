{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreWLAN.Internal.Classes (
    module ObjC.CoreWLAN.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SecurityFoundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.SecurityFoundation.Internal.Classes

-- ---------- CWChannel ----------

-- | Represents an IEEE 802.11 channel.
--
-- The CWChannel class is used by both CWInterface and CWNetwork as a representation of an IEEE 802.11 Wi-Fi channel.
-- 
-- Phantom type for @CWChannel@.
data CWChannel

instance IsObjCObject (Id CWChannel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWChannel"

class IsNSObject a => IsCWChannel a where
  toCWChannel :: a -> Id CWChannel

instance IsCWChannel (Id CWChannel) where
  toCWChannel = unsafeCastId

instance IsNSObject (Id CWChannel) where
  toNSObject = unsafeCastId

-- ---------- CWConfiguration ----------

-- | Encapsulates the system configuration for a given Wi-Fi interface.
--
-- The CWConfiguration class contains basic network configuration settings and also the list of preferred networks. CWConfiguration is an immutable object. For changing configuration settings and/or the preferred networks list, see CWMutableConfiguration.
-- 
-- Phantom type for @CWConfiguration@.
data CWConfiguration

instance IsObjCObject (Id CWConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWConfiguration"

class IsNSObject a => IsCWConfiguration a where
  toCWConfiguration :: a -> Id CWConfiguration

instance IsCWConfiguration (Id CWConfiguration) where
  toCWConfiguration = unsafeCastId

instance IsNSObject (Id CWConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CWInterface ----------

-- | Control and query a Wi-Fi interface on OS X.
--
-- All actions performed by a CWInterface object are executed on the Wi-Fi device with the corresponding interface name.
-- 
-- Phantom type for @CWInterface@.
data CWInterface

instance IsObjCObject (Id CWInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWInterface"

class IsNSObject a => IsCWInterface a where
  toCWInterface :: a -> Id CWInterface

instance IsCWInterface (Id CWInterface) where
  toCWInterface = unsafeCastId

instance IsNSObject (Id CWInterface) where
  toNSObject = unsafeCastId

-- ---------- CWNetwork ----------

-- | Represents a device participating in a Wi-Fi network, providing accessors to various network attributes.
-- 
-- Phantom type for @CWNetwork@.
data CWNetwork

instance IsObjCObject (Id CWNetwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWNetwork"

class IsNSObject a => IsCWNetwork a where
  toCWNetwork :: a -> Id CWNetwork

instance IsCWNetwork (Id CWNetwork) where
  toCWNetwork = unsafeCastId

instance IsNSObject (Id CWNetwork) where
  toNSObject = unsafeCastId

-- ---------- CWNetworkProfile ----------

-- | Encapsulates a preferred network entry.
-- 
-- Phantom type for @CWNetworkProfile@.
data CWNetworkProfile

instance IsObjCObject (Id CWNetworkProfile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWNetworkProfile"

class IsNSObject a => IsCWNetworkProfile a where
  toCWNetworkProfile :: a -> Id CWNetworkProfile

instance IsCWNetworkProfile (Id CWNetworkProfile) where
  toCWNetworkProfile = unsafeCastId

instance IsNSObject (Id CWNetworkProfile) where
  toNSObject = unsafeCastId

-- ---------- CWWiFiClient ----------

-- | The interface to the Wi-Fi subsystem on OS X.
--
-- Provides access to all Wi-Fi interfaces and allows Wi-Fi clients to setup event notifications.
--
-- CWWiFiClient objects are heavy objects, therefore, clients of the CoreWLAN framework should use a single,  long-running instance rather than creating several short-lived instances.   For convenience, +[CWWiFiClient sharedWiFiClient] can be used to return a singleton instance.
--
-- The CWWiFiClient object should be used to instantiate CWInterface objects rather than using a CWInterface initializer directly.
-- 
-- Phantom type for @CWWiFiClient@.
data CWWiFiClient

instance IsObjCObject (Id CWWiFiClient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWWiFiClient"

class IsNSObject a => IsCWWiFiClient a where
  toCWWiFiClient :: a -> Id CWWiFiClient

instance IsCWWiFiClient (Id CWWiFiClient) where
  toCWWiFiClient = unsafeCastId

instance IsNSObject (Id CWWiFiClient) where
  toNSObject = unsafeCastId

-- ---------- CWMutableConfiguration ----------

-- | Mutable subclass of CWConfiguration.  Use this class for changing configuration settings and/or the preferred networks list.
--
-- To commit configuration changes, use -[CWInterface commitConfiguration:authorization:error:].
-- 
-- Phantom type for @CWMutableConfiguration@.
data CWMutableConfiguration

instance IsObjCObject (Id CWMutableConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWMutableConfiguration"

class IsCWConfiguration a => IsCWMutableConfiguration a where
  toCWMutableConfiguration :: a -> Id CWMutableConfiguration

instance IsCWMutableConfiguration (Id CWMutableConfiguration) where
  toCWMutableConfiguration = unsafeCastId

instance IsCWConfiguration (Id CWMutableConfiguration) where
  toCWConfiguration = unsafeCastId

instance IsNSObject (Id CWMutableConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CWMutableNetworkProfile ----------

-- | Mutable subclass of CWNetworkProfile.  Use this class for changing profile properties.
--
-- To commit Wi-Fi network profile changes, use -[CWMutableConfiguration setNetworkProfiles:] and  -[CWInterface commitConfiguration:authorization:error:].
-- 
-- Phantom type for @CWMutableNetworkProfile@.
data CWMutableNetworkProfile

instance IsObjCObject (Id CWMutableNetworkProfile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CWMutableNetworkProfile"

class IsCWNetworkProfile a => IsCWMutableNetworkProfile a where
  toCWMutableNetworkProfile :: a -> Id CWMutableNetworkProfile

instance IsCWMutableNetworkProfile (Id CWMutableNetworkProfile) where
  toCWMutableNetworkProfile = unsafeCastId

instance IsCWNetworkProfile (Id CWMutableNetworkProfile) where
  toCWNetworkProfile = unsafeCastId

instance IsNSObject (Id CWMutableNetworkProfile) where
  toNSObject = unsafeCastId
