{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AVRouting.Internal.Classes (
    module ObjC.AVRouting.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- AVCustomDeviceRoute ----------

-- | An object that represents a custom device route.
--
-- Use the value of a route’s ``AVCustomDeviceRoute/networkEndpoint`` or ``AVCustomDeviceRoute/bluetoothIdentifier`` property to establish a connection to a device. Typically, only one of the properties provides a valid value, depending on the type of device. In certain cases, both properties might provide valid values, in which case your app determines which one to use.
-- 
-- Phantom type for @AVCustomDeviceRoute@.
data AVCustomDeviceRoute

instance IsObjCObject (Id AVCustomDeviceRoute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCustomDeviceRoute"

class IsNSObject a => IsAVCustomDeviceRoute a where
  toAVCustomDeviceRoute :: a -> Id AVCustomDeviceRoute

instance IsAVCustomDeviceRoute (Id AVCustomDeviceRoute) where
  toAVCustomDeviceRoute = unsafeCastId

instance IsNSObject (Id AVCustomDeviceRoute) where
  toNSObject = unsafeCastId

-- ---------- AVCustomRoutingActionItem ----------

-- | An object that represents a custom action item to display in a device route picker.
--
-- Use this class to specify supplemental action items to display in the list of discovered routes. Tapping a custom item dismisses the picker and calls the ``AVCustomRoutingControllerDelegate/customRoutingController:didSelectItem:`` method of ``AVCustomRoutingControllerDelegate``.
-- 
-- Phantom type for @AVCustomRoutingActionItem@.
data AVCustomRoutingActionItem

instance IsObjCObject (Id AVCustomRoutingActionItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCustomRoutingActionItem"

class IsNSObject a => IsAVCustomRoutingActionItem a where
  toAVCustomRoutingActionItem :: a -> Id AVCustomRoutingActionItem

instance IsAVCustomRoutingActionItem (Id AVCustomRoutingActionItem) where
  toAVCustomRoutingActionItem = unsafeCastId

instance IsNSObject (Id AVCustomRoutingActionItem) where
  toNSObject = unsafeCastId

-- ---------- AVCustomRoutingController ----------

-- | An object that manages the connection from a device to a destination.
--
-- A routing controller also informs its ``AVCustomRoutingController/delegate`` object about which routes the user previously authorized, so it can reconnect, if appropriate.
-- 
-- Phantom type for @AVCustomRoutingController@.
data AVCustomRoutingController

instance IsObjCObject (Id AVCustomRoutingController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCustomRoutingController"

class IsNSObject a => IsAVCustomRoutingController a where
  toAVCustomRoutingController :: a -> Id AVCustomRoutingController

instance IsAVCustomRoutingController (Id AVCustomRoutingController) where
  toAVCustomRoutingController = unsafeCastId

instance IsNSObject (Id AVCustomRoutingController) where
  toNSObject = unsafeCastId

-- ---------- AVCustomRoutingEvent ----------

-- | An object that represents an event that occurs on a route.
--
-- Depending on the route’s reason, apps establish or tear down a connection to a specified route.
-- 
-- Phantom type for @AVCustomRoutingEvent@.
data AVCustomRoutingEvent

instance IsObjCObject (Id AVCustomRoutingEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCustomRoutingEvent"

class IsNSObject a => IsAVCustomRoutingEvent a where
  toAVCustomRoutingEvent :: a -> Id AVCustomRoutingEvent

instance IsAVCustomRoutingEvent (Id AVCustomRoutingEvent) where
  toAVCustomRoutingEvent = unsafeCastId

instance IsNSObject (Id AVCustomRoutingEvent) where
  toNSObject = unsafeCastId

-- ---------- AVCustomRoutingPartialIP ----------

-- | Represents a full or partial IP address.
--
-- Use this class in conjunction with ``knownRouteIPs``.
-- 
-- Phantom type for @AVCustomRoutingPartialIP@.
data AVCustomRoutingPartialIP

instance IsObjCObject (Id AVCustomRoutingPartialIP) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCustomRoutingPartialIP"

class IsNSObject a => IsAVCustomRoutingPartialIP a where
  toAVCustomRoutingPartialIP :: a -> Id AVCustomRoutingPartialIP

instance IsAVCustomRoutingPartialIP (Id AVCustomRoutingPartialIP) where
  toAVCustomRoutingPartialIP = unsafeCastId

instance IsNSObject (Id AVCustomRoutingPartialIP) where
  toNSObject = unsafeCastId

-- ---------- AVRoutingPlaybackArbiter ----------

-- | An object that manages playback routing preferences.
--
-- This object manages instances of ``AVRoutingPlaybackParticipant`` for arbitration of media playback routing priorities and preferences on restricted playback interfaces. The playback routing arbiter is responsible for collecting and applying preferences, such as priorities in non-mixable audio routes and external playback states where the number of allowed players is limited.
-- 
-- Phantom type for @AVRoutingPlaybackArbiter@.
data AVRoutingPlaybackArbiter

instance IsObjCObject (Id AVRoutingPlaybackArbiter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVRoutingPlaybackArbiter"

class IsNSObject a => IsAVRoutingPlaybackArbiter a where
  toAVRoutingPlaybackArbiter :: a -> Id AVRoutingPlaybackArbiter

instance IsAVRoutingPlaybackArbiter (Id AVRoutingPlaybackArbiter) where
  toAVRoutingPlaybackArbiter = unsafeCastId

instance IsNSObject (Id AVRoutingPlaybackArbiter) where
  toNSObject = unsafeCastId
