{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PushKit.Internal.Classes (
    module ObjC.PushKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- PKPushCredentials ----------

-- | An object that encapsulates the device token you use to deliver push notifications to your app.
--
-- When registering your app's push types, PushKit creates a ``PushKit/PKPushCredentials`` object for each type your app supports and delivers it to your delegate's ``PushKit/PKPushRegistryDelegate/pushRegistry:didUpdatePushCredentials:forType:`` method. Don't create ``PushKit/PKPushCredentials`` objects yourself.
-- 
-- Phantom type for @PKPushCredentials@.
data PKPushCredentials

instance IsObjCObject (Id PKPushCredentials) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPushCredentials"

class IsNSObject a => IsPKPushCredentials a where
  toPKPushCredentials :: a -> Id PKPushCredentials

instance IsPKPushCredentials (Id PKPushCredentials) where
  toPKPushCredentials = unsafeCastId

instance IsNSObject (Id PKPushCredentials) where
  toNSObject = unsafeCastId

-- ---------- PKPushPayload ----------

-- | An object that contains information about a received PushKit notification.
--
-- ## Topics
--
-- ### Payload Data
--
-- - ``PushKit/PKPushPayload/dictionaryPayload`` - ``PushKit/PKPushPayload/type``
-- 
-- Phantom type for @PKPushPayload@.
data PKPushPayload

instance IsObjCObject (Id PKPushPayload) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPushPayload"

class IsNSObject a => IsPKPushPayload a where
  toPKPushPayload :: a -> Id PKPushPayload

instance IsPKPushPayload (Id PKPushPayload) where
  toPKPushPayload = unsafeCastId

instance IsNSObject (Id PKPushPayload) where
  toNSObject = unsafeCastId

-- ---------- PKPushRegistry ----------

-- | An object that requests the delivery and handles the receipt of PushKit notifications.
--
-- A @PKPushRegistry@ object manages only certain types of notifications, such as high-priority notifications needed by a VoIP app. PushKit wakes up your app as needed to deliver incoming notifications and delivers the notifications directly to the push registry object that requested them.
--
-- Every time your app launches, whether in the foreground or in the background, create a push registry object and configure it. Typically, you keep the push registry object running for the duration of your app. Each push registry object delivers incoming notifications to its ``PushKit/PKPushRegistry/delegate`` object, which also handles the responses for registration requests. The listing below shows how to create a push registry object and request VoIP notifications. Always assign an appropriate delegate object before modifying the ``PushKit/PKPushRegistry/desiredPushTypes`` property.
--
-- ```objc - (void) registerForVoIPPushes {    self.voipRegistry = [[PKPushRegistry alloc] initWithQueue:nil];    self.voipRegistry.delegate = self;
--
-- // Initiate registration.    self.voipRegistry.desiredPushTypes = [NSSet setWithObject:PKPushTypeVoIP]; } ```
--
-- Assigning a new value to the ``PushKit/PKPushRegistry/desiredPushTypes`` property registers the push registry object with the PushKit servers. The server reports the success or failure of your registration attempts asynchronously to the push registry, which then reports those results to its delegate object. The push registry also delivers all received notifications to the delegate object. For more information about the delegate methods, see ``PushKit/PKPushRegistryDelegate``.
--
-- ## Topics
--
-- ### Initializing a Push Registry
--
-- - ``PushKit/PKPushRegistry/initWithQueue:``
--
-- ### Receiving the Notification Data
--
-- - ``PushKit/PKPushRegistry/delegate`` - ``PushKit/PKPushRegistryDelegate``
--
-- ### Managing the Push Registry
--
-- - ``PushKit/PKPushRegistry/desiredPushTypes`` - ``PushKit/PKPushRegistry/pushTokenForType:``
-- 
-- Phantom type for @PKPushRegistry@.
data PKPushRegistry

instance IsObjCObject (Id PKPushRegistry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPushRegistry"

class IsNSObject a => IsPKPushRegistry a where
  toPKPushRegistry :: a -> Id PKPushRegistry

instance IsPKPushRegistry (Id PKPushRegistry) where
  toPKPushRegistry = unsafeCastId

instance IsNSObject (Id PKPushRegistry) where
  toNSObject = unsafeCastId
