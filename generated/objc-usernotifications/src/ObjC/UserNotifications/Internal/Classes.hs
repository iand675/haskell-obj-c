{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.UserNotifications.Internal.Classes (
    module ObjC.UserNotifications.Internal.Classes,
    module ObjC.CoreLocation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.Intents.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- ---------- UNNotification ----------

-- | Phantom type for @UNNotification@.
data UNNotification

instance IsObjCObject (Id UNNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotification"

class IsNSObject a => IsUNNotification a where
  toUNNotification :: a -> Id UNNotification

instance IsUNNotification (Id UNNotification) where
  toUNNotification = unsafeCastId

instance IsNSObject (Id UNNotification) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationAction ----------

-- | Phantom type for @UNNotificationAction@.
data UNNotificationAction

instance IsObjCObject (Id UNNotificationAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationAction"

class IsNSObject a => IsUNNotificationAction a where
  toUNNotificationAction :: a -> Id UNNotificationAction

instance IsUNNotificationAction (Id UNNotificationAction) where
  toUNNotificationAction = unsafeCastId

instance IsNSObject (Id UNNotificationAction) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationActionIcon ----------

-- | Phantom type for @UNNotificationActionIcon@.
data UNNotificationActionIcon

instance IsObjCObject (Id UNNotificationActionIcon) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationActionIcon"

class IsNSObject a => IsUNNotificationActionIcon a where
  toUNNotificationActionIcon :: a -> Id UNNotificationActionIcon

instance IsUNNotificationActionIcon (Id UNNotificationActionIcon) where
  toUNNotificationActionIcon = unsafeCastId

instance IsNSObject (Id UNNotificationActionIcon) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationAttachment ----------

-- | Phantom type for @UNNotificationAttachment@.
data UNNotificationAttachment

instance IsObjCObject (Id UNNotificationAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationAttachment"

class IsNSObject a => IsUNNotificationAttachment a where
  toUNNotificationAttachment :: a -> Id UNNotificationAttachment

instance IsUNNotificationAttachment (Id UNNotificationAttachment) where
  toUNNotificationAttachment = unsafeCastId

instance IsNSObject (Id UNNotificationAttachment) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationAttributedMessageContext ----------

-- | Phantom type for @UNNotificationAttributedMessageContext@.
data UNNotificationAttributedMessageContext

instance IsObjCObject (Id UNNotificationAttributedMessageContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationAttributedMessageContext"

class IsNSObject a => IsUNNotificationAttributedMessageContext a where
  toUNNotificationAttributedMessageContext :: a -> Id UNNotificationAttributedMessageContext

instance IsUNNotificationAttributedMessageContext (Id UNNotificationAttributedMessageContext) where
  toUNNotificationAttributedMessageContext = unsafeCastId

instance IsNSObject (Id UNNotificationAttributedMessageContext) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationCategory ----------

-- | Phantom type for @UNNotificationCategory@.
data UNNotificationCategory

instance IsObjCObject (Id UNNotificationCategory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationCategory"

class IsNSObject a => IsUNNotificationCategory a where
  toUNNotificationCategory :: a -> Id UNNotificationCategory

instance IsUNNotificationCategory (Id UNNotificationCategory) where
  toUNNotificationCategory = unsafeCastId

instance IsNSObject (Id UNNotificationCategory) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationRequest ----------

-- | Phantom type for @UNNotificationRequest@.
data UNNotificationRequest

instance IsObjCObject (Id UNNotificationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationRequest"

class IsNSObject a => IsUNNotificationRequest a where
  toUNNotificationRequest :: a -> Id UNNotificationRequest

instance IsUNNotificationRequest (Id UNNotificationRequest) where
  toUNNotificationRequest = unsafeCastId

instance IsNSObject (Id UNNotificationRequest) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationResponse ----------

-- | Phantom type for @UNNotificationResponse@.
data UNNotificationResponse

instance IsObjCObject (Id UNNotificationResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationResponse"

class IsNSObject a => IsUNNotificationResponse a where
  toUNNotificationResponse :: a -> Id UNNotificationResponse

instance IsUNNotificationResponse (Id UNNotificationResponse) where
  toUNNotificationResponse = unsafeCastId

instance IsNSObject (Id UNNotificationResponse) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationServiceExtension ----------

-- | Phantom type for @UNNotificationServiceExtension@.
data UNNotificationServiceExtension

instance IsObjCObject (Id UNNotificationServiceExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationServiceExtension"

class IsNSObject a => IsUNNotificationServiceExtension a where
  toUNNotificationServiceExtension :: a -> Id UNNotificationServiceExtension

instance IsUNNotificationServiceExtension (Id UNNotificationServiceExtension) where
  toUNNotificationServiceExtension = unsafeCastId

instance IsNSObject (Id UNNotificationServiceExtension) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationSettings ----------

-- | Phantom type for @UNNotificationSettings@.
data UNNotificationSettings

instance IsObjCObject (Id UNNotificationSettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationSettings"

class IsNSObject a => IsUNNotificationSettings a where
  toUNNotificationSettings :: a -> Id UNNotificationSettings

instance IsUNNotificationSettings (Id UNNotificationSettings) where
  toUNNotificationSettings = unsafeCastId

instance IsNSObject (Id UNNotificationSettings) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationSound ----------

-- | Phantom type for @UNNotificationSound@.
data UNNotificationSound

instance IsObjCObject (Id UNNotificationSound) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationSound"

class IsNSObject a => IsUNNotificationSound a where
  toUNNotificationSound :: a -> Id UNNotificationSound

instance IsUNNotificationSound (Id UNNotificationSound) where
  toUNNotificationSound = unsafeCastId

instance IsNSObject (Id UNNotificationSound) where
  toNSObject = unsafeCastId

-- ---------- UNNotificationTrigger ----------

-- | Phantom type for @UNNotificationTrigger@.
data UNNotificationTrigger

instance IsObjCObject (Id UNNotificationTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNNotificationTrigger"

class IsNSObject a => IsUNNotificationTrigger a where
  toUNNotificationTrigger :: a -> Id UNNotificationTrigger

instance IsUNNotificationTrigger (Id UNNotificationTrigger) where
  toUNNotificationTrigger = unsafeCastId

instance IsNSObject (Id UNNotificationTrigger) where
  toNSObject = unsafeCastId

-- ---------- UNUserNotificationCenter ----------

-- | Phantom type for @UNUserNotificationCenter@.
data UNUserNotificationCenter

instance IsObjCObject (Id UNUserNotificationCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNUserNotificationCenter"

class IsNSObject a => IsUNUserNotificationCenter a where
  toUNUserNotificationCenter :: a -> Id UNUserNotificationCenter

instance IsUNUserNotificationCenter (Id UNUserNotificationCenter) where
  toUNUserNotificationCenter = unsafeCastId

instance IsNSObject (Id UNUserNotificationCenter) where
  toNSObject = unsafeCastId

-- ---------- UNTextInputNotificationAction ----------

-- | Phantom type for @UNTextInputNotificationAction@.
data UNTextInputNotificationAction

instance IsObjCObject (Id UNTextInputNotificationAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNTextInputNotificationAction"

class IsUNNotificationAction a => IsUNTextInputNotificationAction a where
  toUNTextInputNotificationAction :: a -> Id UNTextInputNotificationAction

instance IsUNTextInputNotificationAction (Id UNTextInputNotificationAction) where
  toUNTextInputNotificationAction = unsafeCastId

instance IsNSObject (Id UNTextInputNotificationAction) where
  toNSObject = unsafeCastId

instance IsUNNotificationAction (Id UNTextInputNotificationAction) where
  toUNNotificationAction = unsafeCastId

-- ---------- UNTextInputNotificationResponse ----------

-- | Phantom type for @UNTextInputNotificationResponse@.
data UNTextInputNotificationResponse

instance IsObjCObject (Id UNTextInputNotificationResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNTextInputNotificationResponse"

class IsUNNotificationResponse a => IsUNTextInputNotificationResponse a where
  toUNTextInputNotificationResponse :: a -> Id UNTextInputNotificationResponse

instance IsUNTextInputNotificationResponse (Id UNTextInputNotificationResponse) where
  toUNTextInputNotificationResponse = unsafeCastId

instance IsNSObject (Id UNTextInputNotificationResponse) where
  toNSObject = unsafeCastId

instance IsUNNotificationResponse (Id UNTextInputNotificationResponse) where
  toUNNotificationResponse = unsafeCastId

-- ---------- UNCalendarNotificationTrigger ----------

-- | Phantom type for @UNCalendarNotificationTrigger@.
data UNCalendarNotificationTrigger

instance IsObjCObject (Id UNCalendarNotificationTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNCalendarNotificationTrigger"

class IsUNNotificationTrigger a => IsUNCalendarNotificationTrigger a where
  toUNCalendarNotificationTrigger :: a -> Id UNCalendarNotificationTrigger

instance IsUNCalendarNotificationTrigger (Id UNCalendarNotificationTrigger) where
  toUNCalendarNotificationTrigger = unsafeCastId

instance IsNSObject (Id UNCalendarNotificationTrigger) where
  toNSObject = unsafeCastId

instance IsUNNotificationTrigger (Id UNCalendarNotificationTrigger) where
  toUNNotificationTrigger = unsafeCastId

-- ---------- UNLocationNotificationTrigger ----------

-- | Phantom type for @UNLocationNotificationTrigger@.
data UNLocationNotificationTrigger

instance IsObjCObject (Id UNLocationNotificationTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNLocationNotificationTrigger"

class IsUNNotificationTrigger a => IsUNLocationNotificationTrigger a where
  toUNLocationNotificationTrigger :: a -> Id UNLocationNotificationTrigger

instance IsUNLocationNotificationTrigger (Id UNLocationNotificationTrigger) where
  toUNLocationNotificationTrigger = unsafeCastId

instance IsNSObject (Id UNLocationNotificationTrigger) where
  toNSObject = unsafeCastId

instance IsUNNotificationTrigger (Id UNLocationNotificationTrigger) where
  toUNNotificationTrigger = unsafeCastId

-- ---------- UNPushNotificationTrigger ----------

-- | Phantom type for @UNPushNotificationTrigger@.
data UNPushNotificationTrigger

instance IsObjCObject (Id UNPushNotificationTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNPushNotificationTrigger"

class IsUNNotificationTrigger a => IsUNPushNotificationTrigger a where
  toUNPushNotificationTrigger :: a -> Id UNPushNotificationTrigger

instance IsUNPushNotificationTrigger (Id UNPushNotificationTrigger) where
  toUNPushNotificationTrigger = unsafeCastId

instance IsNSObject (Id UNPushNotificationTrigger) where
  toNSObject = unsafeCastId

instance IsUNNotificationTrigger (Id UNPushNotificationTrigger) where
  toUNNotificationTrigger = unsafeCastId

-- ---------- UNTimeIntervalNotificationTrigger ----------

-- | Phantom type for @UNTimeIntervalNotificationTrigger@.
data UNTimeIntervalNotificationTrigger

instance IsObjCObject (Id UNTimeIntervalNotificationTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UNTimeIntervalNotificationTrigger"

class IsUNNotificationTrigger a => IsUNTimeIntervalNotificationTrigger a where
  toUNTimeIntervalNotificationTrigger :: a -> Id UNTimeIntervalNotificationTrigger

instance IsUNTimeIntervalNotificationTrigger (Id UNTimeIntervalNotificationTrigger) where
  toUNTimeIntervalNotificationTrigger = unsafeCastId

instance IsNSObject (Id UNTimeIntervalNotificationTrigger) where
  toNSObject = unsafeCastId

instance IsUNNotificationTrigger (Id UNTimeIntervalNotificationTrigger) where
  toUNNotificationTrigger = unsafeCastId
