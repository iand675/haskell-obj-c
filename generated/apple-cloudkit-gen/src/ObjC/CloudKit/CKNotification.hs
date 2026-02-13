{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Pushes from CloudKit servers contain both CloudKit-specific and APS-specific information. APS-specific information includes elements like alerts, badges, sounds, categories, etc. When receiving a push from CloudKit servers, the push may be delivered via multiple API flows. The flow(s) chosen will depend on the type of push requested (e.g. via the ``CKSubscription`` that triggered it and its configured @notificationInfo@).
--
-- Pushes with UI elements (alerts, badges, sounds): These pushes are delivered via the @UserNotifications@ framework, in the form of a @UNNotification@ Applications should use the @UserNotifications@ framework to interact with the UI elements of this push. Applications may create a ``CKNotification`` from a @UNNotification@ in their @UNUserNotificationCenterDelegate@:
--
-- func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification) async -> UNNotificationPresentationOptions {         let ckNotification = CKNotification(fromRemoteNotificationDictionary: notification.request.content.userInfo)     }
--
-- Pushes with @content-available@: These pushes are delivered via an application delegate, in the form of a remote notification. For example: @UIApplicationDelegate.application(_:didReceiveRemoteNotification:) async@ Applications do not need to interact with any UI element in the push payload argument, that's intended to be handled via the @UserNotifications@ flow (a push with both UI elements and @content-available@ will be delivered via both API flows) Applications may create a ``CKNotification`` from the remote notification in their @UIApplicationDelegate@:
--
-- func application(_ application: UIApplication, didReceiveRemoteNotification userInfo: [AnyHashable : Any]) async -> UIBackgroundFetchResult {         let ckNotification = CKNotification(fromRemoteNotificationDictionary: userInfo)     }
--
-- Generated bindings for @CKNotification@.
module ObjC.CloudKit.CKNotification
  ( CKNotification
  , IsCKNotification(..)
  , init_
  , new
  , notificationFromRemoteNotificationDictionary
  , notificationType
  , notificationID
  , containerIdentifier
  , subscriptionOwnerUserRecordID
  , isPruned
  , subscriptionID
  , badge
  , badgeSelector
  , containerIdentifierSelector
  , initSelector
  , isPrunedSelector
  , newSelector
  , notificationFromRemoteNotificationDictionarySelector
  , notificationIDSelector
  , notificationTypeSelector
  , subscriptionIDSelector
  , subscriptionOwnerUserRecordIDSelector

  -- * Enum types
  , CKNotificationType(CKNotificationType)
  , pattern CKNotificationTypeQuery
  , pattern CKNotificationTypeRecordZone
  , pattern CKNotificationTypeReadNotification
  , pattern CKNotificationTypeDatabase

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKNotification ckNotification => ckNotification -> IO (Id CKNotification)
init_ ckNotification =
  sendOwnedMessage ckNotification initSelector

-- | @+ new@
new :: IO (Id CKNotification)
new  =
  do
    cls' <- getRequiredClass "CKNotification"
    sendOwnedClassMessage cls' newSelector

-- | @+ notificationFromRemoteNotificationDictionary:@
notificationFromRemoteNotificationDictionary :: IsNSDictionary notificationDictionary => notificationDictionary -> IO (Id CKNotification)
notificationFromRemoteNotificationDictionary notificationDictionary =
  do
    cls' <- getRequiredClass "CKNotification"
    sendClassMessage cls' notificationFromRemoteNotificationDictionarySelector (toNSDictionary notificationDictionary)

-- | When you instantiate a ``CKNotification`` from a remote notification dictionary, you will get back a concrete subclass defined below.  Use @notificationType@ to avoid @as?@ or @-isKindOfClass:@ checks.
--
-- ObjC selector: @- notificationType@
notificationType :: IsCKNotification ckNotification => ckNotification -> IO CKNotificationType
notificationType ckNotification =
  sendMessage ckNotification notificationTypeSelector

-- | @- notificationID@
notificationID :: IsCKNotification ckNotification => ckNotification -> IO (Id CKNotificationID)
notificationID ckNotification =
  sendMessage ckNotification notificationIDSelector

-- | @- containerIdentifier@
containerIdentifier :: IsCKNotification ckNotification => ckNotification -> IO (Id NSString)
containerIdentifier ckNotification =
  sendMessage ckNotification containerIdentifierSelector

-- | The user @recordID@ of the owner of the subscription for which this notification was generated
--
-- ObjC selector: @- subscriptionOwnerUserRecordID@
subscriptionOwnerUserRecordID :: IsCKNotification ckNotification => ckNotification -> IO (Id CKRecordID)
subscriptionOwnerUserRecordID ckNotification =
  sendMessage ckNotification subscriptionOwnerUserRecordIDSelector

-- | Whether or not the notification fully represents what the server wanted to send.
--
-- Push notifications have a limited size.  In some cases, CloudKit servers may not be able to send you a full ``CKNotification``'s worth of info in one push. In those cases, @isPruned@ returns @true@. The order in which properties are dropped from a push notification is defined in each ``CKNotification`` subclass below.
--
-- ObjC selector: @- isPruned@
isPruned :: IsCKNotification ckNotification => ckNotification -> IO Bool
isPruned ckNotification =
  sendMessage ckNotification isPrunedSelector

-- | The ID of the subscription that caused this notification to fire.
--
-- ObjC selector: @- subscriptionID@
subscriptionID :: IsCKNotification ckNotification => ckNotification -> IO (Id NSString)
subscriptionID ckNotification =
  sendMessage ckNotification subscriptionIDSelector

-- | @- badge@
badge :: IsCKNotification ckNotification => ckNotification -> IO (Id NSNumber)
badge ckNotification =
  sendMessage ckNotification badgeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKNotification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKNotification)
newSelector = mkSelector "new"

-- | @Selector@ for @notificationFromRemoteNotificationDictionary:@
notificationFromRemoteNotificationDictionarySelector :: Selector '[Id NSDictionary] (Id CKNotification)
notificationFromRemoteNotificationDictionarySelector = mkSelector "notificationFromRemoteNotificationDictionary:"

-- | @Selector@ for @notificationType@
notificationTypeSelector :: Selector '[] CKNotificationType
notificationTypeSelector = mkSelector "notificationType"

-- | @Selector@ for @notificationID@
notificationIDSelector :: Selector '[] (Id CKNotificationID)
notificationIDSelector = mkSelector "notificationID"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @subscriptionOwnerUserRecordID@
subscriptionOwnerUserRecordIDSelector :: Selector '[] (Id CKRecordID)
subscriptionOwnerUserRecordIDSelector = mkSelector "subscriptionOwnerUserRecordID"

-- | @Selector@ for @isPruned@
isPrunedSelector :: Selector '[] Bool
isPrunedSelector = mkSelector "isPruned"

-- | @Selector@ for @subscriptionID@
subscriptionIDSelector :: Selector '[] (Id NSString)
subscriptionIDSelector = mkSelector "subscriptionID"

-- | @Selector@ for @badge@
badgeSelector :: Selector '[] (Id NSNumber)
badgeSelector = mkSelector "badge"

