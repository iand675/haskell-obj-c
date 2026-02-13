{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKNotificationInfo
--
-- The payload of a push notification delivered in the UIApplication @application:didReceiveRemoteNotification:@ delegate method contains information about the firing subscription.
--
-- Use
--
-- +[CKNotification notificationFromRemoteNotificationDictionary:]
--
-- to parse that payload.  On tvOS, alerts, badges, sounds, and categories are not handled in push notifications. However, CKSubscriptions remain available to help you avoid polling the server.
--
-- Generated bindings for @CKNotificationInfo@.
module ObjC.CloudKit.CKNotificationInfo
  ( CKNotificationInfo
  , IsCKNotificationInfo(..)
  , desiredKeys
  , setDesiredKeys
  , shouldBadge
  , setShouldBadge
  , shouldSendContentAvailable
  , setShouldSendContentAvailable
  , shouldSendMutableContent
  , setShouldSendMutableContent
  , collapseIDKey
  , setCollapseIDKey
  , collapseIDKeySelector
  , desiredKeysSelector
  , setCollapseIDKeySelector
  , setDesiredKeysSelector
  , setShouldBadgeSelector
  , setShouldSendContentAvailableSelector
  , setShouldSendMutableContentSelector
  , shouldBadgeSelector
  , shouldSendContentAvailableSelector
  , shouldSendMutableContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A list of keys from the matching record to include in the notification payload.
--
-- Only some keys are allowed.  The value types associated with those keys on the server must be one of these classes:  - CKReference  - CLLocation  - NSDate  - NSNumber  - NSString
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO (Id NSArray)
desiredKeys ckNotificationInfo =
  sendMessage ckNotificationInfo desiredKeysSelector

-- | A list of keys from the matching record to include in the notification payload.
--
-- Only some keys are allowed.  The value types associated with those keys on the server must be one of these classes:  - CKReference  - CLLocation  - NSDate  - NSNumber  - NSString
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKNotificationInfo ckNotificationInfo, IsNSArray value) => ckNotificationInfo -> value -> IO ()
setDesiredKeys ckNotificationInfo value =
  sendMessage ckNotificationInfo setDesiredKeysSelector (toNSArray value)

-- | Indicates that the notification should increment the app's badge count. Default value is @NO.@
--
-- ObjC selector: @- shouldBadge@
shouldBadge :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO Bool
shouldBadge ckNotificationInfo =
  sendMessage ckNotificationInfo shouldBadgeSelector

-- | Indicates that the notification should increment the app's badge count. Default value is @NO.@
--
-- ObjC selector: @- setShouldBadge:@
setShouldBadge :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> Bool -> IO ()
setShouldBadge ckNotificationInfo value =
  sendMessage ckNotificationInfo setShouldBadgeSelector value

-- | Indicates that the notification should be sent with the "content-available" flag to allow for background downloads in the application.
--
-- Default value is @NO.@
--
-- ObjC selector: @- shouldSendContentAvailable@
shouldSendContentAvailable :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO Bool
shouldSendContentAvailable ckNotificationInfo =
  sendMessage ckNotificationInfo shouldSendContentAvailableSelector

-- | Indicates that the notification should be sent with the "content-available" flag to allow for background downloads in the application.
--
-- Default value is @NO.@
--
-- ObjC selector: @- setShouldSendContentAvailable:@
setShouldSendContentAvailable :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> Bool -> IO ()
setShouldSendContentAvailable ckNotificationInfo value =
  sendMessage ckNotificationInfo setShouldSendContentAvailableSelector value

-- | Indicates that the notification should be sent with the "mutable-content" flag to allow a Notification Service app extension to modify or replace the push payload.
--
-- Default value is @NO.@
--
-- ObjC selector: @- shouldSendMutableContent@
shouldSendMutableContent :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO Bool
shouldSendMutableContent ckNotificationInfo =
  sendMessage ckNotificationInfo shouldSendMutableContentSelector

-- | Indicates that the notification should be sent with the "mutable-content" flag to allow a Notification Service app extension to modify or replace the push payload.
--
-- Default value is @NO.@
--
-- ObjC selector: @- setShouldSendMutableContent:@
setShouldSendMutableContent :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> Bool -> IO ()
setShouldSendMutableContent ckNotificationInfo value =
  sendMessage ckNotificationInfo setShouldSendMutableContentSelector value

-- | Optional property specifying a field name to take from the matching record whose value is used as the apns-collapse-id header.
--
-- See: APNs Notification API documentation
--
-- ObjC selector: @- collapseIDKey@
collapseIDKey :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO (Id NSString)
collapseIDKey ckNotificationInfo =
  sendMessage ckNotificationInfo collapseIDKeySelector

-- | Optional property specifying a field name to take from the matching record whose value is used as the apns-collapse-id header.
--
-- See: APNs Notification API documentation
--
-- ObjC selector: @- setCollapseIDKey:@
setCollapseIDKey :: (IsCKNotificationInfo ckNotificationInfo, IsNSString value) => ckNotificationInfo -> value -> IO ()
setCollapseIDKey ckNotificationInfo value =
  sendMessage ckNotificationInfo setCollapseIDKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector '[] (Id NSArray)
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector '[Id NSArray] ()
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

-- | @Selector@ for @shouldBadge@
shouldBadgeSelector :: Selector '[] Bool
shouldBadgeSelector = mkSelector "shouldBadge"

-- | @Selector@ for @setShouldBadge:@
setShouldBadgeSelector :: Selector '[Bool] ()
setShouldBadgeSelector = mkSelector "setShouldBadge:"

-- | @Selector@ for @shouldSendContentAvailable@
shouldSendContentAvailableSelector :: Selector '[] Bool
shouldSendContentAvailableSelector = mkSelector "shouldSendContentAvailable"

-- | @Selector@ for @setShouldSendContentAvailable:@
setShouldSendContentAvailableSelector :: Selector '[Bool] ()
setShouldSendContentAvailableSelector = mkSelector "setShouldSendContentAvailable:"

-- | @Selector@ for @shouldSendMutableContent@
shouldSendMutableContentSelector :: Selector '[] Bool
shouldSendMutableContentSelector = mkSelector "shouldSendMutableContent"

-- | @Selector@ for @setShouldSendMutableContent:@
setShouldSendMutableContentSelector :: Selector '[Bool] ()
setShouldSendMutableContentSelector = mkSelector "setShouldSendMutableContent:"

-- | @Selector@ for @collapseIDKey@
collapseIDKeySelector :: Selector '[] (Id NSString)
collapseIDKeySelector = mkSelector "collapseIDKey"

-- | @Selector@ for @setCollapseIDKey:@
setCollapseIDKeySelector :: Selector '[Id NSString] ()
setCollapseIDKeySelector = mkSelector "setCollapseIDKey:"

