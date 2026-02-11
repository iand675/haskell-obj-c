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
  , desiredKeysSelector
  , setDesiredKeysSelector
  , shouldBadgeSelector
  , setShouldBadgeSelector
  , shouldSendContentAvailableSelector
  , setShouldSendContentAvailableSelector
  , shouldSendMutableContentSelector
  , setShouldSendMutableContentSelector
  , collapseIDKeySelector
  , setCollapseIDKeySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
desiredKeys ckNotificationInfo  =
  sendMsg ckNotificationInfo (mkSelector "desiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of keys from the matching record to include in the notification payload.
--
-- Only some keys are allowed.  The value types associated with those keys on the server must be one of these classes:  - CKReference  - CLLocation  - NSDate  - NSNumber  - NSString
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKNotificationInfo ckNotificationInfo, IsNSArray value) => ckNotificationInfo -> value -> IO ()
setDesiredKeys ckNotificationInfo  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckNotificationInfo (mkSelector "setDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates that the notification should increment the app's badge count. Default value is @NO.@
--
-- ObjC selector: @- shouldBadge@
shouldBadge :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO Bool
shouldBadge ckNotificationInfo  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckNotificationInfo (mkSelector "shouldBadge") retCULong []

-- | Indicates that the notification should increment the app's badge count. Default value is @NO.@
--
-- ObjC selector: @- setShouldBadge:@
setShouldBadge :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> Bool -> IO ()
setShouldBadge ckNotificationInfo  value =
  sendMsg ckNotificationInfo (mkSelector "setShouldBadge:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates that the notification should be sent with the "content-available" flag to allow for background downloads in the application.
--
-- Default value is @NO.@
--
-- ObjC selector: @- shouldSendContentAvailable@
shouldSendContentAvailable :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO Bool
shouldSendContentAvailable ckNotificationInfo  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckNotificationInfo (mkSelector "shouldSendContentAvailable") retCULong []

-- | Indicates that the notification should be sent with the "content-available" flag to allow for background downloads in the application.
--
-- Default value is @NO.@
--
-- ObjC selector: @- setShouldSendContentAvailable:@
setShouldSendContentAvailable :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> Bool -> IO ()
setShouldSendContentAvailable ckNotificationInfo  value =
  sendMsg ckNotificationInfo (mkSelector "setShouldSendContentAvailable:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates that the notification should be sent with the "mutable-content" flag to allow a Notification Service app extension to modify or replace the push payload.
--
-- Default value is @NO.@
--
-- ObjC selector: @- shouldSendMutableContent@
shouldSendMutableContent :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO Bool
shouldSendMutableContent ckNotificationInfo  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckNotificationInfo (mkSelector "shouldSendMutableContent") retCULong []

-- | Indicates that the notification should be sent with the "mutable-content" flag to allow a Notification Service app extension to modify or replace the push payload.
--
-- Default value is @NO.@
--
-- ObjC selector: @- setShouldSendMutableContent:@
setShouldSendMutableContent :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> Bool -> IO ()
setShouldSendMutableContent ckNotificationInfo  value =
  sendMsg ckNotificationInfo (mkSelector "setShouldSendMutableContent:") retVoid [argCULong (if value then 1 else 0)]

-- | Optional property specifying a field name to take from the matching record whose value is used as the apns-collapse-id header.
--
-- See: APNs Notification API documentation
--
-- ObjC selector: @- collapseIDKey@
collapseIDKey :: IsCKNotificationInfo ckNotificationInfo => ckNotificationInfo -> IO (Id NSString)
collapseIDKey ckNotificationInfo  =
  sendMsg ckNotificationInfo (mkSelector "collapseIDKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional property specifying a field name to take from the matching record whose value is used as the apns-collapse-id header.
--
-- See: APNs Notification API documentation
--
-- ObjC selector: @- setCollapseIDKey:@
setCollapseIDKey :: (IsCKNotificationInfo ckNotificationInfo, IsNSString value) => ckNotificationInfo -> value -> IO ()
setCollapseIDKey ckNotificationInfo  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckNotificationInfo (mkSelector "setCollapseIDKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

-- | @Selector@ for @shouldBadge@
shouldBadgeSelector :: Selector
shouldBadgeSelector = mkSelector "shouldBadge"

-- | @Selector@ for @setShouldBadge:@
setShouldBadgeSelector :: Selector
setShouldBadgeSelector = mkSelector "setShouldBadge:"

-- | @Selector@ for @shouldSendContentAvailable@
shouldSendContentAvailableSelector :: Selector
shouldSendContentAvailableSelector = mkSelector "shouldSendContentAvailable"

-- | @Selector@ for @setShouldSendContentAvailable:@
setShouldSendContentAvailableSelector :: Selector
setShouldSendContentAvailableSelector = mkSelector "setShouldSendContentAvailable:"

-- | @Selector@ for @shouldSendMutableContent@
shouldSendMutableContentSelector :: Selector
shouldSendMutableContentSelector = mkSelector "shouldSendMutableContent"

-- | @Selector@ for @setShouldSendMutableContent:@
setShouldSendMutableContentSelector :: Selector
setShouldSendMutableContentSelector = mkSelector "setShouldSendMutableContent:"

-- | @Selector@ for @collapseIDKey@
collapseIDKeySelector :: Selector
collapseIDKeySelector = mkSelector "collapseIDKey"

-- | @Selector@ for @setCollapseIDKey:@
setCollapseIDKeySelector :: Selector
setCollapseIDKeySelector = mkSelector "setCollapseIDKey:"

