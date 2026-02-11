{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserNotificationCenter@.
module ObjC.Foundation.NSUserNotificationCenter
  ( NSUserNotificationCenter
  , IsNSUserNotificationCenter(..)
  , scheduleNotification
  , removeScheduledNotification
  , deliverNotification
  , removeDeliveredNotification
  , removeAllDeliveredNotifications
  , defaultUserNotificationCenter
  , scheduledNotifications
  , setScheduledNotifications
  , deliveredNotifications
  , scheduleNotificationSelector
  , removeScheduledNotificationSelector
  , deliverNotificationSelector
  , removeDeliveredNotificationSelector
  , removeAllDeliveredNotificationsSelector
  , defaultUserNotificationCenterSelector
  , scheduledNotificationsSelector
  , setScheduledNotificationsSelector
  , deliveredNotificationsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- scheduleNotification:@
scheduleNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
scheduleNotification nsUserNotificationCenter  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsUserNotificationCenter (mkSelector "scheduleNotification:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- removeScheduledNotification:@
removeScheduledNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
removeScheduledNotification nsUserNotificationCenter  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsUserNotificationCenter (mkSelector "removeScheduledNotification:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- deliverNotification:@
deliverNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
deliverNotification nsUserNotificationCenter  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsUserNotificationCenter (mkSelector "deliverNotification:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- removeDeliveredNotification:@
removeDeliveredNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
removeDeliveredNotification nsUserNotificationCenter  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsUserNotificationCenter (mkSelector "removeDeliveredNotification:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- removeAllDeliveredNotifications@
removeAllDeliveredNotifications :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO ()
removeAllDeliveredNotifications nsUserNotificationCenter  =
  sendMsg nsUserNotificationCenter (mkSelector "removeAllDeliveredNotifications") retVoid []

-- | @+ defaultUserNotificationCenter@
defaultUserNotificationCenter :: IO (Id NSUserNotificationCenter)
defaultUserNotificationCenter  =
  do
    cls' <- getRequiredClass "NSUserNotificationCenter"
    sendClassMsg cls' (mkSelector "defaultUserNotificationCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scheduledNotifications@
scheduledNotifications :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO (Id NSArray)
scheduledNotifications nsUserNotificationCenter  =
  sendMsg nsUserNotificationCenter (mkSelector "scheduledNotifications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScheduledNotifications:@
setScheduledNotifications :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSArray value) => nsUserNotificationCenter -> value -> IO ()
setScheduledNotifications nsUserNotificationCenter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserNotificationCenter (mkSelector "setScheduledNotifications:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deliveredNotifications@
deliveredNotifications :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO (Id NSArray)
deliveredNotifications nsUserNotificationCenter  =
  sendMsg nsUserNotificationCenter (mkSelector "deliveredNotifications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scheduleNotification:@
scheduleNotificationSelector :: Selector
scheduleNotificationSelector = mkSelector "scheduleNotification:"

-- | @Selector@ for @removeScheduledNotification:@
removeScheduledNotificationSelector :: Selector
removeScheduledNotificationSelector = mkSelector "removeScheduledNotification:"

-- | @Selector@ for @deliverNotification:@
deliverNotificationSelector :: Selector
deliverNotificationSelector = mkSelector "deliverNotification:"

-- | @Selector@ for @removeDeliveredNotification:@
removeDeliveredNotificationSelector :: Selector
removeDeliveredNotificationSelector = mkSelector "removeDeliveredNotification:"

-- | @Selector@ for @removeAllDeliveredNotifications@
removeAllDeliveredNotificationsSelector :: Selector
removeAllDeliveredNotificationsSelector = mkSelector "removeAllDeliveredNotifications"

-- | @Selector@ for @defaultUserNotificationCenter@
defaultUserNotificationCenterSelector :: Selector
defaultUserNotificationCenterSelector = mkSelector "defaultUserNotificationCenter"

-- | @Selector@ for @scheduledNotifications@
scheduledNotificationsSelector :: Selector
scheduledNotificationsSelector = mkSelector "scheduledNotifications"

-- | @Selector@ for @setScheduledNotifications:@
setScheduledNotificationsSelector :: Selector
setScheduledNotificationsSelector = mkSelector "setScheduledNotifications:"

-- | @Selector@ for @deliveredNotifications@
deliveredNotificationsSelector :: Selector
deliveredNotificationsSelector = mkSelector "deliveredNotifications"

