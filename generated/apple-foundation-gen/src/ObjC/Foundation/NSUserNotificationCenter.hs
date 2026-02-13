{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , scheduledNotifications
  , setScheduledNotifications
  , deliveredNotifications
  , defaultUserNotificationCenterSelector
  , delegateSelector
  , deliverNotificationSelector
  , deliveredNotificationsSelector
  , removeAllDeliveredNotificationsSelector
  , removeDeliveredNotificationSelector
  , removeScheduledNotificationSelector
  , scheduleNotificationSelector
  , scheduledNotificationsSelector
  , setDelegateSelector
  , setScheduledNotificationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- scheduleNotification:@
scheduleNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
scheduleNotification nsUserNotificationCenter notification =
  sendMessage nsUserNotificationCenter scheduleNotificationSelector (toNSUserNotification notification)

-- | @- removeScheduledNotification:@
removeScheduledNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
removeScheduledNotification nsUserNotificationCenter notification =
  sendMessage nsUserNotificationCenter removeScheduledNotificationSelector (toNSUserNotification notification)

-- | @- deliverNotification:@
deliverNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
deliverNotification nsUserNotificationCenter notification =
  sendMessage nsUserNotificationCenter deliverNotificationSelector (toNSUserNotification notification)

-- | @- removeDeliveredNotification:@
removeDeliveredNotification :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSUserNotification notification) => nsUserNotificationCenter -> notification -> IO ()
removeDeliveredNotification nsUserNotificationCenter notification =
  sendMessage nsUserNotificationCenter removeDeliveredNotificationSelector (toNSUserNotification notification)

-- | @- removeAllDeliveredNotifications@
removeAllDeliveredNotifications :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO ()
removeAllDeliveredNotifications nsUserNotificationCenter =
  sendMessage nsUserNotificationCenter removeAllDeliveredNotificationsSelector

-- | @+ defaultUserNotificationCenter@
defaultUserNotificationCenter :: IO (Id NSUserNotificationCenter)
defaultUserNotificationCenter  =
  do
    cls' <- getRequiredClass "NSUserNotificationCenter"
    sendClassMessage cls' defaultUserNotificationCenterSelector

-- | @- delegate@
delegate :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO RawId
delegate nsUserNotificationCenter =
  sendMessage nsUserNotificationCenter delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> RawId -> IO ()
setDelegate nsUserNotificationCenter value =
  sendMessage nsUserNotificationCenter setDelegateSelector value

-- | @- scheduledNotifications@
scheduledNotifications :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO (Id NSArray)
scheduledNotifications nsUserNotificationCenter =
  sendMessage nsUserNotificationCenter scheduledNotificationsSelector

-- | @- setScheduledNotifications:@
setScheduledNotifications :: (IsNSUserNotificationCenter nsUserNotificationCenter, IsNSArray value) => nsUserNotificationCenter -> value -> IO ()
setScheduledNotifications nsUserNotificationCenter value =
  sendMessage nsUserNotificationCenter setScheduledNotificationsSelector (toNSArray value)

-- | @- deliveredNotifications@
deliveredNotifications :: IsNSUserNotificationCenter nsUserNotificationCenter => nsUserNotificationCenter -> IO (Id NSArray)
deliveredNotifications nsUserNotificationCenter =
  sendMessage nsUserNotificationCenter deliveredNotificationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scheduleNotification:@
scheduleNotificationSelector :: Selector '[Id NSUserNotification] ()
scheduleNotificationSelector = mkSelector "scheduleNotification:"

-- | @Selector@ for @removeScheduledNotification:@
removeScheduledNotificationSelector :: Selector '[Id NSUserNotification] ()
removeScheduledNotificationSelector = mkSelector "removeScheduledNotification:"

-- | @Selector@ for @deliverNotification:@
deliverNotificationSelector :: Selector '[Id NSUserNotification] ()
deliverNotificationSelector = mkSelector "deliverNotification:"

-- | @Selector@ for @removeDeliveredNotification:@
removeDeliveredNotificationSelector :: Selector '[Id NSUserNotification] ()
removeDeliveredNotificationSelector = mkSelector "removeDeliveredNotification:"

-- | @Selector@ for @removeAllDeliveredNotifications@
removeAllDeliveredNotificationsSelector :: Selector '[] ()
removeAllDeliveredNotificationsSelector = mkSelector "removeAllDeliveredNotifications"

-- | @Selector@ for @defaultUserNotificationCenter@
defaultUserNotificationCenterSelector :: Selector '[] (Id NSUserNotificationCenter)
defaultUserNotificationCenterSelector = mkSelector "defaultUserNotificationCenter"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @scheduledNotifications@
scheduledNotificationsSelector :: Selector '[] (Id NSArray)
scheduledNotificationsSelector = mkSelector "scheduledNotifications"

-- | @Selector@ for @setScheduledNotifications:@
setScheduledNotificationsSelector :: Selector '[Id NSArray] ()
setScheduledNotificationsSelector = mkSelector "setScheduledNotifications:"

-- | @Selector@ for @deliveredNotifications@
deliveredNotificationsSelector :: Selector '[] (Id NSArray)
deliveredNotificationsSelector = mkSelector "deliveredNotifications"

