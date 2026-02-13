{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNUserNotificationCenter@.
module ObjC.UserNotifications.UNUserNotificationCenter
  ( UNUserNotificationCenter
  , IsUNUserNotificationCenter(..)
  , currentNotificationCenter
  , init_
  , requestAuthorizationWithOptions_completionHandler
  , setNotificationCategories
  , getNotificationSettingsWithCompletionHandler
  , addNotificationRequest_withCompletionHandler
  , removePendingNotificationRequestsWithIdentifiers
  , removeAllPendingNotificationRequests
  , removeDeliveredNotificationsWithIdentifiers
  , removeAllDeliveredNotifications
  , setBadgeCount_withCompletionHandler
  , delegate
  , setDelegate
  , supportsContentExtensions
  , addNotificationRequest_withCompletionHandlerSelector
  , currentNotificationCenterSelector
  , delegateSelector
  , getNotificationSettingsWithCompletionHandlerSelector
  , initSelector
  , removeAllDeliveredNotificationsSelector
  , removeAllPendingNotificationRequestsSelector
  , removeDeliveredNotificationsWithIdentifiersSelector
  , removePendingNotificationRequestsWithIdentifiersSelector
  , requestAuthorizationWithOptions_completionHandlerSelector
  , setBadgeCount_withCompletionHandlerSelector
  , setDelegateSelector
  , setNotificationCategoriesSelector
  , supportsContentExtensionsSelector

  -- * Enum types
  , UNAuthorizationOptions(UNAuthorizationOptions)
  , pattern UNAuthorizationOptionBadge
  , pattern UNAuthorizationOptionSound
  , pattern UNAuthorizationOptionAlert
  , pattern UNAuthorizationOptionCarPlay
  , pattern UNAuthorizationOptionCriticalAlert
  , pattern UNAuthorizationOptionProvidesAppNotificationSettings
  , pattern UNAuthorizationOptionProvisional
  , pattern UNAuthorizationOptionAnnouncement
  , pattern UNAuthorizationOptionTimeSensitive

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ currentNotificationCenter@
currentNotificationCenter :: IO (Id UNUserNotificationCenter)
currentNotificationCenter  =
  do
    cls' <- getRequiredClass "UNUserNotificationCenter"
    sendClassMessage cls' currentNotificationCenterSelector

-- | @- init@
init_ :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO (Id UNUserNotificationCenter)
init_ unUserNotificationCenter =
  sendOwnedMessage unUserNotificationCenter initSelector

-- | @- requestAuthorizationWithOptions:completionHandler:@
requestAuthorizationWithOptions_completionHandler :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> UNAuthorizationOptions -> Ptr () -> IO ()
requestAuthorizationWithOptions_completionHandler unUserNotificationCenter options completionHandler =
  sendMessage unUserNotificationCenter requestAuthorizationWithOptions_completionHandlerSelector options completionHandler

-- | @- setNotificationCategories:@
setNotificationCategories :: (IsUNUserNotificationCenter unUserNotificationCenter, IsNSSet categories) => unUserNotificationCenter -> categories -> IO ()
setNotificationCategories unUserNotificationCenter categories =
  sendMessage unUserNotificationCenter setNotificationCategoriesSelector (toNSSet categories)

-- | @- getNotificationSettingsWithCompletionHandler:@
getNotificationSettingsWithCompletionHandler :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> Ptr () -> IO ()
getNotificationSettingsWithCompletionHandler unUserNotificationCenter completionHandler =
  sendMessage unUserNotificationCenter getNotificationSettingsWithCompletionHandlerSelector completionHandler

-- | @- addNotificationRequest:withCompletionHandler:@
addNotificationRequest_withCompletionHandler :: (IsUNUserNotificationCenter unUserNotificationCenter, IsUNNotificationRequest request) => unUserNotificationCenter -> request -> Ptr () -> IO ()
addNotificationRequest_withCompletionHandler unUserNotificationCenter request completionHandler =
  sendMessage unUserNotificationCenter addNotificationRequest_withCompletionHandlerSelector (toUNNotificationRequest request) completionHandler

-- | @- removePendingNotificationRequestsWithIdentifiers:@
removePendingNotificationRequestsWithIdentifiers :: (IsUNUserNotificationCenter unUserNotificationCenter, IsNSArray identifiers) => unUserNotificationCenter -> identifiers -> IO ()
removePendingNotificationRequestsWithIdentifiers unUserNotificationCenter identifiers =
  sendMessage unUserNotificationCenter removePendingNotificationRequestsWithIdentifiersSelector (toNSArray identifiers)

-- | @- removeAllPendingNotificationRequests@
removeAllPendingNotificationRequests :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO ()
removeAllPendingNotificationRequests unUserNotificationCenter =
  sendMessage unUserNotificationCenter removeAllPendingNotificationRequestsSelector

-- | @- removeDeliveredNotificationsWithIdentifiers:@
removeDeliveredNotificationsWithIdentifiers :: (IsUNUserNotificationCenter unUserNotificationCenter, IsNSArray identifiers) => unUserNotificationCenter -> identifiers -> IO ()
removeDeliveredNotificationsWithIdentifiers unUserNotificationCenter identifiers =
  sendMessage unUserNotificationCenter removeDeliveredNotificationsWithIdentifiersSelector (toNSArray identifiers)

-- | @- removeAllDeliveredNotifications@
removeAllDeliveredNotifications :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO ()
removeAllDeliveredNotifications unUserNotificationCenter =
  sendMessage unUserNotificationCenter removeAllDeliveredNotificationsSelector

-- | @- setBadgeCount:withCompletionHandler:@
setBadgeCount_withCompletionHandler :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> CLong -> Ptr () -> IO ()
setBadgeCount_withCompletionHandler unUserNotificationCenter newBadgeCount completionHandler =
  sendMessage unUserNotificationCenter setBadgeCount_withCompletionHandlerSelector newBadgeCount completionHandler

-- | @- delegate@
delegate :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO RawId
delegate unUserNotificationCenter =
  sendMessage unUserNotificationCenter delegateSelector

-- | @- setDelegate:@
setDelegate :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> RawId -> IO ()
setDelegate unUserNotificationCenter value =
  sendMessage unUserNotificationCenter setDelegateSelector value

-- | @- supportsContentExtensions@
supportsContentExtensions :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO Bool
supportsContentExtensions unUserNotificationCenter =
  sendMessage unUserNotificationCenter supportsContentExtensionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentNotificationCenter@
currentNotificationCenterSelector :: Selector '[] (Id UNUserNotificationCenter)
currentNotificationCenterSelector = mkSelector "currentNotificationCenter"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNUserNotificationCenter)
initSelector = mkSelector "init"

-- | @Selector@ for @requestAuthorizationWithOptions:completionHandler:@
requestAuthorizationWithOptions_completionHandlerSelector :: Selector '[UNAuthorizationOptions, Ptr ()] ()
requestAuthorizationWithOptions_completionHandlerSelector = mkSelector "requestAuthorizationWithOptions:completionHandler:"

-- | @Selector@ for @setNotificationCategories:@
setNotificationCategoriesSelector :: Selector '[Id NSSet] ()
setNotificationCategoriesSelector = mkSelector "setNotificationCategories:"

-- | @Selector@ for @getNotificationSettingsWithCompletionHandler:@
getNotificationSettingsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getNotificationSettingsWithCompletionHandlerSelector = mkSelector "getNotificationSettingsWithCompletionHandler:"

-- | @Selector@ for @addNotificationRequest:withCompletionHandler:@
addNotificationRequest_withCompletionHandlerSelector :: Selector '[Id UNNotificationRequest, Ptr ()] ()
addNotificationRequest_withCompletionHandlerSelector = mkSelector "addNotificationRequest:withCompletionHandler:"

-- | @Selector@ for @removePendingNotificationRequestsWithIdentifiers:@
removePendingNotificationRequestsWithIdentifiersSelector :: Selector '[Id NSArray] ()
removePendingNotificationRequestsWithIdentifiersSelector = mkSelector "removePendingNotificationRequestsWithIdentifiers:"

-- | @Selector@ for @removeAllPendingNotificationRequests@
removeAllPendingNotificationRequestsSelector :: Selector '[] ()
removeAllPendingNotificationRequestsSelector = mkSelector "removeAllPendingNotificationRequests"

-- | @Selector@ for @removeDeliveredNotificationsWithIdentifiers:@
removeDeliveredNotificationsWithIdentifiersSelector :: Selector '[Id NSArray] ()
removeDeliveredNotificationsWithIdentifiersSelector = mkSelector "removeDeliveredNotificationsWithIdentifiers:"

-- | @Selector@ for @removeAllDeliveredNotifications@
removeAllDeliveredNotificationsSelector :: Selector '[] ()
removeAllDeliveredNotificationsSelector = mkSelector "removeAllDeliveredNotifications"

-- | @Selector@ for @setBadgeCount:withCompletionHandler:@
setBadgeCount_withCompletionHandlerSelector :: Selector '[CLong, Ptr ()] ()
setBadgeCount_withCompletionHandlerSelector = mkSelector "setBadgeCount:withCompletionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @supportsContentExtensions@
supportsContentExtensionsSelector :: Selector '[] Bool
supportsContentExtensionsSelector = mkSelector "supportsContentExtensions"

