{-# LANGUAGE PatternSynonyms #-}
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
  , currentNotificationCenterSelector
  , initSelector
  , requestAuthorizationWithOptions_completionHandlerSelector
  , setNotificationCategoriesSelector
  , getNotificationSettingsWithCompletionHandlerSelector
  , addNotificationRequest_withCompletionHandlerSelector
  , removePendingNotificationRequestsWithIdentifiersSelector
  , removeAllPendingNotificationRequestsSelector
  , removeDeliveredNotificationsWithIdentifiersSelector
  , removeAllDeliveredNotificationsSelector
  , setBadgeCount_withCompletionHandlerSelector
  , delegateSelector
  , setDelegateSelector
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

import ObjC.UserNotifications.Internal.Classes
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ currentNotificationCenter@
currentNotificationCenter :: IO (Id UNUserNotificationCenter)
currentNotificationCenter  =
  do
    cls' <- getRequiredClass "UNUserNotificationCenter"
    sendClassMsg cls' (mkSelector "currentNotificationCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO (Id UNUserNotificationCenter)
init_ unUserNotificationCenter  =
    sendMsg unUserNotificationCenter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- requestAuthorizationWithOptions:completionHandler:@
requestAuthorizationWithOptions_completionHandler :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> UNAuthorizationOptions -> Ptr () -> IO ()
requestAuthorizationWithOptions_completionHandler unUserNotificationCenter  options completionHandler =
    sendMsg unUserNotificationCenter (mkSelector "requestAuthorizationWithOptions:completionHandler:") retVoid [argCULong (coerce options), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setNotificationCategories:@
setNotificationCategories :: (IsUNUserNotificationCenter unUserNotificationCenter, IsNSSet categories) => unUserNotificationCenter -> categories -> IO ()
setNotificationCategories unUserNotificationCenter  categories =
  withObjCPtr categories $ \raw_categories ->
      sendMsg unUserNotificationCenter (mkSelector "setNotificationCategories:") retVoid [argPtr (castPtr raw_categories :: Ptr ())]

-- | @- getNotificationSettingsWithCompletionHandler:@
getNotificationSettingsWithCompletionHandler :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> Ptr () -> IO ()
getNotificationSettingsWithCompletionHandler unUserNotificationCenter  completionHandler =
    sendMsg unUserNotificationCenter (mkSelector "getNotificationSettingsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addNotificationRequest:withCompletionHandler:@
addNotificationRequest_withCompletionHandler :: (IsUNUserNotificationCenter unUserNotificationCenter, IsUNNotificationRequest request) => unUserNotificationCenter -> request -> Ptr () -> IO ()
addNotificationRequest_withCompletionHandler unUserNotificationCenter  request completionHandler =
  withObjCPtr request $ \raw_request ->
      sendMsg unUserNotificationCenter (mkSelector "addNotificationRequest:withCompletionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removePendingNotificationRequestsWithIdentifiers:@
removePendingNotificationRequestsWithIdentifiers :: (IsUNUserNotificationCenter unUserNotificationCenter, IsNSArray identifiers) => unUserNotificationCenter -> identifiers -> IO ()
removePendingNotificationRequestsWithIdentifiers unUserNotificationCenter  identifiers =
  withObjCPtr identifiers $ \raw_identifiers ->
      sendMsg unUserNotificationCenter (mkSelector "removePendingNotificationRequestsWithIdentifiers:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ())]

-- | @- removeAllPendingNotificationRequests@
removeAllPendingNotificationRequests :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO ()
removeAllPendingNotificationRequests unUserNotificationCenter  =
    sendMsg unUserNotificationCenter (mkSelector "removeAllPendingNotificationRequests") retVoid []

-- | @- removeDeliveredNotificationsWithIdentifiers:@
removeDeliveredNotificationsWithIdentifiers :: (IsUNUserNotificationCenter unUserNotificationCenter, IsNSArray identifiers) => unUserNotificationCenter -> identifiers -> IO ()
removeDeliveredNotificationsWithIdentifiers unUserNotificationCenter  identifiers =
  withObjCPtr identifiers $ \raw_identifiers ->
      sendMsg unUserNotificationCenter (mkSelector "removeDeliveredNotificationsWithIdentifiers:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ())]

-- | @- removeAllDeliveredNotifications@
removeAllDeliveredNotifications :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO ()
removeAllDeliveredNotifications unUserNotificationCenter  =
    sendMsg unUserNotificationCenter (mkSelector "removeAllDeliveredNotifications") retVoid []

-- | @- setBadgeCount:withCompletionHandler:@
setBadgeCount_withCompletionHandler :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> CLong -> Ptr () -> IO ()
setBadgeCount_withCompletionHandler unUserNotificationCenter  newBadgeCount completionHandler =
    sendMsg unUserNotificationCenter (mkSelector "setBadgeCount:withCompletionHandler:") retVoid [argCLong newBadgeCount, argPtr (castPtr completionHandler :: Ptr ())]

-- | @- delegate@
delegate :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO RawId
delegate unUserNotificationCenter  =
    fmap (RawId . castPtr) $ sendMsg unUserNotificationCenter (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> RawId -> IO ()
setDelegate unUserNotificationCenter  value =
    sendMsg unUserNotificationCenter (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- supportsContentExtensions@
supportsContentExtensions :: IsUNUserNotificationCenter unUserNotificationCenter => unUserNotificationCenter -> IO Bool
supportsContentExtensions unUserNotificationCenter  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg unUserNotificationCenter (mkSelector "supportsContentExtensions") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentNotificationCenter@
currentNotificationCenterSelector :: Selector
currentNotificationCenterSelector = mkSelector "currentNotificationCenter"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @requestAuthorizationWithOptions:completionHandler:@
requestAuthorizationWithOptions_completionHandlerSelector :: Selector
requestAuthorizationWithOptions_completionHandlerSelector = mkSelector "requestAuthorizationWithOptions:completionHandler:"

-- | @Selector@ for @setNotificationCategories:@
setNotificationCategoriesSelector :: Selector
setNotificationCategoriesSelector = mkSelector "setNotificationCategories:"

-- | @Selector@ for @getNotificationSettingsWithCompletionHandler:@
getNotificationSettingsWithCompletionHandlerSelector :: Selector
getNotificationSettingsWithCompletionHandlerSelector = mkSelector "getNotificationSettingsWithCompletionHandler:"

-- | @Selector@ for @addNotificationRequest:withCompletionHandler:@
addNotificationRequest_withCompletionHandlerSelector :: Selector
addNotificationRequest_withCompletionHandlerSelector = mkSelector "addNotificationRequest:withCompletionHandler:"

-- | @Selector@ for @removePendingNotificationRequestsWithIdentifiers:@
removePendingNotificationRequestsWithIdentifiersSelector :: Selector
removePendingNotificationRequestsWithIdentifiersSelector = mkSelector "removePendingNotificationRequestsWithIdentifiers:"

-- | @Selector@ for @removeAllPendingNotificationRequests@
removeAllPendingNotificationRequestsSelector :: Selector
removeAllPendingNotificationRequestsSelector = mkSelector "removeAllPendingNotificationRequests"

-- | @Selector@ for @removeDeliveredNotificationsWithIdentifiers:@
removeDeliveredNotificationsWithIdentifiersSelector :: Selector
removeDeliveredNotificationsWithIdentifiersSelector = mkSelector "removeDeliveredNotificationsWithIdentifiers:"

-- | @Selector@ for @removeAllDeliveredNotifications@
removeAllDeliveredNotificationsSelector :: Selector
removeAllDeliveredNotificationsSelector = mkSelector "removeAllDeliveredNotifications"

-- | @Selector@ for @setBadgeCount:withCompletionHandler:@
setBadgeCount_withCompletionHandlerSelector :: Selector
setBadgeCount_withCompletionHandlerSelector = mkSelector "setBadgeCount:withCompletionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @supportsContentExtensions@
supportsContentExtensionsSelector :: Selector
supportsContentExtensionsSelector = mkSelector "supportsContentExtensions"

