{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDistributedNotificationCenter@.
module ObjC.Foundation.NSDistributedNotificationCenter
  ( NSDistributedNotificationCenter
  , IsNSDistributedNotificationCenter(..)
  , notificationCenterForType
  , defaultCenter
  , addObserver_selector_name_object_suspensionBehavior
  , postNotificationName_object_userInfo_deliverImmediately
  , postNotificationName_object_userInfo_options
  , addObserver_selector_name_object
  , postNotificationName_object
  , postNotificationName_object_userInfo
  , removeObserver_name_object
  , suspended
  , setSuspended
  , addObserver_selector_name_objectSelector
  , addObserver_selector_name_object_suspensionBehaviorSelector
  , defaultCenterSelector
  , notificationCenterForTypeSelector
  , postNotificationName_objectSelector
  , postNotificationName_object_userInfoSelector
  , postNotificationName_object_userInfo_deliverImmediatelySelector
  , postNotificationName_object_userInfo_optionsSelector
  , removeObserver_name_objectSelector
  , setSuspendedSelector
  , suspendedSelector

  -- * Enum types
  , NSDistributedNotificationOptions(NSDistributedNotificationOptions)
  , pattern NSDistributedNotificationDeliverImmediately
  , pattern NSDistributedNotificationPostToAllSessions
  , NSNotificationSuspensionBehavior(NSNotificationSuspensionBehavior)
  , pattern NSNotificationSuspensionBehaviorDrop
  , pattern NSNotificationSuspensionBehaviorCoalesce
  , pattern NSNotificationSuspensionBehaviorHold
  , pattern NSNotificationSuspensionBehaviorDeliverImmediately

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ notificationCenterForType:@
notificationCenterForType :: IsNSString notificationCenterType => notificationCenterType -> IO (Id NSDistributedNotificationCenter)
notificationCenterForType notificationCenterType =
  do
    cls' <- getRequiredClass "NSDistributedNotificationCenter"
    sendClassMessage cls' notificationCenterForTypeSelector (toNSString notificationCenterType)

-- | @+ defaultCenter@
defaultCenter :: IO (Id NSDistributedNotificationCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "NSDistributedNotificationCenter"
    sendClassMessage cls' defaultCenterSelector

-- | @- addObserver:selector:name:object:suspensionBehavior:@
addObserver_selector_name_object_suspensionBehavior :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString name, IsNSString object) => nsDistributedNotificationCenter -> RawId -> Sel -> name -> object -> NSNotificationSuspensionBehavior -> IO ()
addObserver_selector_name_object_suspensionBehavior nsDistributedNotificationCenter observer selector name object suspensionBehavior =
  sendMessage nsDistributedNotificationCenter addObserver_selector_name_object_suspensionBehaviorSelector observer selector (toNSString name) (toNSString object) suspensionBehavior

-- | @- postNotificationName:object:userInfo:deliverImmediately:@
postNotificationName_object_userInfo_deliverImmediately :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString name, IsNSString object, IsNSDictionary userInfo) => nsDistributedNotificationCenter -> name -> object -> userInfo -> Bool -> IO ()
postNotificationName_object_userInfo_deliverImmediately nsDistributedNotificationCenter name object userInfo deliverImmediately =
  sendMessage nsDistributedNotificationCenter postNotificationName_object_userInfo_deliverImmediatelySelector (toNSString name) (toNSString object) (toNSDictionary userInfo) deliverImmediately

-- | @- postNotificationName:object:userInfo:options:@
postNotificationName_object_userInfo_options :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString name, IsNSString object, IsNSDictionary userInfo) => nsDistributedNotificationCenter -> name -> object -> userInfo -> NSDistributedNotificationOptions -> IO ()
postNotificationName_object_userInfo_options nsDistributedNotificationCenter name object userInfo options =
  sendMessage nsDistributedNotificationCenter postNotificationName_object_userInfo_optionsSelector (toNSString name) (toNSString object) (toNSDictionary userInfo) options

-- | @- addObserver:selector:name:object:@
addObserver_selector_name_object :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject) => nsDistributedNotificationCenter -> RawId -> Sel -> aName -> anObject -> IO ()
addObserver_selector_name_object nsDistributedNotificationCenter observer aSelector aName anObject =
  sendMessage nsDistributedNotificationCenter addObserver_selector_name_objectSelector observer aSelector (toNSString aName) (toNSString anObject)

-- | @- postNotificationName:object:@
postNotificationName_object :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject) => nsDistributedNotificationCenter -> aName -> anObject -> IO ()
postNotificationName_object nsDistributedNotificationCenter aName anObject =
  sendMessage nsDistributedNotificationCenter postNotificationName_objectSelector (toNSString aName) (toNSString anObject)

-- | @- postNotificationName:object:userInfo:@
postNotificationName_object_userInfo :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject, IsNSDictionary aUserInfo) => nsDistributedNotificationCenter -> aName -> anObject -> aUserInfo -> IO ()
postNotificationName_object_userInfo nsDistributedNotificationCenter aName anObject aUserInfo =
  sendMessage nsDistributedNotificationCenter postNotificationName_object_userInfoSelector (toNSString aName) (toNSString anObject) (toNSDictionary aUserInfo)

-- | @- removeObserver:name:object:@
removeObserver_name_object :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject) => nsDistributedNotificationCenter -> RawId -> aName -> anObject -> IO ()
removeObserver_name_object nsDistributedNotificationCenter observer aName anObject =
  sendMessage nsDistributedNotificationCenter removeObserver_name_objectSelector observer (toNSString aName) (toNSString anObject)

-- | @- suspended@
suspended :: IsNSDistributedNotificationCenter nsDistributedNotificationCenter => nsDistributedNotificationCenter -> IO Bool
suspended nsDistributedNotificationCenter =
  sendMessage nsDistributedNotificationCenter suspendedSelector

-- | @- setSuspended:@
setSuspended :: IsNSDistributedNotificationCenter nsDistributedNotificationCenter => nsDistributedNotificationCenter -> Bool -> IO ()
setSuspended nsDistributedNotificationCenter value =
  sendMessage nsDistributedNotificationCenter setSuspendedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @notificationCenterForType:@
notificationCenterForTypeSelector :: Selector '[Id NSString] (Id NSDistributedNotificationCenter)
notificationCenterForTypeSelector = mkSelector "notificationCenterForType:"

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector '[] (Id NSDistributedNotificationCenter)
defaultCenterSelector = mkSelector "defaultCenter"

-- | @Selector@ for @addObserver:selector:name:object:suspensionBehavior:@
addObserver_selector_name_object_suspensionBehaviorSelector :: Selector '[RawId, Sel, Id NSString, Id NSString, NSNotificationSuspensionBehavior] ()
addObserver_selector_name_object_suspensionBehaviorSelector = mkSelector "addObserver:selector:name:object:suspensionBehavior:"

-- | @Selector@ for @postNotificationName:object:userInfo:deliverImmediately:@
postNotificationName_object_userInfo_deliverImmediatelySelector :: Selector '[Id NSString, Id NSString, Id NSDictionary, Bool] ()
postNotificationName_object_userInfo_deliverImmediatelySelector = mkSelector "postNotificationName:object:userInfo:deliverImmediately:"

-- | @Selector@ for @postNotificationName:object:userInfo:options:@
postNotificationName_object_userInfo_optionsSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary, NSDistributedNotificationOptions] ()
postNotificationName_object_userInfo_optionsSelector = mkSelector "postNotificationName:object:userInfo:options:"

-- | @Selector@ for @addObserver:selector:name:object:@
addObserver_selector_name_objectSelector :: Selector '[RawId, Sel, Id NSString, Id NSString] ()
addObserver_selector_name_objectSelector = mkSelector "addObserver:selector:name:object:"

-- | @Selector@ for @postNotificationName:object:@
postNotificationName_objectSelector :: Selector '[Id NSString, Id NSString] ()
postNotificationName_objectSelector = mkSelector "postNotificationName:object:"

-- | @Selector@ for @postNotificationName:object:userInfo:@
postNotificationName_object_userInfoSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] ()
postNotificationName_object_userInfoSelector = mkSelector "postNotificationName:object:userInfo:"

-- | @Selector@ for @removeObserver:name:object:@
removeObserver_name_objectSelector :: Selector '[RawId, Id NSString, Id NSString] ()
removeObserver_name_objectSelector = mkSelector "removeObserver:name:object:"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] Bool
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @setSuspended:@
setSuspendedSelector :: Selector '[Bool] ()
setSuspendedSelector = mkSelector "setSuspended:"

