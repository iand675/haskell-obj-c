{-# LANGUAGE PatternSynonyms #-}
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
  , notificationCenterForTypeSelector
  , defaultCenterSelector
  , addObserver_selector_name_object_suspensionBehaviorSelector
  , postNotificationName_object_userInfo_deliverImmediatelySelector
  , postNotificationName_object_userInfo_optionsSelector
  , addObserver_selector_name_objectSelector
  , postNotificationName_objectSelector
  , postNotificationName_object_userInfoSelector
  , removeObserver_name_objectSelector
  , suspendedSelector
  , setSuspendedSelector

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
import ObjC.Foundation.Internal.Enums

-- | @+ notificationCenterForType:@
notificationCenterForType :: IsNSString notificationCenterType => notificationCenterType -> IO (Id NSDistributedNotificationCenter)
notificationCenterForType notificationCenterType =
  do
    cls' <- getRequiredClass "NSDistributedNotificationCenter"
    withObjCPtr notificationCenterType $ \raw_notificationCenterType ->
      sendClassMsg cls' (mkSelector "notificationCenterForType:") (retPtr retVoid) [argPtr (castPtr raw_notificationCenterType :: Ptr ())] >>= retainedObject . castPtr

-- | @+ defaultCenter@
defaultCenter :: IO (Id NSDistributedNotificationCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "NSDistributedNotificationCenter"
    sendClassMsg cls' (mkSelector "defaultCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addObserver:selector:name:object:suspensionBehavior:@
addObserver_selector_name_object_suspensionBehavior :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString name, IsNSString object) => nsDistributedNotificationCenter -> RawId -> Selector -> name -> object -> NSNotificationSuspensionBehavior -> IO ()
addObserver_selector_name_object_suspensionBehavior nsDistributedNotificationCenter  observer selector name object suspensionBehavior =
withObjCPtr name $ \raw_name ->
  withObjCPtr object $ \raw_object ->
      sendMsg nsDistributedNotificationCenter (mkSelector "addObserver:selector:name:object:suspensionBehavior:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (unSelector selector), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_object :: Ptr ()), argCULong (coerce suspensionBehavior)]

-- | @- postNotificationName:object:userInfo:deliverImmediately:@
postNotificationName_object_userInfo_deliverImmediately :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString name, IsNSString object, IsNSDictionary userInfo) => nsDistributedNotificationCenter -> name -> object -> userInfo -> Bool -> IO ()
postNotificationName_object_userInfo_deliverImmediately nsDistributedNotificationCenter  name object userInfo deliverImmediately =
withObjCPtr name $ \raw_name ->
  withObjCPtr object $ \raw_object ->
    withObjCPtr userInfo $ \raw_userInfo ->
        sendMsg nsDistributedNotificationCenter (mkSelector "postNotificationName:object:userInfo:deliverImmediately:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_object :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ()), argCULong (if deliverImmediately then 1 else 0)]

-- | @- postNotificationName:object:userInfo:options:@
postNotificationName_object_userInfo_options :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString name, IsNSString object, IsNSDictionary userInfo) => nsDistributedNotificationCenter -> name -> object -> userInfo -> NSDistributedNotificationOptions -> IO ()
postNotificationName_object_userInfo_options nsDistributedNotificationCenter  name object userInfo options =
withObjCPtr name $ \raw_name ->
  withObjCPtr object $ \raw_object ->
    withObjCPtr userInfo $ \raw_userInfo ->
        sendMsg nsDistributedNotificationCenter (mkSelector "postNotificationName:object:userInfo:options:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_object :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ()), argCULong (coerce options)]

-- | @- addObserver:selector:name:object:@
addObserver_selector_name_object :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject) => nsDistributedNotificationCenter -> RawId -> Selector -> aName -> anObject -> IO ()
addObserver_selector_name_object nsDistributedNotificationCenter  observer aSelector aName anObject =
withObjCPtr aName $ \raw_aName ->
  withObjCPtr anObject $ \raw_anObject ->
      sendMsg nsDistributedNotificationCenter (mkSelector "addObserver:selector:name:object:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (unSelector aSelector), argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr raw_anObject :: Ptr ())]

-- | @- postNotificationName:object:@
postNotificationName_object :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject) => nsDistributedNotificationCenter -> aName -> anObject -> IO ()
postNotificationName_object nsDistributedNotificationCenter  aName anObject =
withObjCPtr aName $ \raw_aName ->
  withObjCPtr anObject $ \raw_anObject ->
      sendMsg nsDistributedNotificationCenter (mkSelector "postNotificationName:object:") retVoid [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr raw_anObject :: Ptr ())]

-- | @- postNotificationName:object:userInfo:@
postNotificationName_object_userInfo :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject, IsNSDictionary aUserInfo) => nsDistributedNotificationCenter -> aName -> anObject -> aUserInfo -> IO ()
postNotificationName_object_userInfo nsDistributedNotificationCenter  aName anObject aUserInfo =
withObjCPtr aName $ \raw_aName ->
  withObjCPtr anObject $ \raw_anObject ->
    withObjCPtr aUserInfo $ \raw_aUserInfo ->
        sendMsg nsDistributedNotificationCenter (mkSelector "postNotificationName:object:userInfo:") retVoid [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr raw_anObject :: Ptr ()), argPtr (castPtr raw_aUserInfo :: Ptr ())]

-- | @- removeObserver:name:object:@
removeObserver_name_object :: (IsNSDistributedNotificationCenter nsDistributedNotificationCenter, IsNSString aName, IsNSString anObject) => nsDistributedNotificationCenter -> RawId -> aName -> anObject -> IO ()
removeObserver_name_object nsDistributedNotificationCenter  observer aName anObject =
withObjCPtr aName $ \raw_aName ->
  withObjCPtr anObject $ \raw_anObject ->
      sendMsg nsDistributedNotificationCenter (mkSelector "removeObserver:name:object:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr raw_anObject :: Ptr ())]

-- | @- suspended@
suspended :: IsNSDistributedNotificationCenter nsDistributedNotificationCenter => nsDistributedNotificationCenter -> IO Bool
suspended nsDistributedNotificationCenter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDistributedNotificationCenter (mkSelector "suspended") retCULong []

-- | @- setSuspended:@
setSuspended :: IsNSDistributedNotificationCenter nsDistributedNotificationCenter => nsDistributedNotificationCenter -> Bool -> IO ()
setSuspended nsDistributedNotificationCenter  value =
  sendMsg nsDistributedNotificationCenter (mkSelector "setSuspended:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @notificationCenterForType:@
notificationCenterForTypeSelector :: Selector
notificationCenterForTypeSelector = mkSelector "notificationCenterForType:"

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector
defaultCenterSelector = mkSelector "defaultCenter"

-- | @Selector@ for @addObserver:selector:name:object:suspensionBehavior:@
addObserver_selector_name_object_suspensionBehaviorSelector :: Selector
addObserver_selector_name_object_suspensionBehaviorSelector = mkSelector "addObserver:selector:name:object:suspensionBehavior:"

-- | @Selector@ for @postNotificationName:object:userInfo:deliverImmediately:@
postNotificationName_object_userInfo_deliverImmediatelySelector :: Selector
postNotificationName_object_userInfo_deliverImmediatelySelector = mkSelector "postNotificationName:object:userInfo:deliverImmediately:"

-- | @Selector@ for @postNotificationName:object:userInfo:options:@
postNotificationName_object_userInfo_optionsSelector :: Selector
postNotificationName_object_userInfo_optionsSelector = mkSelector "postNotificationName:object:userInfo:options:"

-- | @Selector@ for @addObserver:selector:name:object:@
addObserver_selector_name_objectSelector :: Selector
addObserver_selector_name_objectSelector = mkSelector "addObserver:selector:name:object:"

-- | @Selector@ for @postNotificationName:object:@
postNotificationName_objectSelector :: Selector
postNotificationName_objectSelector = mkSelector "postNotificationName:object:"

-- | @Selector@ for @postNotificationName:object:userInfo:@
postNotificationName_object_userInfoSelector :: Selector
postNotificationName_object_userInfoSelector = mkSelector "postNotificationName:object:userInfo:"

-- | @Selector@ for @removeObserver:name:object:@
removeObserver_name_objectSelector :: Selector
removeObserver_name_objectSelector = mkSelector "removeObserver:name:object:"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @setSuspended:@
setSuspendedSelector :: Selector
setSuspendedSelector = mkSelector "setSuspended:"

