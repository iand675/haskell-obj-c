{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Notifications	***************
--
-- Generated bindings for @NSNotification@.
module ObjC.Foundation.NSNotification
  ( NSNotification
  , IsNSNotification(..)
  , initWithName_object_userInfo
  , initWithCoder
  , notificationWithName_object
  , notificationWithName_object_userInfo
  , init_
  , name
  , object
  , userInfo
  , initSelector
  , initWithCoderSelector
  , initWithName_object_userInfoSelector
  , nameSelector
  , notificationWithName_objectSelector
  , notificationWithName_object_userInfoSelector
  , objectSelector
  , userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithName:object:userInfo:@
initWithName_object_userInfo :: (IsNSNotification nsNotification, IsNSString name, IsNSDictionary userInfo) => nsNotification -> name -> RawId -> userInfo -> IO (Id NSNotification)
initWithName_object_userInfo nsNotification name object userInfo =
  sendOwnedMessage nsNotification initWithName_object_userInfoSelector (toNSString name) object (toNSDictionary userInfo)

-- | @- initWithCoder:@
initWithCoder :: (IsNSNotification nsNotification, IsNSCoder coder) => nsNotification -> coder -> IO (Id NSNotification)
initWithCoder nsNotification coder =
  sendOwnedMessage nsNotification initWithCoderSelector (toNSCoder coder)

-- | @+ notificationWithName:object:@
notificationWithName_object :: IsNSString aName => aName -> RawId -> IO (Id NSNotification)
notificationWithName_object aName anObject =
  do
    cls' <- getRequiredClass "NSNotification"
    sendClassMessage cls' notificationWithName_objectSelector (toNSString aName) anObject

-- | @+ notificationWithName:object:userInfo:@
notificationWithName_object_userInfo :: (IsNSString aName, IsNSDictionary aUserInfo) => aName -> RawId -> aUserInfo -> IO (Id NSNotification)
notificationWithName_object_userInfo aName anObject aUserInfo =
  do
    cls' <- getRequiredClass "NSNotification"
    sendClassMessage cls' notificationWithName_object_userInfoSelector (toNSString aName) anObject (toNSDictionary aUserInfo)

-- | @- init@
init_ :: IsNSNotification nsNotification => nsNotification -> IO (Id NSNotification)
init_ nsNotification =
  sendOwnedMessage nsNotification initSelector

-- | @- name@
name :: IsNSNotification nsNotification => nsNotification -> IO (Id NSString)
name nsNotification =
  sendMessage nsNotification nameSelector

-- | @- object@
object :: IsNSNotification nsNotification => nsNotification -> IO RawId
object nsNotification =
  sendMessage nsNotification objectSelector

-- | @- userInfo@
userInfo :: IsNSNotification nsNotification => nsNotification -> IO (Id NSDictionary)
userInfo nsNotification =
  sendMessage nsNotification userInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:object:userInfo:@
initWithName_object_userInfoSelector :: Selector '[Id NSString, RawId, Id NSDictionary] (Id NSNotification)
initWithName_object_userInfoSelector = mkSelector "initWithName:object:userInfo:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSNotification)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @notificationWithName:object:@
notificationWithName_objectSelector :: Selector '[Id NSString, RawId] (Id NSNotification)
notificationWithName_objectSelector = mkSelector "notificationWithName:object:"

-- | @Selector@ for @notificationWithName:object:userInfo:@
notificationWithName_object_userInfoSelector :: Selector '[Id NSString, RawId, Id NSDictionary] (Id NSNotification)
notificationWithName_object_userInfoSelector = mkSelector "notificationWithName:object:userInfo:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSNotification)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @object@
objectSelector :: Selector '[] RawId
objectSelector = mkSelector "object"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

