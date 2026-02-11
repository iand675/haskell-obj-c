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
  , initWithName_object_userInfoSelector
  , initWithCoderSelector
  , notificationWithName_objectSelector
  , notificationWithName_object_userInfoSelector
  , initSelector
  , nameSelector
  , objectSelector
  , userInfoSelector


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

-- | @- initWithName:object:userInfo:@
initWithName_object_userInfo :: (IsNSNotification nsNotification, IsNSString name, IsNSDictionary userInfo) => nsNotification -> name -> RawId -> userInfo -> IO (Id NSNotification)
initWithName_object_userInfo nsNotification  name object userInfo =
withObjCPtr name $ \raw_name ->
  withObjCPtr userInfo $ \raw_userInfo ->
      sendMsg nsNotification (mkSelector "initWithName:object:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSNotification nsNotification, IsNSCoder coder) => nsNotification -> coder -> IO (Id NSNotification)
initWithCoder nsNotification  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsNotification (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ notificationWithName:object:@
notificationWithName_object :: IsNSString aName => aName -> RawId -> IO (Id NSNotification)
notificationWithName_object aName anObject =
  do
    cls' <- getRequiredClass "NSNotification"
    withObjCPtr aName $ \raw_aName ->
      sendClassMsg cls' (mkSelector "notificationWithName:object:") (retPtr retVoid) [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ notificationWithName:object:userInfo:@
notificationWithName_object_userInfo :: (IsNSString aName, IsNSDictionary aUserInfo) => aName -> RawId -> aUserInfo -> IO (Id NSNotification)
notificationWithName_object_userInfo aName anObject aUserInfo =
  do
    cls' <- getRequiredClass "NSNotification"
    withObjCPtr aName $ \raw_aName ->
      withObjCPtr aUserInfo $ \raw_aUserInfo ->
        sendClassMsg cls' (mkSelector "notificationWithName:object:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr raw_aUserInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSNotification nsNotification => nsNotification -> IO (Id NSNotification)
init_ nsNotification  =
  sendMsg nsNotification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- name@
name :: IsNSNotification nsNotification => nsNotification -> IO (Id NSString)
name nsNotification  =
  sendMsg nsNotification (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- object@
object :: IsNSNotification nsNotification => nsNotification -> IO RawId
object nsNotification  =
  fmap (RawId . castPtr) $ sendMsg nsNotification (mkSelector "object") (retPtr retVoid) []

-- | @- userInfo@
userInfo :: IsNSNotification nsNotification => nsNotification -> IO (Id NSDictionary)
userInfo nsNotification  =
  sendMsg nsNotification (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:object:userInfo:@
initWithName_object_userInfoSelector :: Selector
initWithName_object_userInfoSelector = mkSelector "initWithName:object:userInfo:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @notificationWithName:object:@
notificationWithName_objectSelector :: Selector
notificationWithName_objectSelector = mkSelector "notificationWithName:object:"

-- | @Selector@ for @notificationWithName:object:userInfo:@
notificationWithName_object_userInfoSelector :: Selector
notificationWithName_object_userInfoSelector = mkSelector "notificationWithName:object:userInfo:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @object@
objectSelector :: Selector
objectSelector = mkSelector "object"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

