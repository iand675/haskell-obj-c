{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Notification Center	***************
--
-- Generated bindings for @NSNotificationCenter@.
module ObjC.Foundation.NSNotificationCenter
  ( NSNotificationCenter
  , IsNSNotificationCenter(..)
  , addObserver_selector_name_object
  , postNotification
  , postNotificationName_object
  , postNotificationName_object_userInfo
  , removeObserver
  , removeObserver_name_object
  , addObserverForName_object_queue_usingBlock
  , defaultCenter
  , addObserver_selector_name_objectSelector
  , postNotificationSelector
  , postNotificationName_objectSelector
  , postNotificationName_object_userInfoSelector
  , removeObserverSelector
  , removeObserver_name_objectSelector
  , addObserverForName_object_queue_usingBlockSelector
  , defaultCenterSelector


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

-- | @- addObserver:selector:name:object:@
addObserver_selector_name_object :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName) => nsNotificationCenter -> RawId -> Selector -> aName -> RawId -> IO ()
addObserver_selector_name_object nsNotificationCenter  observer aSelector aName anObject =
withObjCPtr aName $ \raw_aName ->
    sendMsg nsNotificationCenter (mkSelector "addObserver:selector:name:object:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (unSelector aSelector), argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- postNotification:@
postNotification :: (IsNSNotificationCenter nsNotificationCenter, IsNSNotification notification) => nsNotificationCenter -> notification -> IO ()
postNotification nsNotificationCenter  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsNotificationCenter (mkSelector "postNotification:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- postNotificationName:object:@
postNotificationName_object :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName) => nsNotificationCenter -> aName -> RawId -> IO ()
postNotificationName_object nsNotificationCenter  aName anObject =
withObjCPtr aName $ \raw_aName ->
    sendMsg nsNotificationCenter (mkSelector "postNotificationName:object:") retVoid [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- postNotificationName:object:userInfo:@
postNotificationName_object_userInfo :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName, IsNSDictionary aUserInfo) => nsNotificationCenter -> aName -> RawId -> aUserInfo -> IO ()
postNotificationName_object_userInfo nsNotificationCenter  aName anObject aUserInfo =
withObjCPtr aName $ \raw_aName ->
  withObjCPtr aUserInfo $ \raw_aUserInfo ->
      sendMsg nsNotificationCenter (mkSelector "postNotificationName:object:userInfo:") retVoid [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr raw_aUserInfo :: Ptr ())]

-- | @- removeObserver:@
removeObserver :: IsNSNotificationCenter nsNotificationCenter => nsNotificationCenter -> RawId -> IO ()
removeObserver nsNotificationCenter  observer =
  sendMsg nsNotificationCenter (mkSelector "removeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- removeObserver:name:object:@
removeObserver_name_object :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName) => nsNotificationCenter -> RawId -> aName -> RawId -> IO ()
removeObserver_name_object nsNotificationCenter  observer aName anObject =
withObjCPtr aName $ \raw_aName ->
    sendMsg nsNotificationCenter (mkSelector "removeObserver:name:object:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- addObserverForName:object:queue:usingBlock:@
addObserverForName_object_queue_usingBlock :: (IsNSNotificationCenter nsNotificationCenter, IsNSString name, IsNSOperationQueue queue) => nsNotificationCenter -> name -> RawId -> queue -> Ptr () -> IO RawId
addObserverForName_object_queue_usingBlock nsNotificationCenter  name obj_ queue block =
withObjCPtr name $ \raw_name ->
  withObjCPtr queue $ \raw_queue ->
      fmap (RawId . castPtr) $ sendMsg nsNotificationCenter (mkSelector "addObserverForName:object:queue:usingBlock:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @+ defaultCenter@
defaultCenter :: IO (Id NSNotificationCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "NSNotificationCenter"
    sendClassMsg cls' (mkSelector "defaultCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObserver:selector:name:object:@
addObserver_selector_name_objectSelector :: Selector
addObserver_selector_name_objectSelector = mkSelector "addObserver:selector:name:object:"

-- | @Selector@ for @postNotification:@
postNotificationSelector :: Selector
postNotificationSelector = mkSelector "postNotification:"

-- | @Selector@ for @postNotificationName:object:@
postNotificationName_objectSelector :: Selector
postNotificationName_objectSelector = mkSelector "postNotificationName:object:"

-- | @Selector@ for @postNotificationName:object:userInfo:@
postNotificationName_object_userInfoSelector :: Selector
postNotificationName_object_userInfoSelector = mkSelector "postNotificationName:object:userInfo:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @removeObserver:name:object:@
removeObserver_name_objectSelector :: Selector
removeObserver_name_objectSelector = mkSelector "removeObserver:name:object:"

-- | @Selector@ for @addObserverForName:object:queue:usingBlock:@
addObserverForName_object_queue_usingBlockSelector :: Selector
addObserverForName_object_queue_usingBlockSelector = mkSelector "addObserverForName:object:queue:usingBlock:"

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector
defaultCenterSelector = mkSelector "defaultCenter"

