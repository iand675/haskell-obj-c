{-# LANGUAGE DataKinds #-}
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
  , addObserverForName_object_queue_usingBlockSelector
  , addObserver_selector_name_objectSelector
  , defaultCenterSelector
  , postNotificationName_objectSelector
  , postNotificationName_object_userInfoSelector
  , postNotificationSelector
  , removeObserverSelector
  , removeObserver_name_objectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- addObserver:selector:name:object:@
addObserver_selector_name_object :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName) => nsNotificationCenter -> RawId -> Sel -> aName -> RawId -> IO ()
addObserver_selector_name_object nsNotificationCenter observer aSelector aName anObject =
  sendMessage nsNotificationCenter addObserver_selector_name_objectSelector observer aSelector (toNSString aName) anObject

-- | @- postNotification:@
postNotification :: (IsNSNotificationCenter nsNotificationCenter, IsNSNotification notification) => nsNotificationCenter -> notification -> IO ()
postNotification nsNotificationCenter notification =
  sendMessage nsNotificationCenter postNotificationSelector (toNSNotification notification)

-- | @- postNotificationName:object:@
postNotificationName_object :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName) => nsNotificationCenter -> aName -> RawId -> IO ()
postNotificationName_object nsNotificationCenter aName anObject =
  sendMessage nsNotificationCenter postNotificationName_objectSelector (toNSString aName) anObject

-- | @- postNotificationName:object:userInfo:@
postNotificationName_object_userInfo :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName, IsNSDictionary aUserInfo) => nsNotificationCenter -> aName -> RawId -> aUserInfo -> IO ()
postNotificationName_object_userInfo nsNotificationCenter aName anObject aUserInfo =
  sendMessage nsNotificationCenter postNotificationName_object_userInfoSelector (toNSString aName) anObject (toNSDictionary aUserInfo)

-- | @- removeObserver:@
removeObserver :: IsNSNotificationCenter nsNotificationCenter => nsNotificationCenter -> RawId -> IO ()
removeObserver nsNotificationCenter observer =
  sendMessage nsNotificationCenter removeObserverSelector observer

-- | @- removeObserver:name:object:@
removeObserver_name_object :: (IsNSNotificationCenter nsNotificationCenter, IsNSString aName) => nsNotificationCenter -> RawId -> aName -> RawId -> IO ()
removeObserver_name_object nsNotificationCenter observer aName anObject =
  sendMessage nsNotificationCenter removeObserver_name_objectSelector observer (toNSString aName) anObject

-- | @- addObserverForName:object:queue:usingBlock:@
addObserverForName_object_queue_usingBlock :: (IsNSNotificationCenter nsNotificationCenter, IsNSString name, IsNSOperationQueue queue) => nsNotificationCenter -> name -> RawId -> queue -> Ptr () -> IO RawId
addObserverForName_object_queue_usingBlock nsNotificationCenter name obj_ queue block =
  sendMessage nsNotificationCenter addObserverForName_object_queue_usingBlockSelector (toNSString name) obj_ (toNSOperationQueue queue) block

-- | @+ defaultCenter@
defaultCenter :: IO (Id NSNotificationCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "NSNotificationCenter"
    sendClassMessage cls' defaultCenterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObserver:selector:name:object:@
addObserver_selector_name_objectSelector :: Selector '[RawId, Sel, Id NSString, RawId] ()
addObserver_selector_name_objectSelector = mkSelector "addObserver:selector:name:object:"

-- | @Selector@ for @postNotification:@
postNotificationSelector :: Selector '[Id NSNotification] ()
postNotificationSelector = mkSelector "postNotification:"

-- | @Selector@ for @postNotificationName:object:@
postNotificationName_objectSelector :: Selector '[Id NSString, RawId] ()
postNotificationName_objectSelector = mkSelector "postNotificationName:object:"

-- | @Selector@ for @postNotificationName:object:userInfo:@
postNotificationName_object_userInfoSelector :: Selector '[Id NSString, RawId, Id NSDictionary] ()
postNotificationName_object_userInfoSelector = mkSelector "postNotificationName:object:userInfo:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector '[RawId] ()
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @removeObserver:name:object:@
removeObserver_name_objectSelector :: Selector '[RawId, Id NSString, RawId] ()
removeObserver_name_objectSelector = mkSelector "removeObserver:name:object:"

-- | @Selector@ for @addObserverForName:object:queue:usingBlock:@
addObserverForName_object_queue_usingBlockSelector :: Selector '[Id NSString, RawId, Id NSOperationQueue, Ptr ()] RawId
addObserverForName_object_queue_usingBlockSelector = mkSelector "addObserverForName:object:queue:usingBlock:"

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector '[] (Id NSNotificationCenter)
defaultCenterSelector = mkSelector "defaultCenter"

