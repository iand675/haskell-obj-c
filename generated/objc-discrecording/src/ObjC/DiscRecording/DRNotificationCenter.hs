{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRNotificationCenter
--
-- A DRNotificationCenter object (or simply, notification center) is				essentially a notification dispatch table. It notifies all observers of				notifications meeting specific criteria. This information is encapsulated in				NSNotification objects, also known as notifications. Client objects register				themselves with the notification center as observers of specific notifications				posted by DiscRecording. When an event occurs, DiscRecording posts an appropriate				notification to the notification center. The notification center dispatches a				message to each registered observer, passing the notification as the sole				argument.
--
-- There are two main differences between a DRNotificationCenter and the				NSNotificationCenter from Foundation. First is that only Disc Recording				posts notifications received through this mechanism. You use this to 				obtain device plug/unplug events, burn status, etc. Second, there can be				multple notification centers active at once. Each run loop of your application				will have it's own notification center and notifications from that notification				center will be posted to the runloop it was created on.
--
-- Generated bindings for @DRNotificationCenter@.
module ObjC.DiscRecording.DRNotificationCenter
  ( DRNotificationCenter
  , IsDRNotificationCenter(..)
  , currentRunLoopCenter
  , addObserver_selector_name_object
  , removeObserver_name_object
  , currentRunLoopCenterSelector
  , addObserver_selector_name_objectSelector
  , removeObserver_name_objectSelector


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

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | currentRunLoopCenter
--
-- Creates an initializes a DRNotificationCenter
--
-- The instance returned sends Disc Recording notifications only					to the current run loop. If you want to receive notifications 					on another run loop, this method must be called from that runloop.
--
-- Returns: A shared DRNotificationCenter object.
--
-- ObjC selector: @+ currentRunLoopCenter@
currentRunLoopCenter :: IO (Id DRNotificationCenter)
currentRunLoopCenter  =
  do
    cls' <- getRequiredClass "DRNotificationCenter"
    sendClassMsg cls' (mkSelector "currentRunLoopCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | addObserver:selector:name:object:
--
-- Adds an observer to the receiver.
--
-- Registers anObserver to receive notifications with the name notificationName					and/or containing anObject. When a notification of name notificationName					containing the object anObject is posted, anObserver receives an aSelector					message with this notification as the argument. The method for the selector					specified in aSelector must have one and only one argument. If notificationName					is nil, the notification center notifies the observer of all notifications with					an object matching anObject. If anObject is nil, the notification center					notifies the observer of all notifications with the name notificationName.
--
-- The notification center does not retain anObserver or anObject. Therefore, you					should always send
--
-- //apple_ref/occ/instm/DRNotificationCenter/removeObserver:name:object: removeObserver:name:object:
--
-- to the notification center 					before releasing these objects.
--
-- @observer@ — The observer to send notifications to.
--
-- @aSelector@ — The selector to call
--
-- @notificationName@ — The notification to listen for
--
-- @anObject@ — The object to limit notifications for.
--
-- ObjC selector: @- addObserver:selector:name:object:@
addObserver_selector_name_object :: (IsDRNotificationCenter drNotificationCenter, IsNSString notificationName) => drNotificationCenter -> RawId -> Selector -> notificationName -> RawId -> IO ()
addObserver_selector_name_object drNotificationCenter  observer aSelector notificationName anObject =
withObjCPtr notificationName $ \raw_notificationName ->
    sendMsg drNotificationCenter (mkSelector "addObserver:selector:name:object:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (unSelector aSelector), argPtr (castPtr raw_notificationName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | removeObserver:name:object:
--
-- Removes anObserver from receiving notifications.
--
-- Removes anObserver as the observer of notifications with the name					notificationName and object anObject from the receiver. Be sure to invoke this					method before deallocating the observer object or any object specified in
--
-- //apple_ref/occ/instm/DRNotificationCenter/addObserver:selector:name:object: addObserver:selector:name:object:
--
-- .
--
-- @observer@ — The observer to remove
--
-- @aName@ — The notification the remove the observer from.
--
-- @anObject@ — The object the observer was listening for.
--
-- ObjC selector: @- removeObserver:name:object:@
removeObserver_name_object :: (IsDRNotificationCenter drNotificationCenter, IsNSString aName) => drNotificationCenter -> RawId -> aName -> RawId -> IO ()
removeObserver_name_object drNotificationCenter  observer aName anObject =
withObjCPtr aName $ \raw_aName ->
    sendMsg drNotificationCenter (mkSelector "removeObserver:name:object:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentRunLoopCenter@
currentRunLoopCenterSelector :: Selector
currentRunLoopCenterSelector = mkSelector "currentRunLoopCenter"

-- | @Selector@ for @addObserver:selector:name:object:@
addObserver_selector_name_objectSelector :: Selector
addObserver_selector_name_objectSelector = mkSelector "addObserver:selector:name:object:"

-- | @Selector@ for @removeObserver:name:object:@
removeObserver_name_objectSelector :: Selector
removeObserver_name_objectSelector = mkSelector "removeObserver:name:object:"

