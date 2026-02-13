{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAppleEventManager@.
module ObjC.Foundation.NSAppleEventManager
  ( NSAppleEventManager
  , IsNSAppleEventManager(..)
  , sharedAppleEventManager
  , setEventHandler_andSelector_forEventClass_andEventID
  , removeEventHandlerForEventClass_andEventID
  , dispatchRawAppleEvent_withRawReply_handlerRefCon
  , suspendCurrentAppleEvent
  , appleEventForSuspensionID
  , replyAppleEventForSuspensionID
  , setCurrentAppleEventAndReplyEventWithSuspensionID
  , resumeWithSuspensionID
  , currentAppleEvent
  , currentReplyAppleEvent
  , appleEventForSuspensionIDSelector
  , currentAppleEventSelector
  , currentReplyAppleEventSelector
  , dispatchRawAppleEvent_withRawReply_handlerRefConSelector
  , removeEventHandlerForEventClass_andEventIDSelector
  , replyAppleEventForSuspensionIDSelector
  , resumeWithSuspensionIDSelector
  , setCurrentAppleEventAndReplyEventWithSuspensionIDSelector
  , setEventHandler_andSelector_forEventClass_andEventIDSelector
  , sharedAppleEventManagerSelector
  , suspendCurrentAppleEventSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sharedAppleEventManager@
sharedAppleEventManager :: IO (Id NSAppleEventManager)
sharedAppleEventManager  =
  do
    cls' <- getRequiredClass "NSAppleEventManager"
    sendClassMessage cls' sharedAppleEventManagerSelector

-- | @- setEventHandler:andSelector:forEventClass:andEventID:@
setEventHandler_andSelector_forEventClass_andEventID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> Sel -> CUInt -> CUInt -> IO ()
setEventHandler_andSelector_forEventClass_andEventID nsAppleEventManager handler handleEventSelector eventClass eventID =
  sendMessage nsAppleEventManager setEventHandler_andSelector_forEventClass_andEventIDSelector handler handleEventSelector eventClass eventID

-- | @- removeEventHandlerForEventClass:andEventID:@
removeEventHandlerForEventClass_andEventID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> CUInt -> CUInt -> IO ()
removeEventHandlerForEventClass_andEventID nsAppleEventManager eventClass eventID =
  sendMessage nsAppleEventManager removeEventHandlerForEventClass_andEventIDSelector eventClass eventID

-- | @- dispatchRawAppleEvent:withRawReply:handlerRefCon:@
dispatchRawAppleEvent_withRawReply_handlerRefCon :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> Const RawId -> RawId -> RawId -> IO CShort
dispatchRawAppleEvent_withRawReply_handlerRefCon nsAppleEventManager theAppleEvent theReply handlerRefCon =
  sendMessage nsAppleEventManager dispatchRawAppleEvent_withRawReply_handlerRefConSelector theAppleEvent theReply handlerRefCon

-- | @- suspendCurrentAppleEvent@
suspendCurrentAppleEvent :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> IO RawId
suspendCurrentAppleEvent nsAppleEventManager =
  sendMessage nsAppleEventManager suspendCurrentAppleEventSelector

-- | @- appleEventForSuspensionID:@
appleEventForSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO (Id NSAppleEventDescriptor)
appleEventForSuspensionID nsAppleEventManager suspensionID =
  sendMessage nsAppleEventManager appleEventForSuspensionIDSelector suspensionID

-- | @- replyAppleEventForSuspensionID:@
replyAppleEventForSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO (Id NSAppleEventDescriptor)
replyAppleEventForSuspensionID nsAppleEventManager suspensionID =
  sendMessage nsAppleEventManager replyAppleEventForSuspensionIDSelector suspensionID

-- | @- setCurrentAppleEventAndReplyEventWithSuspensionID:@
setCurrentAppleEventAndReplyEventWithSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO ()
setCurrentAppleEventAndReplyEventWithSuspensionID nsAppleEventManager suspensionID =
  sendMessage nsAppleEventManager setCurrentAppleEventAndReplyEventWithSuspensionIDSelector suspensionID

-- | @- resumeWithSuspensionID:@
resumeWithSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO ()
resumeWithSuspensionID nsAppleEventManager suspensionID =
  sendMessage nsAppleEventManager resumeWithSuspensionIDSelector suspensionID

-- | @- currentAppleEvent@
currentAppleEvent :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> IO (Id NSAppleEventDescriptor)
currentAppleEvent nsAppleEventManager =
  sendMessage nsAppleEventManager currentAppleEventSelector

-- | @- currentReplyAppleEvent@
currentReplyAppleEvent :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> IO (Id NSAppleEventDescriptor)
currentReplyAppleEvent nsAppleEventManager =
  sendMessage nsAppleEventManager currentReplyAppleEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAppleEventManager@
sharedAppleEventManagerSelector :: Selector '[] (Id NSAppleEventManager)
sharedAppleEventManagerSelector = mkSelector "sharedAppleEventManager"

-- | @Selector@ for @setEventHandler:andSelector:forEventClass:andEventID:@
setEventHandler_andSelector_forEventClass_andEventIDSelector :: Selector '[RawId, Sel, CUInt, CUInt] ()
setEventHandler_andSelector_forEventClass_andEventIDSelector = mkSelector "setEventHandler:andSelector:forEventClass:andEventID:"

-- | @Selector@ for @removeEventHandlerForEventClass:andEventID:@
removeEventHandlerForEventClass_andEventIDSelector :: Selector '[CUInt, CUInt] ()
removeEventHandlerForEventClass_andEventIDSelector = mkSelector "removeEventHandlerForEventClass:andEventID:"

-- | @Selector@ for @dispatchRawAppleEvent:withRawReply:handlerRefCon:@
dispatchRawAppleEvent_withRawReply_handlerRefConSelector :: Selector '[Const RawId, RawId, RawId] CShort
dispatchRawAppleEvent_withRawReply_handlerRefConSelector = mkSelector "dispatchRawAppleEvent:withRawReply:handlerRefCon:"

-- | @Selector@ for @suspendCurrentAppleEvent@
suspendCurrentAppleEventSelector :: Selector '[] RawId
suspendCurrentAppleEventSelector = mkSelector "suspendCurrentAppleEvent"

-- | @Selector@ for @appleEventForSuspensionID:@
appleEventForSuspensionIDSelector :: Selector '[RawId] (Id NSAppleEventDescriptor)
appleEventForSuspensionIDSelector = mkSelector "appleEventForSuspensionID:"

-- | @Selector@ for @replyAppleEventForSuspensionID:@
replyAppleEventForSuspensionIDSelector :: Selector '[RawId] (Id NSAppleEventDescriptor)
replyAppleEventForSuspensionIDSelector = mkSelector "replyAppleEventForSuspensionID:"

-- | @Selector@ for @setCurrentAppleEventAndReplyEventWithSuspensionID:@
setCurrentAppleEventAndReplyEventWithSuspensionIDSelector :: Selector '[RawId] ()
setCurrentAppleEventAndReplyEventWithSuspensionIDSelector = mkSelector "setCurrentAppleEventAndReplyEventWithSuspensionID:"

-- | @Selector@ for @resumeWithSuspensionID:@
resumeWithSuspensionIDSelector :: Selector '[RawId] ()
resumeWithSuspensionIDSelector = mkSelector "resumeWithSuspensionID:"

-- | @Selector@ for @currentAppleEvent@
currentAppleEventSelector :: Selector '[] (Id NSAppleEventDescriptor)
currentAppleEventSelector = mkSelector "currentAppleEvent"

-- | @Selector@ for @currentReplyAppleEvent@
currentReplyAppleEventSelector :: Selector '[] (Id NSAppleEventDescriptor)
currentReplyAppleEventSelector = mkSelector "currentReplyAppleEvent"

