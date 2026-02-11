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
  , sharedAppleEventManagerSelector
  , setEventHandler_andSelector_forEventClass_andEventIDSelector
  , removeEventHandlerForEventClass_andEventIDSelector
  , dispatchRawAppleEvent_withRawReply_handlerRefConSelector
  , suspendCurrentAppleEventSelector
  , appleEventForSuspensionIDSelector
  , replyAppleEventForSuspensionIDSelector
  , setCurrentAppleEventAndReplyEventWithSuspensionIDSelector
  , resumeWithSuspensionIDSelector
  , currentAppleEventSelector
  , currentReplyAppleEventSelector


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

-- | @+ sharedAppleEventManager@
sharedAppleEventManager :: IO (Id NSAppleEventManager)
sharedAppleEventManager  =
  do
    cls' <- getRequiredClass "NSAppleEventManager"
    sendClassMsg cls' (mkSelector "sharedAppleEventManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEventHandler:andSelector:forEventClass:andEventID:@
setEventHandler_andSelector_forEventClass_andEventID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> Selector -> CUInt -> CUInt -> IO ()
setEventHandler_andSelector_forEventClass_andEventID nsAppleEventManager  handler handleEventSelector eventClass eventID =
    sendMsg nsAppleEventManager (mkSelector "setEventHandler:andSelector:forEventClass:andEventID:") retVoid [argPtr (castPtr (unRawId handler) :: Ptr ()), argPtr (unSelector handleEventSelector), argCUInt eventClass, argCUInt eventID]

-- | @- removeEventHandlerForEventClass:andEventID:@
removeEventHandlerForEventClass_andEventID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> CUInt -> CUInt -> IO ()
removeEventHandlerForEventClass_andEventID nsAppleEventManager  eventClass eventID =
    sendMsg nsAppleEventManager (mkSelector "removeEventHandlerForEventClass:andEventID:") retVoid [argCUInt eventClass, argCUInt eventID]

-- | @- dispatchRawAppleEvent:withRawReply:handlerRefCon:@
dispatchRawAppleEvent_withRawReply_handlerRefCon :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> Const RawId -> RawId -> RawId -> IO CShort
dispatchRawAppleEvent_withRawReply_handlerRefCon nsAppleEventManager  theAppleEvent theReply handlerRefCon =
    fmap fromIntegral $ sendMsg nsAppleEventManager (mkSelector "dispatchRawAppleEvent:withRawReply:handlerRefCon:") retCInt [argPtr (castPtr (unRawId (unConst theAppleEvent)) :: Ptr ()), argPtr (castPtr (unRawId theReply) :: Ptr ()), argPtr (castPtr (unRawId handlerRefCon) :: Ptr ())]

-- | @- suspendCurrentAppleEvent@
suspendCurrentAppleEvent :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> IO RawId
suspendCurrentAppleEvent nsAppleEventManager  =
    fmap (RawId . castPtr) $ sendMsg nsAppleEventManager (mkSelector "suspendCurrentAppleEvent") (retPtr retVoid) []

-- | @- appleEventForSuspensionID:@
appleEventForSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO (Id NSAppleEventDescriptor)
appleEventForSuspensionID nsAppleEventManager  suspensionID =
    sendMsg nsAppleEventManager (mkSelector "appleEventForSuspensionID:") (retPtr retVoid) [argPtr (castPtr (unRawId suspensionID) :: Ptr ())] >>= retainedObject . castPtr

-- | @- replyAppleEventForSuspensionID:@
replyAppleEventForSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO (Id NSAppleEventDescriptor)
replyAppleEventForSuspensionID nsAppleEventManager  suspensionID =
    sendMsg nsAppleEventManager (mkSelector "replyAppleEventForSuspensionID:") (retPtr retVoid) [argPtr (castPtr (unRawId suspensionID) :: Ptr ())] >>= retainedObject . castPtr

-- | @- setCurrentAppleEventAndReplyEventWithSuspensionID:@
setCurrentAppleEventAndReplyEventWithSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO ()
setCurrentAppleEventAndReplyEventWithSuspensionID nsAppleEventManager  suspensionID =
    sendMsg nsAppleEventManager (mkSelector "setCurrentAppleEventAndReplyEventWithSuspensionID:") retVoid [argPtr (castPtr (unRawId suspensionID) :: Ptr ())]

-- | @- resumeWithSuspensionID:@
resumeWithSuspensionID :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> RawId -> IO ()
resumeWithSuspensionID nsAppleEventManager  suspensionID =
    sendMsg nsAppleEventManager (mkSelector "resumeWithSuspensionID:") retVoid [argPtr (castPtr (unRawId suspensionID) :: Ptr ())]

-- | @- currentAppleEvent@
currentAppleEvent :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> IO (Id NSAppleEventDescriptor)
currentAppleEvent nsAppleEventManager  =
    sendMsg nsAppleEventManager (mkSelector "currentAppleEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentReplyAppleEvent@
currentReplyAppleEvent :: IsNSAppleEventManager nsAppleEventManager => nsAppleEventManager -> IO (Id NSAppleEventDescriptor)
currentReplyAppleEvent nsAppleEventManager  =
    sendMsg nsAppleEventManager (mkSelector "currentReplyAppleEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAppleEventManager@
sharedAppleEventManagerSelector :: Selector
sharedAppleEventManagerSelector = mkSelector "sharedAppleEventManager"

-- | @Selector@ for @setEventHandler:andSelector:forEventClass:andEventID:@
setEventHandler_andSelector_forEventClass_andEventIDSelector :: Selector
setEventHandler_andSelector_forEventClass_andEventIDSelector = mkSelector "setEventHandler:andSelector:forEventClass:andEventID:"

-- | @Selector@ for @removeEventHandlerForEventClass:andEventID:@
removeEventHandlerForEventClass_andEventIDSelector :: Selector
removeEventHandlerForEventClass_andEventIDSelector = mkSelector "removeEventHandlerForEventClass:andEventID:"

-- | @Selector@ for @dispatchRawAppleEvent:withRawReply:handlerRefCon:@
dispatchRawAppleEvent_withRawReply_handlerRefConSelector :: Selector
dispatchRawAppleEvent_withRawReply_handlerRefConSelector = mkSelector "dispatchRawAppleEvent:withRawReply:handlerRefCon:"

-- | @Selector@ for @suspendCurrentAppleEvent@
suspendCurrentAppleEventSelector :: Selector
suspendCurrentAppleEventSelector = mkSelector "suspendCurrentAppleEvent"

-- | @Selector@ for @appleEventForSuspensionID:@
appleEventForSuspensionIDSelector :: Selector
appleEventForSuspensionIDSelector = mkSelector "appleEventForSuspensionID:"

-- | @Selector@ for @replyAppleEventForSuspensionID:@
replyAppleEventForSuspensionIDSelector :: Selector
replyAppleEventForSuspensionIDSelector = mkSelector "replyAppleEventForSuspensionID:"

-- | @Selector@ for @setCurrentAppleEventAndReplyEventWithSuspensionID:@
setCurrentAppleEventAndReplyEventWithSuspensionIDSelector :: Selector
setCurrentAppleEventAndReplyEventWithSuspensionIDSelector = mkSelector "setCurrentAppleEventAndReplyEventWithSuspensionID:"

-- | @Selector@ for @resumeWithSuspensionID:@
resumeWithSuspensionIDSelector :: Selector
resumeWithSuspensionIDSelector = mkSelector "resumeWithSuspensionID:"

-- | @Selector@ for @currentAppleEvent@
currentAppleEventSelector :: Selector
currentAppleEventSelector = mkSelector "currentAppleEvent"

-- | @Selector@ for @currentReplyAppleEvent@
currentReplyAppleEventSelector :: Selector
currentReplyAppleEventSelector = mkSelector "currentReplyAppleEvent"

