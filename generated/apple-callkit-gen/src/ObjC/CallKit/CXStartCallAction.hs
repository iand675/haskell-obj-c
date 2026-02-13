{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXStartCallAction@.
module ObjC.CallKit.CXStartCallAction
  ( CXStartCallAction
  , IsCXStartCallAction(..)
  , initWithCallUUID_handle
  , initWithCoder
  , initWithCallUUID
  , fulfillWithDateStarted
  , handle
  , setHandle
  , contactIdentifier
  , setContactIdentifier
  , video
  , setVideo
  , contactIdentifierSelector
  , fulfillWithDateStartedSelector
  , handleSelector
  , initWithCallUUIDSelector
  , initWithCallUUID_handleSelector
  , initWithCoderSelector
  , setContactIdentifierSelector
  , setHandleSelector
  , setVideoSelector
  , videoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:handle:@
initWithCallUUID_handle :: (IsCXStartCallAction cxStartCallAction, IsNSUUID callUUID, IsCXHandle handle) => cxStartCallAction -> callUUID -> handle -> IO (Id CXStartCallAction)
initWithCallUUID_handle cxStartCallAction callUUID handle =
  sendOwnedMessage cxStartCallAction initWithCallUUID_handleSelector (toNSUUID callUUID) (toCXHandle handle)

-- | @- initWithCoder:@
initWithCoder :: (IsCXStartCallAction cxStartCallAction, IsNSCoder aDecoder) => cxStartCallAction -> aDecoder -> IO (Id CXStartCallAction)
initWithCoder cxStartCallAction aDecoder =
  sendOwnedMessage cxStartCallAction initWithCoderSelector (toNSCoder aDecoder)

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXStartCallAction cxStartCallAction, IsNSUUID callUUID) => cxStartCallAction -> callUUID -> IO (Id CXStartCallAction)
initWithCallUUID cxStartCallAction callUUID =
  sendOwnedMessage cxStartCallAction initWithCallUUIDSelector (toNSUUID callUUID)

-- | Normally, providers can just call -[CXAction fulfill] to indicate action fulfillment. Use this method to note a specific date that the call started if it is different from [NSDate date]. A call is considered started when its invitation has been sent to the remote callee.
--
-- ObjC selector: @- fulfillWithDateStarted:@
fulfillWithDateStarted :: (IsCXStartCallAction cxStartCallAction, IsNSDate dateStarted) => cxStartCallAction -> dateStarted -> IO ()
fulfillWithDateStarted cxStartCallAction dateStarted =
  sendMessage cxStartCallAction fulfillWithDateStartedSelector (toNSDate dateStarted)

-- | Handle for the party to call
--
-- ObjC selector: @- handle@
handle :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> IO (Id CXHandle)
handle cxStartCallAction =
  sendMessage cxStartCallAction handleSelector

-- | Handle for the party to call
--
-- ObjC selector: @- setHandle:@
setHandle :: (IsCXStartCallAction cxStartCallAction, IsCXHandle value) => cxStartCallAction -> value -> IO ()
setHandle cxStartCallAction value =
  sendMessage cxStartCallAction setHandleSelector (toCXHandle value)

-- | @- contactIdentifier@
contactIdentifier :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> IO (Id NSString)
contactIdentifier cxStartCallAction =
  sendMessage cxStartCallAction contactIdentifierSelector

-- | @- setContactIdentifier:@
setContactIdentifier :: (IsCXStartCallAction cxStartCallAction, IsNSString value) => cxStartCallAction -> value -> IO ()
setContactIdentifier cxStartCallAction value =
  sendMessage cxStartCallAction setContactIdentifierSelector (toNSString value)

-- | @- video@
video :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> IO Bool
video cxStartCallAction =
  sendMessage cxStartCallAction videoSelector

-- | @- setVideo:@
setVideo :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> Bool -> IO ()
setVideo cxStartCallAction value =
  sendMessage cxStartCallAction setVideoSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:handle:@
initWithCallUUID_handleSelector :: Selector '[Id NSUUID, Id CXHandle] (Id CXStartCallAction)
initWithCallUUID_handleSelector = mkSelector "initWithCallUUID:handle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXStartCallAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector '[Id NSUUID] (Id CXStartCallAction)
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @fulfillWithDateStarted:@
fulfillWithDateStartedSelector :: Selector '[Id NSDate] ()
fulfillWithDateStartedSelector = mkSelector "fulfillWithDateStarted:"

-- | @Selector@ for @handle@
handleSelector :: Selector '[] (Id CXHandle)
handleSelector = mkSelector "handle"

-- | @Selector@ for @setHandle:@
setHandleSelector :: Selector '[Id CXHandle] ()
setHandleSelector = mkSelector "setHandle:"

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector '[] (Id NSString)
contactIdentifierSelector = mkSelector "contactIdentifier"

-- | @Selector@ for @setContactIdentifier:@
setContactIdentifierSelector :: Selector '[Id NSString] ()
setContactIdentifierSelector = mkSelector "setContactIdentifier:"

-- | @Selector@ for @video@
videoSelector :: Selector '[] Bool
videoSelector = mkSelector "video"

-- | @Selector@ for @setVideo:@
setVideoSelector :: Selector '[Bool] ()
setVideoSelector = mkSelector "setVideo:"

