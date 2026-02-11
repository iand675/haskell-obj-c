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
  , initWithCallUUID_handleSelector
  , initWithCoderSelector
  , initWithCallUUIDSelector
  , fulfillWithDateStartedSelector
  , handleSelector
  , setHandleSelector
  , contactIdentifierSelector
  , setContactIdentifierSelector
  , videoSelector
  , setVideoSelector


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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:handle:@
initWithCallUUID_handle :: (IsCXStartCallAction cxStartCallAction, IsNSUUID callUUID, IsCXHandle handle) => cxStartCallAction -> callUUID -> handle -> IO (Id CXStartCallAction)
initWithCallUUID_handle cxStartCallAction  callUUID handle =
withObjCPtr callUUID $ \raw_callUUID ->
  withObjCPtr handle $ \raw_handle ->
      sendMsg cxStartCallAction (mkSelector "initWithCallUUID:handle:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ()), argPtr (castPtr raw_handle :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXStartCallAction cxStartCallAction, IsNSCoder aDecoder) => cxStartCallAction -> aDecoder -> IO (Id CXStartCallAction)
initWithCoder cxStartCallAction  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg cxStartCallAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXStartCallAction cxStartCallAction, IsNSUUID callUUID) => cxStartCallAction -> callUUID -> IO (Id CXStartCallAction)
initWithCallUUID cxStartCallAction  callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxStartCallAction (mkSelector "initWithCallUUID:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ())] >>= ownedObject . castPtr

-- | Normally, providers can just call -[CXAction fulfill] to indicate action fulfillment. Use this method to note a specific date that the call started if it is different from [NSDate date]. A call is considered started when its invitation has been sent to the remote callee.
--
-- ObjC selector: @- fulfillWithDateStarted:@
fulfillWithDateStarted :: (IsCXStartCallAction cxStartCallAction, IsNSDate dateStarted) => cxStartCallAction -> dateStarted -> IO ()
fulfillWithDateStarted cxStartCallAction  dateStarted =
withObjCPtr dateStarted $ \raw_dateStarted ->
    sendMsg cxStartCallAction (mkSelector "fulfillWithDateStarted:") retVoid [argPtr (castPtr raw_dateStarted :: Ptr ())]

-- | Handle for the party to call
--
-- ObjC selector: @- handle@
handle :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> IO (Id CXHandle)
handle cxStartCallAction  =
  sendMsg cxStartCallAction (mkSelector "handle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Handle for the party to call
--
-- ObjC selector: @- setHandle:@
setHandle :: (IsCXStartCallAction cxStartCallAction, IsCXHandle value) => cxStartCallAction -> value -> IO ()
setHandle cxStartCallAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxStartCallAction (mkSelector "setHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contactIdentifier@
contactIdentifier :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> IO (Id NSString)
contactIdentifier cxStartCallAction  =
  sendMsg cxStartCallAction (mkSelector "contactIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContactIdentifier:@
setContactIdentifier :: (IsCXStartCallAction cxStartCallAction, IsNSString value) => cxStartCallAction -> value -> IO ()
setContactIdentifier cxStartCallAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxStartCallAction (mkSelector "setContactIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- video@
video :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> IO Bool
video cxStartCallAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxStartCallAction (mkSelector "video") retCULong []

-- | @- setVideo:@
setVideo :: IsCXStartCallAction cxStartCallAction => cxStartCallAction -> Bool -> IO ()
setVideo cxStartCallAction  value =
  sendMsg cxStartCallAction (mkSelector "setVideo:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:handle:@
initWithCallUUID_handleSelector :: Selector
initWithCallUUID_handleSelector = mkSelector "initWithCallUUID:handle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @fulfillWithDateStarted:@
fulfillWithDateStartedSelector :: Selector
fulfillWithDateStartedSelector = mkSelector "fulfillWithDateStarted:"

-- | @Selector@ for @handle@
handleSelector :: Selector
handleSelector = mkSelector "handle"

-- | @Selector@ for @setHandle:@
setHandleSelector :: Selector
setHandleSelector = mkSelector "setHandle:"

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector
contactIdentifierSelector = mkSelector "contactIdentifier"

-- | @Selector@ for @setContactIdentifier:@
setContactIdentifierSelector :: Selector
setContactIdentifierSelector = mkSelector "setContactIdentifier:"

-- | @Selector@ for @video@
videoSelector :: Selector
videoSelector = mkSelector "video"

-- | @Selector@ for @setVideo:@
setVideoSelector :: Selector
setVideoSelector = mkSelector "setVideo:"

