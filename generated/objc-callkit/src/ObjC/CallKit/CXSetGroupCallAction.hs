{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXSetGroupCallAction@.
module ObjC.CallKit.CXSetGroupCallAction
  ( CXSetGroupCallAction
  , IsCXSetGroupCallAction(..)
  , initWithCallUUID_callUUIDToGroupWith
  , initWithCoder
  , initWithCallUUID
  , callUUIDToGroupWith
  , setCallUUIDToGroupWith
  , initWithCallUUID_callUUIDToGroupWithSelector
  , initWithCoderSelector
  , initWithCallUUIDSelector
  , callUUIDToGroupWithSelector
  , setCallUUIDToGroupWithSelector


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

-- | @- initWithCallUUID:callUUIDToGroupWith:@
initWithCallUUID_callUUIDToGroupWith :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSUUID callUUID, IsNSUUID callUUIDToGroupWith) => cxSetGroupCallAction -> callUUID -> callUUIDToGroupWith -> IO (Id CXSetGroupCallAction)
initWithCallUUID_callUUIDToGroupWith cxSetGroupCallAction  callUUID callUUIDToGroupWith =
withObjCPtr callUUID $ \raw_callUUID ->
  withObjCPtr callUUIDToGroupWith $ \raw_callUUIDToGroupWith ->
      sendMsg cxSetGroupCallAction (mkSelector "initWithCallUUID:callUUIDToGroupWith:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ()), argPtr (castPtr raw_callUUIDToGroupWith :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSCoder aDecoder) => cxSetGroupCallAction -> aDecoder -> IO (Id CXSetGroupCallAction)
initWithCoder cxSetGroupCallAction  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg cxSetGroupCallAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSUUID callUUID) => cxSetGroupCallAction -> callUUID -> IO (Id CXSetGroupCallAction)
initWithCallUUID cxSetGroupCallAction  callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxSetGroupCallAction (mkSelector "initWithCallUUID:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ())] >>= ownedObject . castPtr

-- | The UUID of another call to group with.
--
-- - If the call for this action's UUID is already in a group, it should leave that group if necessary. - If nil, leave any group the call is currently in.
--
-- ObjC selector: @- callUUIDToGroupWith@
callUUIDToGroupWith :: IsCXSetGroupCallAction cxSetGroupCallAction => cxSetGroupCallAction -> IO (Id NSUUID)
callUUIDToGroupWith cxSetGroupCallAction  =
  sendMsg cxSetGroupCallAction (mkSelector "callUUIDToGroupWith") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The UUID of another call to group with.
--
-- - If the call for this action's UUID is already in a group, it should leave that group if necessary. - If nil, leave any group the call is currently in.
--
-- ObjC selector: @- setCallUUIDToGroupWith:@
setCallUUIDToGroupWith :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSUUID value) => cxSetGroupCallAction -> value -> IO ()
setCallUUIDToGroupWith cxSetGroupCallAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxSetGroupCallAction (mkSelector "setCallUUIDToGroupWith:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:callUUIDToGroupWith:@
initWithCallUUID_callUUIDToGroupWithSelector :: Selector
initWithCallUUID_callUUIDToGroupWithSelector = mkSelector "initWithCallUUID:callUUIDToGroupWith:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @callUUIDToGroupWith@
callUUIDToGroupWithSelector :: Selector
callUUIDToGroupWithSelector = mkSelector "callUUIDToGroupWith"

-- | @Selector@ for @setCallUUIDToGroupWith:@
setCallUUIDToGroupWithSelector :: Selector
setCallUUIDToGroupWithSelector = mkSelector "setCallUUIDToGroupWith:"

