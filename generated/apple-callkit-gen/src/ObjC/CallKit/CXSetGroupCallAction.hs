{-# LANGUAGE DataKinds #-}
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
  , callUUIDToGroupWithSelector
  , initWithCallUUIDSelector
  , initWithCallUUID_callUUIDToGroupWithSelector
  , initWithCoderSelector
  , setCallUUIDToGroupWithSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:callUUIDToGroupWith:@
initWithCallUUID_callUUIDToGroupWith :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSUUID callUUID, IsNSUUID callUUIDToGroupWith) => cxSetGroupCallAction -> callUUID -> callUUIDToGroupWith -> IO (Id CXSetGroupCallAction)
initWithCallUUID_callUUIDToGroupWith cxSetGroupCallAction callUUID callUUIDToGroupWith =
  sendOwnedMessage cxSetGroupCallAction initWithCallUUID_callUUIDToGroupWithSelector (toNSUUID callUUID) (toNSUUID callUUIDToGroupWith)

-- | @- initWithCoder:@
initWithCoder :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSCoder aDecoder) => cxSetGroupCallAction -> aDecoder -> IO (Id CXSetGroupCallAction)
initWithCoder cxSetGroupCallAction aDecoder =
  sendOwnedMessage cxSetGroupCallAction initWithCoderSelector (toNSCoder aDecoder)

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSUUID callUUID) => cxSetGroupCallAction -> callUUID -> IO (Id CXSetGroupCallAction)
initWithCallUUID cxSetGroupCallAction callUUID =
  sendOwnedMessage cxSetGroupCallAction initWithCallUUIDSelector (toNSUUID callUUID)

-- | The UUID of another call to group with.
--
-- - If the call for this action's UUID is already in a group, it should leave that group if necessary. - If nil, leave any group the call is currently in.
--
-- ObjC selector: @- callUUIDToGroupWith@
callUUIDToGroupWith :: IsCXSetGroupCallAction cxSetGroupCallAction => cxSetGroupCallAction -> IO (Id NSUUID)
callUUIDToGroupWith cxSetGroupCallAction =
  sendMessage cxSetGroupCallAction callUUIDToGroupWithSelector

-- | The UUID of another call to group with.
--
-- - If the call for this action's UUID is already in a group, it should leave that group if necessary. - If nil, leave any group the call is currently in.
--
-- ObjC selector: @- setCallUUIDToGroupWith:@
setCallUUIDToGroupWith :: (IsCXSetGroupCallAction cxSetGroupCallAction, IsNSUUID value) => cxSetGroupCallAction -> value -> IO ()
setCallUUIDToGroupWith cxSetGroupCallAction value =
  sendMessage cxSetGroupCallAction setCallUUIDToGroupWithSelector (toNSUUID value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:callUUIDToGroupWith:@
initWithCallUUID_callUUIDToGroupWithSelector :: Selector '[Id NSUUID, Id NSUUID] (Id CXSetGroupCallAction)
initWithCallUUID_callUUIDToGroupWithSelector = mkSelector "initWithCallUUID:callUUIDToGroupWith:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXSetGroupCallAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector '[Id NSUUID] (Id CXSetGroupCallAction)
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @callUUIDToGroupWith@
callUUIDToGroupWithSelector :: Selector '[] (Id NSUUID)
callUUIDToGroupWithSelector = mkSelector "callUUIDToGroupWith"

-- | @Selector@ for @setCallUUIDToGroupWith:@
setCallUUIDToGroupWithSelector :: Selector '[Id NSUUID] ()
setCallUUIDToGroupWithSelector = mkSelector "setCallUUIDToGroupWith:"

