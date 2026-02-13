{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXSetMutedCallAction@.
module ObjC.CallKit.CXSetMutedCallAction
  ( CXSetMutedCallAction
  , IsCXSetMutedCallAction(..)
  , initWithCallUUID_muted
  , initWithCoder
  , initWithCallUUID
  , muted
  , setMuted
  , initWithCallUUIDSelector
  , initWithCallUUID_mutedSelector
  , initWithCoderSelector
  , mutedSelector
  , setMutedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:muted:@
initWithCallUUID_muted :: (IsCXSetMutedCallAction cxSetMutedCallAction, IsNSUUID callUUID) => cxSetMutedCallAction -> callUUID -> Bool -> IO (Id CXSetMutedCallAction)
initWithCallUUID_muted cxSetMutedCallAction callUUID muted =
  sendOwnedMessage cxSetMutedCallAction initWithCallUUID_mutedSelector (toNSUUID callUUID) muted

-- | @- initWithCoder:@
initWithCoder :: (IsCXSetMutedCallAction cxSetMutedCallAction, IsNSCoder aDecoder) => cxSetMutedCallAction -> aDecoder -> IO (Id CXSetMutedCallAction)
initWithCoder cxSetMutedCallAction aDecoder =
  sendOwnedMessage cxSetMutedCallAction initWithCoderSelector (toNSCoder aDecoder)

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXSetMutedCallAction cxSetMutedCallAction, IsNSUUID callUUID) => cxSetMutedCallAction -> callUUID -> IO (Id CXSetMutedCallAction)
initWithCallUUID cxSetMutedCallAction callUUID =
  sendOwnedMessage cxSetMutedCallAction initWithCallUUIDSelector (toNSUUID callUUID)

-- | @- muted@
muted :: IsCXSetMutedCallAction cxSetMutedCallAction => cxSetMutedCallAction -> IO Bool
muted cxSetMutedCallAction =
  sendMessage cxSetMutedCallAction mutedSelector

-- | @- setMuted:@
setMuted :: IsCXSetMutedCallAction cxSetMutedCallAction => cxSetMutedCallAction -> Bool -> IO ()
setMuted cxSetMutedCallAction value =
  sendMessage cxSetMutedCallAction setMutedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:muted:@
initWithCallUUID_mutedSelector :: Selector '[Id NSUUID, Bool] (Id CXSetMutedCallAction)
initWithCallUUID_mutedSelector = mkSelector "initWithCallUUID:muted:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXSetMutedCallAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector '[Id NSUUID] (Id CXSetMutedCallAction)
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @muted@
mutedSelector :: Selector '[] Bool
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector '[Bool] ()
setMutedSelector = mkSelector "setMuted:"

