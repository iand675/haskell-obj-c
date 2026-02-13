{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXSetHeldCallAction@.
module ObjC.CallKit.CXSetHeldCallAction
  ( CXSetHeldCallAction
  , IsCXSetHeldCallAction(..)
  , initWithCallUUID_onHold
  , initWithCoder
  , initWithCallUUID
  , onHold
  , setOnHold
  , initWithCallUUIDSelector
  , initWithCallUUID_onHoldSelector
  , initWithCoderSelector
  , onHoldSelector
  , setOnHoldSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:onHold:@
initWithCallUUID_onHold :: (IsCXSetHeldCallAction cxSetHeldCallAction, IsNSUUID callUUID) => cxSetHeldCallAction -> callUUID -> Bool -> IO (Id CXSetHeldCallAction)
initWithCallUUID_onHold cxSetHeldCallAction callUUID onHold =
  sendOwnedMessage cxSetHeldCallAction initWithCallUUID_onHoldSelector (toNSUUID callUUID) onHold

-- | @- initWithCoder:@
initWithCoder :: (IsCXSetHeldCallAction cxSetHeldCallAction, IsNSCoder aDecoder) => cxSetHeldCallAction -> aDecoder -> IO (Id CXSetHeldCallAction)
initWithCoder cxSetHeldCallAction aDecoder =
  sendOwnedMessage cxSetHeldCallAction initWithCoderSelector (toNSCoder aDecoder)

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXSetHeldCallAction cxSetHeldCallAction, IsNSUUID callUUID) => cxSetHeldCallAction -> callUUID -> IO (Id CXSetHeldCallAction)
initWithCallUUID cxSetHeldCallAction callUUID =
  sendOwnedMessage cxSetHeldCallAction initWithCallUUIDSelector (toNSUUID callUUID)

-- | @- onHold@
onHold :: IsCXSetHeldCallAction cxSetHeldCallAction => cxSetHeldCallAction -> IO Bool
onHold cxSetHeldCallAction =
  sendMessage cxSetHeldCallAction onHoldSelector

-- | @- setOnHold:@
setOnHold :: IsCXSetHeldCallAction cxSetHeldCallAction => cxSetHeldCallAction -> Bool -> IO ()
setOnHold cxSetHeldCallAction value =
  sendMessage cxSetHeldCallAction setOnHoldSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:onHold:@
initWithCallUUID_onHoldSelector :: Selector '[Id NSUUID, Bool] (Id CXSetHeldCallAction)
initWithCallUUID_onHoldSelector = mkSelector "initWithCallUUID:onHold:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXSetHeldCallAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector '[Id NSUUID] (Id CXSetHeldCallAction)
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @onHold@
onHoldSelector :: Selector '[] Bool
onHoldSelector = mkSelector "onHold"

-- | @Selector@ for @setOnHold:@
setOnHoldSelector :: Selector '[Bool] ()
setOnHoldSelector = mkSelector "setOnHold:"

