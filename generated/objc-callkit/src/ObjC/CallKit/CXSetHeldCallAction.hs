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
  , initWithCallUUID_onHoldSelector
  , initWithCoderSelector
  , initWithCallUUIDSelector
  , onHoldSelector
  , setOnHoldSelector


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

-- | @- initWithCallUUID:onHold:@
initWithCallUUID_onHold :: (IsCXSetHeldCallAction cxSetHeldCallAction, IsNSUUID callUUID) => cxSetHeldCallAction -> callUUID -> Bool -> IO (Id CXSetHeldCallAction)
initWithCallUUID_onHold cxSetHeldCallAction  callUUID onHold =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxSetHeldCallAction (mkSelector "initWithCallUUID:onHold:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ()), argCULong (if onHold then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXSetHeldCallAction cxSetHeldCallAction, IsNSCoder aDecoder) => cxSetHeldCallAction -> aDecoder -> IO (Id CXSetHeldCallAction)
initWithCoder cxSetHeldCallAction  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg cxSetHeldCallAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXSetHeldCallAction cxSetHeldCallAction, IsNSUUID callUUID) => cxSetHeldCallAction -> callUUID -> IO (Id CXSetHeldCallAction)
initWithCallUUID cxSetHeldCallAction  callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxSetHeldCallAction (mkSelector "initWithCallUUID:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ())] >>= ownedObject . castPtr

-- | @- onHold@
onHold :: IsCXSetHeldCallAction cxSetHeldCallAction => cxSetHeldCallAction -> IO Bool
onHold cxSetHeldCallAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxSetHeldCallAction (mkSelector "onHold") retCULong []

-- | @- setOnHold:@
setOnHold :: IsCXSetHeldCallAction cxSetHeldCallAction => cxSetHeldCallAction -> Bool -> IO ()
setOnHold cxSetHeldCallAction  value =
  sendMsg cxSetHeldCallAction (mkSelector "setOnHold:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:onHold:@
initWithCallUUID_onHoldSelector :: Selector
initWithCallUUID_onHoldSelector = mkSelector "initWithCallUUID:onHold:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @onHold@
onHoldSelector :: Selector
onHoldSelector = mkSelector "onHold"

-- | @Selector@ for @setOnHold:@
setOnHoldSelector :: Selector
setOnHoldSelector = mkSelector "setOnHold:"

