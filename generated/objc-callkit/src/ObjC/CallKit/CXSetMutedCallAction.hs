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
  , initWithCallUUID_mutedSelector
  , initWithCoderSelector
  , initWithCallUUIDSelector
  , mutedSelector
  , setMutedSelector


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

-- | @- initWithCallUUID:muted:@
initWithCallUUID_muted :: (IsCXSetMutedCallAction cxSetMutedCallAction, IsNSUUID callUUID) => cxSetMutedCallAction -> callUUID -> Bool -> IO (Id CXSetMutedCallAction)
initWithCallUUID_muted cxSetMutedCallAction  callUUID muted =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxSetMutedCallAction (mkSelector "initWithCallUUID:muted:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ()), argCULong (if muted then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXSetMutedCallAction cxSetMutedCallAction, IsNSCoder aDecoder) => cxSetMutedCallAction -> aDecoder -> IO (Id CXSetMutedCallAction)
initWithCoder cxSetMutedCallAction  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg cxSetMutedCallAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXSetMutedCallAction cxSetMutedCallAction, IsNSUUID callUUID) => cxSetMutedCallAction -> callUUID -> IO (Id CXSetMutedCallAction)
initWithCallUUID cxSetMutedCallAction  callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxSetMutedCallAction (mkSelector "initWithCallUUID:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ())] >>= ownedObject . castPtr

-- | @- muted@
muted :: IsCXSetMutedCallAction cxSetMutedCallAction => cxSetMutedCallAction -> IO Bool
muted cxSetMutedCallAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxSetMutedCallAction (mkSelector "muted") retCULong []

-- | @- setMuted:@
setMuted :: IsCXSetMutedCallAction cxSetMutedCallAction => cxSetMutedCallAction -> Bool -> IO ()
setMuted cxSetMutedCallAction  value =
  sendMsg cxSetMutedCallAction (mkSelector "setMuted:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:muted:@
initWithCallUUID_mutedSelector :: Selector
initWithCallUUID_mutedSelector = mkSelector "initWithCallUUID:muted:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @muted@
mutedSelector :: Selector
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector
setMutedSelector = mkSelector "setMuted:"

