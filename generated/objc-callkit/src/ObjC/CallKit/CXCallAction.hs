{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallAction@.
module ObjC.CallKit.CXCallAction
  ( CXCallAction
  , IsCXCallAction(..)
  , initWithCallUUID
  , initWithCoder
  , init_
  , callUUID
  , initWithCallUUIDSelector
  , initWithCoderSelector
  , initSelector
  , callUUIDSelector


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

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXCallAction cxCallAction, IsNSUUID callUUID) => cxCallAction -> callUUID -> IO (Id CXCallAction)
initWithCallUUID cxCallAction  callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxCallAction (mkSelector "initWithCallUUID:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXCallAction cxCallAction, IsNSCoder aDecoder) => cxCallAction -> aDecoder -> IO (Id CXCallAction)
initWithCoder cxCallAction  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg cxCallAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCXCallAction cxCallAction => cxCallAction -> IO (Id CXCallAction)
init_ cxCallAction  =
  sendMsg cxCallAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- callUUID@
callUUID :: IsCXCallAction cxCallAction => cxCallAction -> IO (Id NSUUID)
callUUID cxCallAction  =
  sendMsg cxCallAction (mkSelector "callUUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @callUUID@
callUUIDSelector :: Selector
callUUIDSelector = mkSelector "callUUID"

