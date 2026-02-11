{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCall@.
module ObjC.CallKit.CXCall
  ( CXCall
  , IsCXCall(..)
  , init_
  , isEqualToCall
  , uuid
  , outgoing
  , onHold
  , hasConnected
  , hasEnded
  , initSelector
  , isEqualToCallSelector
  , uuidSelector
  , outgoingSelector
  , onHoldSelector
  , hasConnectedSelector
  , hasEndedSelector


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

-- | @- init@
init_ :: IsCXCall cxCall => cxCall -> IO (Id CXCall)
init_ cxCall  =
  sendMsg cxCall (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- isEqualToCall:@
isEqualToCall :: (IsCXCall cxCall, IsCXCall call) => cxCall -> call -> IO Bool
isEqualToCall cxCall  call =
withObjCPtr call $ \raw_call ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCall (mkSelector "isEqualToCall:") retCULong [argPtr (castPtr raw_call :: Ptr ())]

-- | @- UUID@
uuid :: IsCXCall cxCall => cxCall -> IO (Id NSUUID)
uuid cxCall  =
  sendMsg cxCall (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- outgoing@
outgoing :: IsCXCall cxCall => cxCall -> IO Bool
outgoing cxCall  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCall (mkSelector "outgoing") retCULong []

-- | @- onHold@
onHold :: IsCXCall cxCall => cxCall -> IO Bool
onHold cxCall  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCall (mkSelector "onHold") retCULong []

-- | @- hasConnected@
hasConnected :: IsCXCall cxCall => cxCall -> IO Bool
hasConnected cxCall  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCall (mkSelector "hasConnected") retCULong []

-- | @- hasEnded@
hasEnded :: IsCXCall cxCall => cxCall -> IO Bool
hasEnded cxCall  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCall (mkSelector "hasEnded") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @isEqualToCall:@
isEqualToCallSelector :: Selector
isEqualToCallSelector = mkSelector "isEqualToCall:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @outgoing@
outgoingSelector :: Selector
outgoingSelector = mkSelector "outgoing"

-- | @Selector@ for @onHold@
onHoldSelector :: Selector
onHoldSelector = mkSelector "onHold"

-- | @Selector@ for @hasConnected@
hasConnectedSelector :: Selector
hasConnectedSelector = mkSelector "hasConnected"

-- | @Selector@ for @hasEnded@
hasEndedSelector :: Selector
hasEndedSelector = mkSelector "hasEnded"

