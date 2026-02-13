{-# LANGUAGE DataKinds #-}
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
  , hasConnectedSelector
  , hasEndedSelector
  , initSelector
  , isEqualToCallSelector
  , onHoldSelector
  , outgoingSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCXCall cxCall => cxCall -> IO (Id CXCall)
init_ cxCall =
  sendOwnedMessage cxCall initSelector

-- | @- isEqualToCall:@
isEqualToCall :: (IsCXCall cxCall, IsCXCall call) => cxCall -> call -> IO Bool
isEqualToCall cxCall call =
  sendMessage cxCall isEqualToCallSelector (toCXCall call)

-- | @- UUID@
uuid :: IsCXCall cxCall => cxCall -> IO (Id NSUUID)
uuid cxCall =
  sendMessage cxCall uuidSelector

-- | @- outgoing@
outgoing :: IsCXCall cxCall => cxCall -> IO Bool
outgoing cxCall =
  sendMessage cxCall outgoingSelector

-- | @- onHold@
onHold :: IsCXCall cxCall => cxCall -> IO Bool
onHold cxCall =
  sendMessage cxCall onHoldSelector

-- | @- hasConnected@
hasConnected :: IsCXCall cxCall => cxCall -> IO Bool
hasConnected cxCall =
  sendMessage cxCall hasConnectedSelector

-- | @- hasEnded@
hasEnded :: IsCXCall cxCall => cxCall -> IO Bool
hasEnded cxCall =
  sendMessage cxCall hasEndedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXCall)
initSelector = mkSelector "init"

-- | @Selector@ for @isEqualToCall:@
isEqualToCallSelector :: Selector '[Id CXCall] Bool
isEqualToCallSelector = mkSelector "isEqualToCall:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @outgoing@
outgoingSelector :: Selector '[] Bool
outgoingSelector = mkSelector "outgoing"

-- | @Selector@ for @onHold@
onHoldSelector :: Selector '[] Bool
onHoldSelector = mkSelector "onHold"

-- | @Selector@ for @hasConnected@
hasConnectedSelector :: Selector '[] Bool
hasConnectedSelector = mkSelector "hasConnected"

-- | @Selector@ for @hasEnded@
hasEndedSelector :: Selector '[] Bool
hasEndedSelector = mkSelector "hasEnded"

