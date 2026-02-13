{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallObserver@.
module ObjC.CallKit.CXCallObserver
  ( CXCallObserver
  , IsCXCallObserver(..)
  , setDelegate_queue
  , calls
  , callsSelector
  , setDelegate_queueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Set delegate and optional queue for delegate callbacks to be performed on. A nil queue implies that delegate callbacks should happen on the main queue. The delegate is stored weakly
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsCXCallObserver cxCallObserver, IsNSObject queue) => cxCallObserver -> RawId -> queue -> IO ()
setDelegate_queue cxCallObserver delegate queue =
  sendMessage cxCallObserver setDelegate_queueSelector delegate (toNSObject queue)

-- | Retrieve the current call list, blocking on initial state retrieval if necessary
--
-- ObjC selector: @- calls@
calls :: IsCXCallObserver cxCallObserver => cxCallObserver -> IO (Id NSArray)
calls cxCallObserver =
  sendMessage cxCallObserver callsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @calls@
callsSelector :: Selector '[] (Id NSArray)
callsSelector = mkSelector "calls"

