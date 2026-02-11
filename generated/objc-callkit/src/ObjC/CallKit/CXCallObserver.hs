{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallObserver@.
module ObjC.CallKit.CXCallObserver
  ( CXCallObserver
  , IsCXCallObserver(..)
  , setDelegate_queue
  , calls
  , setDelegate_queueSelector
  , callsSelector


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

-- | Set delegate and optional queue for delegate callbacks to be performed on. A nil queue implies that delegate callbacks should happen on the main queue. The delegate is stored weakly
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsCXCallObserver cxCallObserver, IsNSObject queue) => cxCallObserver -> RawId -> queue -> IO ()
setDelegate_queue cxCallObserver  delegate queue =
withObjCPtr queue $ \raw_queue ->
    sendMsg cxCallObserver (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Retrieve the current call list, blocking on initial state retrieval if necessary
--
-- ObjC selector: @- calls@
calls :: IsCXCallObserver cxCallObserver => cxCallObserver -> IO (Id NSArray)
calls cxCallObserver  =
  sendMsg cxCallObserver (mkSelector "calls") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @calls@
callsSelector :: Selector
callsSelector = mkSelector "calls"

