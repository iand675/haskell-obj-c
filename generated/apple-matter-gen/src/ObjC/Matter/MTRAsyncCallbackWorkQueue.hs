{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAsyncCallbackWorkQueue@.
module ObjC.Matter.MTRAsyncCallbackWorkQueue
  ( MTRAsyncCallbackWorkQueue
  , IsMTRAsyncCallbackWorkQueue(..)
  , init_
  , new
  , initWithContext_queue
  , invalidate
  , enqueueWorkItem
  , enqueueWorkItemSelector
  , initSelector
  , initWithContext_queueSelector
  , invalidateSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue => mtrAsyncCallbackWorkQueue -> IO (Id MTRAsyncCallbackWorkQueue)
init_ mtrAsyncCallbackWorkQueue =
  sendOwnedMessage mtrAsyncCallbackWorkQueue initSelector

-- | @+ new@
new :: IO (Id MTRAsyncCallbackWorkQueue)
new  =
  do
    cls' <- getRequiredClass "MTRAsyncCallbackWorkQueue"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithContext:queue:@
initWithContext_queue :: (IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue, IsNSObject queue) => mtrAsyncCallbackWorkQueue -> RawId -> queue -> IO (Id MTRAsyncCallbackWorkQueue)
initWithContext_queue mtrAsyncCallbackWorkQueue context queue =
  sendOwnedMessage mtrAsyncCallbackWorkQueue initWithContext_queueSelector context (toNSObject queue)

-- | @- invalidate@
invalidate :: IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue => mtrAsyncCallbackWorkQueue -> IO ()
invalidate mtrAsyncCallbackWorkQueue =
  sendMessage mtrAsyncCallbackWorkQueue invalidateSelector

-- | @- enqueueWorkItem:@
enqueueWorkItem :: (IsMTRAsyncCallbackWorkQueue mtrAsyncCallbackWorkQueue, IsMTRAsyncCallbackQueueWorkItem item) => mtrAsyncCallbackWorkQueue -> item -> IO ()
enqueueWorkItem mtrAsyncCallbackWorkQueue item =
  sendMessage mtrAsyncCallbackWorkQueue enqueueWorkItemSelector (toMTRAsyncCallbackQueueWorkItem item)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRAsyncCallbackWorkQueue)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRAsyncCallbackWorkQueue)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithContext:queue:@
initWithContext_queueSelector :: Selector '[RawId, Id NSObject] (Id MTRAsyncCallbackWorkQueue)
initWithContext_queueSelector = mkSelector "initWithContext:queue:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @enqueueWorkItem:@
enqueueWorkItemSelector :: Selector '[Id MTRAsyncCallbackQueueWorkItem] ()
enqueueWorkItemSelector = mkSelector "enqueueWorkItem:"

