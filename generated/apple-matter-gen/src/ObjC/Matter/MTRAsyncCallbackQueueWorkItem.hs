{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAsyncCallbackQueueWorkItem@.
module ObjC.Matter.MTRAsyncCallbackQueueWorkItem
  ( MTRAsyncCallbackQueueWorkItem
  , IsMTRAsyncCallbackQueueWorkItem(..)
  , init_
  , new
  , initWithQueue
  , endWork
  , retryWork
  , readyHandler
  , setReadyHandler
  , cancelHandler
  , setCancelHandler
  , cancelHandlerSelector
  , endWorkSelector
  , initSelector
  , initWithQueueSelector
  , newSelector
  , readyHandlerSelector
  , retryWorkSelector
  , setCancelHandlerSelector
  , setReadyHandlerSelector


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
init_ :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO (Id MTRAsyncCallbackQueueWorkItem)
init_ mtrAsyncCallbackQueueWorkItem =
  sendOwnedMessage mtrAsyncCallbackQueueWorkItem initSelector

-- | @+ new@
new :: IO (Id MTRAsyncCallbackQueueWorkItem)
new  =
  do
    cls' <- getRequiredClass "MTRAsyncCallbackQueueWorkItem"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithQueue:@
initWithQueue :: (IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem, IsNSObject queue) => mtrAsyncCallbackQueueWorkItem -> queue -> IO (Id MTRAsyncCallbackQueueWorkItem)
initWithQueue mtrAsyncCallbackQueueWorkItem queue =
  sendOwnedMessage mtrAsyncCallbackQueueWorkItem initWithQueueSelector (toNSObject queue)

-- | @- endWork@
endWork :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO ()
endWork mtrAsyncCallbackQueueWorkItem =
  sendMessage mtrAsyncCallbackQueueWorkItem endWorkSelector

-- | @- retryWork@
retryWork :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO ()
retryWork mtrAsyncCallbackQueueWorkItem =
  sendMessage mtrAsyncCallbackQueueWorkItem retryWorkSelector

-- | @- readyHandler@
readyHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO (Ptr ())
readyHandler mtrAsyncCallbackQueueWorkItem =
  sendMessage mtrAsyncCallbackQueueWorkItem readyHandlerSelector

-- | @- setReadyHandler:@
setReadyHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> Ptr () -> IO ()
setReadyHandler mtrAsyncCallbackQueueWorkItem value =
  sendMessage mtrAsyncCallbackQueueWorkItem setReadyHandlerSelector value

-- | @- cancelHandler@
cancelHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> IO (Ptr ())
cancelHandler mtrAsyncCallbackQueueWorkItem =
  sendMessage mtrAsyncCallbackQueueWorkItem cancelHandlerSelector

-- | @- setCancelHandler:@
setCancelHandler :: IsMTRAsyncCallbackQueueWorkItem mtrAsyncCallbackQueueWorkItem => mtrAsyncCallbackQueueWorkItem -> Ptr () -> IO ()
setCancelHandler mtrAsyncCallbackQueueWorkItem value =
  sendMessage mtrAsyncCallbackQueueWorkItem setCancelHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRAsyncCallbackQueueWorkItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRAsyncCallbackQueueWorkItem)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithQueue:@
initWithQueueSelector :: Selector '[Id NSObject] (Id MTRAsyncCallbackQueueWorkItem)
initWithQueueSelector = mkSelector "initWithQueue:"

-- | @Selector@ for @endWork@
endWorkSelector :: Selector '[] ()
endWorkSelector = mkSelector "endWork"

-- | @Selector@ for @retryWork@
retryWorkSelector :: Selector '[] ()
retryWorkSelector = mkSelector "retryWork"

-- | @Selector@ for @readyHandler@
readyHandlerSelector :: Selector '[] (Ptr ())
readyHandlerSelector = mkSelector "readyHandler"

-- | @Selector@ for @setReadyHandler:@
setReadyHandlerSelector :: Selector '[Ptr ()] ()
setReadyHandlerSelector = mkSelector "setReadyHandler:"

-- | @Selector@ for @cancelHandler@
cancelHandlerSelector :: Selector '[] (Ptr ())
cancelHandlerSelector = mkSelector "cancelHandler"

-- | @Selector@ for @setCancelHandler:@
setCancelHandlerSelector :: Selector '[Ptr ()] ()
setCancelHandlerSelector = mkSelector "setCancelHandler:"

