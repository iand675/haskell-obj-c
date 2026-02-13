{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLSharedEventListener
--
-- This class provides a simple interface for handling the dispatching of MTLSharedEvent notifications from Metal.
--
-- Generated bindings for @MTLSharedEventListener@.
module ObjC.Metal.MTLSharedEventListener
  ( MTLSharedEventListener
  , IsMTLSharedEventListener(..)
  , init_
  , initWithDispatchQueue
  , sharedListener
  , dispatchQueue
  , dispatchQueueSelector
  , initSelector
  , initWithDispatchQueueSelector
  , sharedListenerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTLSharedEventListener mtlSharedEventListener => mtlSharedEventListener -> IO (Id MTLSharedEventListener)
init_ mtlSharedEventListener =
  sendOwnedMessage mtlSharedEventListener initSelector

-- | @- initWithDispatchQueue:@
initWithDispatchQueue :: (IsMTLSharedEventListener mtlSharedEventListener, IsNSObject dispatchQueue) => mtlSharedEventListener -> dispatchQueue -> IO (Id MTLSharedEventListener)
initWithDispatchQueue mtlSharedEventListener dispatchQueue =
  sendOwnedMessage mtlSharedEventListener initWithDispatchQueueSelector (toNSObject dispatchQueue)

-- | @+ sharedListener@
sharedListener :: IO (Id MTLSharedEventListener)
sharedListener  =
  do
    cls' <- getRequiredClass "MTLSharedEventListener"
    sendClassMessage cls' sharedListenerSelector

-- | @- dispatchQueue@
dispatchQueue :: IsMTLSharedEventListener mtlSharedEventListener => mtlSharedEventListener -> IO (Id NSObject)
dispatchQueue mtlSharedEventListener =
  sendMessage mtlSharedEventListener dispatchQueueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTLSharedEventListener)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDispatchQueue:@
initWithDispatchQueueSelector :: Selector '[Id NSObject] (Id MTLSharedEventListener)
initWithDispatchQueueSelector = mkSelector "initWithDispatchQueue:"

-- | @Selector@ for @sharedListener@
sharedListenerSelector :: Selector '[] (Id MTLSharedEventListener)
sharedListenerSelector = mkSelector "sharedListener"

-- | @Selector@ for @dispatchQueue@
dispatchQueueSelector :: Selector '[] (Id NSObject)
dispatchQueueSelector = mkSelector "dispatchQueue"

