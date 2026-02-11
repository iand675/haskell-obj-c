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
  , initSelector
  , initWithDispatchQueueSelector
  , sharedListenerSelector
  , dispatchQueueSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTLSharedEventListener mtlSharedEventListener => mtlSharedEventListener -> IO (Id MTLSharedEventListener)
init_ mtlSharedEventListener  =
  sendMsg mtlSharedEventListener (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDispatchQueue:@
initWithDispatchQueue :: (IsMTLSharedEventListener mtlSharedEventListener, IsNSObject dispatchQueue) => mtlSharedEventListener -> dispatchQueue -> IO (Id MTLSharedEventListener)
initWithDispatchQueue mtlSharedEventListener  dispatchQueue =
withObjCPtr dispatchQueue $ \raw_dispatchQueue ->
    sendMsg mtlSharedEventListener (mkSelector "initWithDispatchQueue:") (retPtr retVoid) [argPtr (castPtr raw_dispatchQueue :: Ptr ())] >>= ownedObject . castPtr

-- | @+ sharedListener@
sharedListener :: IO (Id MTLSharedEventListener)
sharedListener  =
  do
    cls' <- getRequiredClass "MTLSharedEventListener"
    sendClassMsg cls' (mkSelector "sharedListener") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dispatchQueue@
dispatchQueue :: IsMTLSharedEventListener mtlSharedEventListener => mtlSharedEventListener -> IO (Id NSObject)
dispatchQueue mtlSharedEventListener  =
  sendMsg mtlSharedEventListener (mkSelector "dispatchQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDispatchQueue:@
initWithDispatchQueueSelector :: Selector
initWithDispatchQueueSelector = mkSelector "initWithDispatchQueue:"

-- | @Selector@ for @sharedListener@
sharedListenerSelector :: Selector
sharedListenerSelector = mkSelector "sharedListener"

-- | @Selector@ for @dispatchQueue@
dispatchQueueSelector :: Selector
dispatchQueueSelector = mkSelector "dispatchQueue"

