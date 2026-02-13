{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKTurnBasedEventHandler@.
module ObjC.GameKit.GKTurnBasedEventHandler
  ( GKTurnBasedEventHandler
  , IsGKTurnBasedEventHandler(..)
  , sharedTurnBasedEventHandler
  , delegate
  , setDelegate
  , delegateSelector
  , setDelegateSelector
  , sharedTurnBasedEventHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedTurnBasedEventHandler@
sharedTurnBasedEventHandler :: IO (Id GKTurnBasedEventHandler)
sharedTurnBasedEventHandler  =
  do
    cls' <- getRequiredClass "GKTurnBasedEventHandler"
    sendClassMessage cls' sharedTurnBasedEventHandlerSelector

-- | @- delegate@
delegate :: IsGKTurnBasedEventHandler gkTurnBasedEventHandler => gkTurnBasedEventHandler -> IO (Id NSObject)
delegate gkTurnBasedEventHandler =
  sendMessage gkTurnBasedEventHandler delegateSelector

-- | @- setDelegate:@
setDelegate :: (IsGKTurnBasedEventHandler gkTurnBasedEventHandler, IsNSObject value) => gkTurnBasedEventHandler -> value -> IO ()
setDelegate gkTurnBasedEventHandler value =
  sendMessage gkTurnBasedEventHandler setDelegateSelector (toNSObject value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedTurnBasedEventHandler@
sharedTurnBasedEventHandlerSelector :: Selector '[] (Id GKTurnBasedEventHandler)
sharedTurnBasedEventHandlerSelector = mkSelector "sharedTurnBasedEventHandler"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] (Id NSObject)
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[Id NSObject] ()
setDelegateSelector = mkSelector "setDelegate:"

