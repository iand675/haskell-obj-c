{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterStateChangedEvent@.
module ObjC.Matter.MTRActionsClusterStateChangedEvent
  ( MTRActionsClusterStateChangedEvent
  , IsMTRActionsClusterStateChangedEvent(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , newState
  , setNewState
  , actionIDSelector
  , invokeIDSelector
  , newStateSelector
  , setActionIDSelector
  , setInvokeIDSelector
  , setNewStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- actionID@
actionID :: IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent => mtrActionsClusterStateChangedEvent -> IO (Id NSNumber)
actionID mtrActionsClusterStateChangedEvent =
  sendMessage mtrActionsClusterStateChangedEvent actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent, IsNSNumber value) => mtrActionsClusterStateChangedEvent -> value -> IO ()
setActionID mtrActionsClusterStateChangedEvent value =
  sendMessage mtrActionsClusterStateChangedEvent setActionIDSelector (toNSNumber value)

-- | @- invokeID@
invokeID :: IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent => mtrActionsClusterStateChangedEvent -> IO (Id NSNumber)
invokeID mtrActionsClusterStateChangedEvent =
  sendMessage mtrActionsClusterStateChangedEvent invokeIDSelector

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent, IsNSNumber value) => mtrActionsClusterStateChangedEvent -> value -> IO ()
setInvokeID mtrActionsClusterStateChangedEvent value =
  sendMessage mtrActionsClusterStateChangedEvent setInvokeIDSelector (toNSNumber value)

-- | @- newState@
newState :: IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent => mtrActionsClusterStateChangedEvent -> IO (Id NSNumber)
newState mtrActionsClusterStateChangedEvent =
  sendOwnedMessage mtrActionsClusterStateChangedEvent newStateSelector

-- | @- setNewState:@
setNewState :: (IsMTRActionsClusterStateChangedEvent mtrActionsClusterStateChangedEvent, IsNSNumber value) => mtrActionsClusterStateChangedEvent -> value -> IO ()
setNewState mtrActionsClusterStateChangedEvent value =
  sendMessage mtrActionsClusterStateChangedEvent setNewStateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionID@
actionIDSelector :: Selector '[] (Id NSNumber)
actionIDSelector = mkSelector "actionID"

-- | @Selector@ for @setActionID:@
setActionIDSelector :: Selector '[Id NSNumber] ()
setActionIDSelector = mkSelector "setActionID:"

-- | @Selector@ for @invokeID@
invokeIDSelector :: Selector '[] (Id NSNumber)
invokeIDSelector = mkSelector "invokeID"

-- | @Selector@ for @setInvokeID:@
setInvokeIDSelector :: Selector '[Id NSNumber] ()
setInvokeIDSelector = mkSelector "setInvokeID:"

-- | @Selector@ for @newState@
newStateSelector :: Selector '[] (Id NSNumber)
newStateSelector = mkSelector "newState"

-- | @Selector@ for @setNewState:@
setNewStateSelector :: Selector '[Id NSNumber] ()
setNewStateSelector = mkSelector "setNewState:"

