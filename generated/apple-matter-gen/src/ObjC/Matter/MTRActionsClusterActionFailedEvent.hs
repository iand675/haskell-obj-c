{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterActionFailedEvent@.
module ObjC.Matter.MTRActionsClusterActionFailedEvent
  ( MTRActionsClusterActionFailedEvent
  , IsMTRActionsClusterActionFailedEvent(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , newState
  , setNewState
  , error_
  , setError
  , actionIDSelector
  , errorSelector
  , invokeIDSelector
  , newStateSelector
  , setActionIDSelector
  , setErrorSelector
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
actionID :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
actionID mtrActionsClusterActionFailedEvent =
  sendMessage mtrActionsClusterActionFailedEvent actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setActionID mtrActionsClusterActionFailedEvent value =
  sendMessage mtrActionsClusterActionFailedEvent setActionIDSelector (toNSNumber value)

-- | @- invokeID@
invokeID :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
invokeID mtrActionsClusterActionFailedEvent =
  sendMessage mtrActionsClusterActionFailedEvent invokeIDSelector

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setInvokeID mtrActionsClusterActionFailedEvent value =
  sendMessage mtrActionsClusterActionFailedEvent setInvokeIDSelector (toNSNumber value)

-- | @- newState@
newState :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
newState mtrActionsClusterActionFailedEvent =
  sendOwnedMessage mtrActionsClusterActionFailedEvent newStateSelector

-- | @- setNewState:@
setNewState :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setNewState mtrActionsClusterActionFailedEvent value =
  sendMessage mtrActionsClusterActionFailedEvent setNewStateSelector (toNSNumber value)

-- | @- error@
error_ :: IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent => mtrActionsClusterActionFailedEvent -> IO (Id NSNumber)
error_ mtrActionsClusterActionFailedEvent =
  sendMessage mtrActionsClusterActionFailedEvent errorSelector

-- | @- setError:@
setError :: (IsMTRActionsClusterActionFailedEvent mtrActionsClusterActionFailedEvent, IsNSNumber value) => mtrActionsClusterActionFailedEvent -> value -> IO ()
setError mtrActionsClusterActionFailedEvent value =
  sendMessage mtrActionsClusterActionFailedEvent setErrorSelector (toNSNumber value)

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

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSNumber)
errorSelector = mkSelector "error"

-- | @Selector@ for @setError:@
setErrorSelector :: Selector '[Id NSNumber] ()
setErrorSelector = mkSelector "setError:"

