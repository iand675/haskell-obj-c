{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateRequestorClusterStateTransitionEvent@.
module ObjC.Matter.MTROTASoftwareUpdateRequestorClusterStateTransitionEvent
  ( MTROTASoftwareUpdateRequestorClusterStateTransitionEvent
  , IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent(..)
  , previousState
  , setPreviousState
  , newState
  , setNewState
  , reason
  , setReason
  , targetSoftwareVersion
  , setTargetSoftwareVersion
  , newStateSelector
  , previousStateSelector
  , reasonSelector
  , setNewStateSelector
  , setPreviousStateSelector
  , setReasonSelector
  , setTargetSoftwareVersionSelector
  , targetSoftwareVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousState@
previousState :: IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
previousState mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent previousStateSelector

-- | @- setPreviousState:@
setPreviousState :: (IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setPreviousState mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent setPreviousStateSelector (toNSNumber value)

-- | @- newState@
newState :: IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
newState mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent =
  sendOwnedMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent newStateSelector

-- | @- setNewState:@
setNewState :: (IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setNewState mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent setNewStateSelector (toNSNumber value)

-- | @- reason@
reason :: IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
reason mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent reasonSelector

-- | @- setReason:@
setReason :: (IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setReason mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent setReasonSelector (toNSNumber value)

-- | @- targetSoftwareVersion@
targetSoftwareVersion :: IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> IO (Id NSNumber)
targetSoftwareVersion mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent targetSoftwareVersionSelector

-- | @- setTargetSoftwareVersion:@
setTargetSoftwareVersion :: (IsMTROTASoftwareUpdateRequestorClusterStateTransitionEvent mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent -> value -> IO ()
setTargetSoftwareVersion mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterStateTransitionEvent setTargetSoftwareVersionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousState@
previousStateSelector :: Selector '[] (Id NSNumber)
previousStateSelector = mkSelector "previousState"

-- | @Selector@ for @setPreviousState:@
setPreviousStateSelector :: Selector '[Id NSNumber] ()
setPreviousStateSelector = mkSelector "setPreviousState:"

-- | @Selector@ for @newState@
newStateSelector :: Selector '[] (Id NSNumber)
newStateSelector = mkSelector "newState"

-- | @Selector@ for @setNewState:@
setNewStateSelector :: Selector '[Id NSNumber] ()
setNewStateSelector = mkSelector "setNewState:"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] (Id NSNumber)
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector '[Id NSNumber] ()
setReasonSelector = mkSelector "setReason:"

-- | @Selector@ for @targetSoftwareVersion@
targetSoftwareVersionSelector :: Selector '[] (Id NSNumber)
targetSoftwareVersionSelector = mkSelector "targetSoftwareVersion"

-- | @Selector@ for @setTargetSoftwareVersion:@
setTargetSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setTargetSoftwareVersionSelector = mkSelector "setTargetSoftwareVersion:"

