{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterFaultEvent@.
module ObjC.Matter.MTREnergyEVSEClusterFaultEvent
  ( MTREnergyEVSEClusterFaultEvent
  , IsMTREnergyEVSEClusterFaultEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , faultStatePreviousState
  , setFaultStatePreviousState
  , faultStateCurrentState
  , setFaultStateCurrentState
  , faultStateCurrentStateSelector
  , faultStatePreviousStateSelector
  , sessionIDSelector
  , setFaultStateCurrentStateSelector
  , setFaultStatePreviousStateSelector
  , setSessionIDSelector
  , setStateSelector
  , stateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sessionID@
sessionID :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterFaultEvent =
  sendMessage mtrEnergyEVSEClusterFaultEvent sessionIDSelector

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterFaultEvent value =
  sendMessage mtrEnergyEVSEClusterFaultEvent setSessionIDSelector (toNSNumber value)

-- | @- state@
state :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterFaultEvent =
  sendMessage mtrEnergyEVSEClusterFaultEvent stateSelector

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setState mtrEnergyEVSEClusterFaultEvent value =
  sendMessage mtrEnergyEVSEClusterFaultEvent setStateSelector (toNSNumber value)

-- | @- faultStatePreviousState@
faultStatePreviousState :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
faultStatePreviousState mtrEnergyEVSEClusterFaultEvent =
  sendMessage mtrEnergyEVSEClusterFaultEvent faultStatePreviousStateSelector

-- | @- setFaultStatePreviousState:@
setFaultStatePreviousState :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setFaultStatePreviousState mtrEnergyEVSEClusterFaultEvent value =
  sendMessage mtrEnergyEVSEClusterFaultEvent setFaultStatePreviousStateSelector (toNSNumber value)

-- | @- faultStateCurrentState@
faultStateCurrentState :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
faultStateCurrentState mtrEnergyEVSEClusterFaultEvent =
  sendMessage mtrEnergyEVSEClusterFaultEvent faultStateCurrentStateSelector

-- | @- setFaultStateCurrentState:@
setFaultStateCurrentState :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setFaultStateCurrentState mtrEnergyEVSEClusterFaultEvent value =
  sendMessage mtrEnergyEVSEClusterFaultEvent setFaultStateCurrentStateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector '[] (Id NSNumber)
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @setSessionID:@
setSessionIDSelector :: Selector '[Id NSNumber] ()
setSessionIDSelector = mkSelector "setSessionID:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSNumber)
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[Id NSNumber] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @faultStatePreviousState@
faultStatePreviousStateSelector :: Selector '[] (Id NSNumber)
faultStatePreviousStateSelector = mkSelector "faultStatePreviousState"

-- | @Selector@ for @setFaultStatePreviousState:@
setFaultStatePreviousStateSelector :: Selector '[Id NSNumber] ()
setFaultStatePreviousStateSelector = mkSelector "setFaultStatePreviousState:"

-- | @Selector@ for @faultStateCurrentState@
faultStateCurrentStateSelector :: Selector '[] (Id NSNumber)
faultStateCurrentStateSelector = mkSelector "faultStateCurrentState"

-- | @Selector@ for @setFaultStateCurrentState:@
setFaultStateCurrentStateSelector :: Selector '[Id NSNumber] ()
setFaultStateCurrentStateSelector = mkSelector "setFaultStateCurrentState:"

