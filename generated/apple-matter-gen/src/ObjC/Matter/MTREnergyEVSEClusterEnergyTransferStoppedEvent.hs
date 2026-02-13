{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnergyTransferStoppedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEnergyTransferStoppedEvent
  ( MTREnergyEVSEClusterEnergyTransferStoppedEvent
  , IsMTREnergyEVSEClusterEnergyTransferStoppedEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , reason
  , setReason
  , energyTransferred
  , setEnergyTransferred
  , energyDischarged
  , setEnergyDischarged
  , energyDischargedSelector
  , energyTransferredSelector
  , reasonSelector
  , sessionIDSelector
  , setEnergyDischargedSelector
  , setEnergyTransferredSelector
  , setReasonSelector
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
sessionID :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEnergyTransferStoppedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent sessionIDSelector

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEnergyTransferStoppedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent setSessionIDSelector (toNSNumber value)

-- | @- state@
state :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterEnergyTransferStoppedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent stateSelector

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setState mtrEnergyEVSEClusterEnergyTransferStoppedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent setStateSelector (toNSNumber value)

-- | @- reason@
reason :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
reason mtrEnergyEVSEClusterEnergyTransferStoppedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent reasonSelector

-- | @- setReason:@
setReason :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setReason mtrEnergyEVSEClusterEnergyTransferStoppedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent setReasonSelector (toNSNumber value)

-- | @- energyTransferred@
energyTransferred :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
energyTransferred mtrEnergyEVSEClusterEnergyTransferStoppedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent energyTransferredSelector

-- | @- setEnergyTransferred:@
setEnergyTransferred :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setEnergyTransferred mtrEnergyEVSEClusterEnergyTransferStoppedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent setEnergyTransferredSelector (toNSNumber value)

-- | @- energyDischarged@
energyDischarged :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
energyDischarged mtrEnergyEVSEClusterEnergyTransferStoppedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent energyDischargedSelector

-- | @- setEnergyDischarged:@
setEnergyDischarged :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setEnergyDischarged mtrEnergyEVSEClusterEnergyTransferStoppedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStoppedEvent setEnergyDischargedSelector (toNSNumber value)

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

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] (Id NSNumber)
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector '[Id NSNumber] ()
setReasonSelector = mkSelector "setReason:"

-- | @Selector@ for @energyTransferred@
energyTransferredSelector :: Selector '[] (Id NSNumber)
energyTransferredSelector = mkSelector "energyTransferred"

-- | @Selector@ for @setEnergyTransferred:@
setEnergyTransferredSelector :: Selector '[Id NSNumber] ()
setEnergyTransferredSelector = mkSelector "setEnergyTransferred:"

-- | @Selector@ for @energyDischarged@
energyDischargedSelector :: Selector '[] (Id NSNumber)
energyDischargedSelector = mkSelector "energyDischarged"

-- | @Selector@ for @setEnergyDischarged:@
setEnergyDischargedSelector :: Selector '[Id NSNumber] ()
setEnergyDischargedSelector = mkSelector "setEnergyDischarged:"

