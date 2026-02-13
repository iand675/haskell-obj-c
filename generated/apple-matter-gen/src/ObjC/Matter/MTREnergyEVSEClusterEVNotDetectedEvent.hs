{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEVNotDetectedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEVNotDetectedEvent
  ( MTREnergyEVSEClusterEVNotDetectedEvent
  , IsMTREnergyEVSEClusterEVNotDetectedEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , sessionDuration
  , setSessionDuration
  , sessionEnergyCharged
  , setSessionEnergyCharged
  , sessionEnergyDischarged
  , setSessionEnergyDischarged
  , sessionDurationSelector
  , sessionEnergyChargedSelector
  , sessionEnergyDischargedSelector
  , sessionIDSelector
  , setSessionDurationSelector
  , setSessionEnergyChargedSelector
  , setSessionEnergyDischargedSelector
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
sessionID :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEVNotDetectedEvent =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent sessionIDSelector

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEVNotDetectedEvent value =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent setSessionIDSelector (toNSNumber value)

-- | @- state@
state :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterEVNotDetectedEvent =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent stateSelector

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setState mtrEnergyEVSEClusterEVNotDetectedEvent value =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent setStateSelector (toNSNumber value)

-- | @- sessionDuration@
sessionDuration :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionDuration mtrEnergyEVSEClusterEVNotDetectedEvent =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent sessionDurationSelector

-- | @- setSessionDuration:@
setSessionDuration :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionDuration mtrEnergyEVSEClusterEVNotDetectedEvent value =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent setSessionDurationSelector (toNSNumber value)

-- | @- sessionEnergyCharged@
sessionEnergyCharged :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionEnergyCharged mtrEnergyEVSEClusterEVNotDetectedEvent =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent sessionEnergyChargedSelector

-- | @- setSessionEnergyCharged:@
setSessionEnergyCharged :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionEnergyCharged mtrEnergyEVSEClusterEVNotDetectedEvent value =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent setSessionEnergyChargedSelector (toNSNumber value)

-- | @- sessionEnergyDischarged@
sessionEnergyDischarged :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionEnergyDischarged mtrEnergyEVSEClusterEVNotDetectedEvent =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent sessionEnergyDischargedSelector

-- | @- setSessionEnergyDischarged:@
setSessionEnergyDischarged :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionEnergyDischarged mtrEnergyEVSEClusterEVNotDetectedEvent value =
  sendMessage mtrEnergyEVSEClusterEVNotDetectedEvent setSessionEnergyDischargedSelector (toNSNumber value)

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

-- | @Selector@ for @sessionDuration@
sessionDurationSelector :: Selector '[] (Id NSNumber)
sessionDurationSelector = mkSelector "sessionDuration"

-- | @Selector@ for @setSessionDuration:@
setSessionDurationSelector :: Selector '[Id NSNumber] ()
setSessionDurationSelector = mkSelector "setSessionDuration:"

-- | @Selector@ for @sessionEnergyCharged@
sessionEnergyChargedSelector :: Selector '[] (Id NSNumber)
sessionEnergyChargedSelector = mkSelector "sessionEnergyCharged"

-- | @Selector@ for @setSessionEnergyCharged:@
setSessionEnergyChargedSelector :: Selector '[Id NSNumber] ()
setSessionEnergyChargedSelector = mkSelector "setSessionEnergyCharged:"

-- | @Selector@ for @sessionEnergyDischarged@
sessionEnergyDischargedSelector :: Selector '[] (Id NSNumber)
sessionEnergyDischargedSelector = mkSelector "sessionEnergyDischarged"

-- | @Selector@ for @setSessionEnergyDischarged:@
setSessionEnergyDischargedSelector :: Selector '[Id NSNumber] ()
setSessionEnergyDischargedSelector = mkSelector "setSessionEnergyDischarged:"

