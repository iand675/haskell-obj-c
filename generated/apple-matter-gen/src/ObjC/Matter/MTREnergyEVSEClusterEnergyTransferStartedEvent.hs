{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnergyTransferStartedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEnergyTransferStartedEvent
  ( MTREnergyEVSEClusterEnergyTransferStartedEvent
  , IsMTREnergyEVSEClusterEnergyTransferStartedEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , maximumCurrent
  , setMaximumCurrent
  , maximumDischargeCurrent
  , setMaximumDischargeCurrent
  , maximumCurrentSelector
  , maximumDischargeCurrentSelector
  , sessionIDSelector
  , setMaximumCurrentSelector
  , setMaximumDischargeCurrentSelector
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
sessionID :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEnergyTransferStartedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent sessionIDSelector

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEnergyTransferStartedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent setSessionIDSelector (toNSNumber value)

-- | @- state@
state :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterEnergyTransferStartedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent stateSelector

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setState mtrEnergyEVSEClusterEnergyTransferStartedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent setStateSelector (toNSNumber value)

-- | @- maximumCurrent@
maximumCurrent :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
maximumCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent maximumCurrentSelector

-- | @- setMaximumCurrent:@
setMaximumCurrent :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setMaximumCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent setMaximumCurrentSelector (toNSNumber value)

-- | @- maximumDischargeCurrent@
maximumDischargeCurrent :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
maximumDischargeCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent maximumDischargeCurrentSelector

-- | @- setMaximumDischargeCurrent:@
setMaximumDischargeCurrent :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setMaximumDischargeCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent value =
  sendMessage mtrEnergyEVSEClusterEnergyTransferStartedEvent setMaximumDischargeCurrentSelector (toNSNumber value)

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

-- | @Selector@ for @maximumCurrent@
maximumCurrentSelector :: Selector '[] (Id NSNumber)
maximumCurrentSelector = mkSelector "maximumCurrent"

-- | @Selector@ for @setMaximumCurrent:@
setMaximumCurrentSelector :: Selector '[Id NSNumber] ()
setMaximumCurrentSelector = mkSelector "setMaximumCurrent:"

-- | @Selector@ for @maximumDischargeCurrent@
maximumDischargeCurrentSelector :: Selector '[] (Id NSNumber)
maximumDischargeCurrentSelector = mkSelector "maximumDischargeCurrent"

-- | @Selector@ for @setMaximumDischargeCurrent:@
setMaximumDischargeCurrentSelector :: Selector '[Id NSNumber] ()
setMaximumDischargeCurrentSelector = mkSelector "setMaximumDischargeCurrent:"

