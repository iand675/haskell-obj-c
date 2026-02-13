{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterRunningStateChangeEvent@.
module ObjC.Matter.MTRThermostatClusterRunningStateChangeEvent
  ( MTRThermostatClusterRunningStateChangeEvent
  , IsMTRThermostatClusterRunningStateChangeEvent(..)
  , previousRunningState
  , setPreviousRunningState
  , currentRunningState
  , setCurrentRunningState
  , currentRunningStateSelector
  , previousRunningStateSelector
  , setCurrentRunningStateSelector
  , setPreviousRunningStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousRunningState@
previousRunningState :: IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent => mtrThermostatClusterRunningStateChangeEvent -> IO (Id NSNumber)
previousRunningState mtrThermostatClusterRunningStateChangeEvent =
  sendMessage mtrThermostatClusterRunningStateChangeEvent previousRunningStateSelector

-- | @- setPreviousRunningState:@
setPreviousRunningState :: (IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningStateChangeEvent -> value -> IO ()
setPreviousRunningState mtrThermostatClusterRunningStateChangeEvent value =
  sendMessage mtrThermostatClusterRunningStateChangeEvent setPreviousRunningStateSelector (toNSNumber value)

-- | @- currentRunningState@
currentRunningState :: IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent => mtrThermostatClusterRunningStateChangeEvent -> IO (Id NSNumber)
currentRunningState mtrThermostatClusterRunningStateChangeEvent =
  sendMessage mtrThermostatClusterRunningStateChangeEvent currentRunningStateSelector

-- | @- setCurrentRunningState:@
setCurrentRunningState :: (IsMTRThermostatClusterRunningStateChangeEvent mtrThermostatClusterRunningStateChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningStateChangeEvent -> value -> IO ()
setCurrentRunningState mtrThermostatClusterRunningStateChangeEvent value =
  sendMessage mtrThermostatClusterRunningStateChangeEvent setCurrentRunningStateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousRunningState@
previousRunningStateSelector :: Selector '[] (Id NSNumber)
previousRunningStateSelector = mkSelector "previousRunningState"

-- | @Selector@ for @setPreviousRunningState:@
setPreviousRunningStateSelector :: Selector '[Id NSNumber] ()
setPreviousRunningStateSelector = mkSelector "setPreviousRunningState:"

-- | @Selector@ for @currentRunningState@
currentRunningStateSelector :: Selector '[] (Id NSNumber)
currentRunningStateSelector = mkSelector "currentRunningState"

-- | @Selector@ for @setCurrentRunningState:@
setCurrentRunningStateSelector :: Selector '[Id NSNumber] ()
setCurrentRunningStateSelector = mkSelector "setCurrentRunningState:"

