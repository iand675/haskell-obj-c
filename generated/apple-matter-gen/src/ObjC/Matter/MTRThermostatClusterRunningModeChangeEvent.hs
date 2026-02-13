{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterRunningModeChangeEvent@.
module ObjC.Matter.MTRThermostatClusterRunningModeChangeEvent
  ( MTRThermostatClusterRunningModeChangeEvent
  , IsMTRThermostatClusterRunningModeChangeEvent(..)
  , previousRunningMode
  , setPreviousRunningMode
  , currentRunningMode
  , setCurrentRunningMode
  , currentRunningModeSelector
  , previousRunningModeSelector
  , setCurrentRunningModeSelector
  , setPreviousRunningModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousRunningMode@
previousRunningMode :: IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent => mtrThermostatClusterRunningModeChangeEvent -> IO (Id NSNumber)
previousRunningMode mtrThermostatClusterRunningModeChangeEvent =
  sendMessage mtrThermostatClusterRunningModeChangeEvent previousRunningModeSelector

-- | @- setPreviousRunningMode:@
setPreviousRunningMode :: (IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningModeChangeEvent -> value -> IO ()
setPreviousRunningMode mtrThermostatClusterRunningModeChangeEvent value =
  sendMessage mtrThermostatClusterRunningModeChangeEvent setPreviousRunningModeSelector (toNSNumber value)

-- | @- currentRunningMode@
currentRunningMode :: IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent => mtrThermostatClusterRunningModeChangeEvent -> IO (Id NSNumber)
currentRunningMode mtrThermostatClusterRunningModeChangeEvent =
  sendMessage mtrThermostatClusterRunningModeChangeEvent currentRunningModeSelector

-- | @- setCurrentRunningMode:@
setCurrentRunningMode :: (IsMTRThermostatClusterRunningModeChangeEvent mtrThermostatClusterRunningModeChangeEvent, IsNSNumber value) => mtrThermostatClusterRunningModeChangeEvent -> value -> IO ()
setCurrentRunningMode mtrThermostatClusterRunningModeChangeEvent value =
  sendMessage mtrThermostatClusterRunningModeChangeEvent setCurrentRunningModeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousRunningMode@
previousRunningModeSelector :: Selector '[] (Id NSNumber)
previousRunningModeSelector = mkSelector "previousRunningMode"

-- | @Selector@ for @setPreviousRunningMode:@
setPreviousRunningModeSelector :: Selector '[Id NSNumber] ()
setPreviousRunningModeSelector = mkSelector "setPreviousRunningMode:"

-- | @Selector@ for @currentRunningMode@
currentRunningModeSelector :: Selector '[] (Id NSNumber)
currentRunningModeSelector = mkSelector "currentRunningMode"

-- | @Selector@ for @setCurrentRunningMode:@
setCurrentRunningModeSelector :: Selector '[Id NSNumber] ()
setCurrentRunningModeSelector = mkSelector "setCurrentRunningMode:"

