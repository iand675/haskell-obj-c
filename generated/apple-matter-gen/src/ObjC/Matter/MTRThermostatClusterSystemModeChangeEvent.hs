{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSystemModeChangeEvent@.
module ObjC.Matter.MTRThermostatClusterSystemModeChangeEvent
  ( MTRThermostatClusterSystemModeChangeEvent
  , IsMTRThermostatClusterSystemModeChangeEvent(..)
  , previousSystemMode
  , setPreviousSystemMode
  , currentSystemMode
  , setCurrentSystemMode
  , currentSystemModeSelector
  , previousSystemModeSelector
  , setCurrentSystemModeSelector
  , setPreviousSystemModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousSystemMode@
previousSystemMode :: IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent => mtrThermostatClusterSystemModeChangeEvent -> IO (Id NSNumber)
previousSystemMode mtrThermostatClusterSystemModeChangeEvent =
  sendMessage mtrThermostatClusterSystemModeChangeEvent previousSystemModeSelector

-- | @- setPreviousSystemMode:@
setPreviousSystemMode :: (IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent, IsNSNumber value) => mtrThermostatClusterSystemModeChangeEvent -> value -> IO ()
setPreviousSystemMode mtrThermostatClusterSystemModeChangeEvent value =
  sendMessage mtrThermostatClusterSystemModeChangeEvent setPreviousSystemModeSelector (toNSNumber value)

-- | @- currentSystemMode@
currentSystemMode :: IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent => mtrThermostatClusterSystemModeChangeEvent -> IO (Id NSNumber)
currentSystemMode mtrThermostatClusterSystemModeChangeEvent =
  sendMessage mtrThermostatClusterSystemModeChangeEvent currentSystemModeSelector

-- | @- setCurrentSystemMode:@
setCurrentSystemMode :: (IsMTRThermostatClusterSystemModeChangeEvent mtrThermostatClusterSystemModeChangeEvent, IsNSNumber value) => mtrThermostatClusterSystemModeChangeEvent -> value -> IO ()
setCurrentSystemMode mtrThermostatClusterSystemModeChangeEvent value =
  sendMessage mtrThermostatClusterSystemModeChangeEvent setCurrentSystemModeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousSystemMode@
previousSystemModeSelector :: Selector '[] (Id NSNumber)
previousSystemModeSelector = mkSelector "previousSystemMode"

-- | @Selector@ for @setPreviousSystemMode:@
setPreviousSystemModeSelector :: Selector '[Id NSNumber] ()
setPreviousSystemModeSelector = mkSelector "setPreviousSystemMode:"

-- | @Selector@ for @currentSystemMode@
currentSystemModeSelector :: Selector '[] (Id NSNumber)
currentSystemModeSelector = mkSelector "currentSystemMode"

-- | @Selector@ for @setCurrentSystemMode:@
setCurrentSystemModeSelector :: Selector '[Id NSNumber] ()
setCurrentSystemModeSelector = mkSelector "setCurrentSystemMode:"

