{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterActiveScheduleChangeEvent@.
module ObjC.Matter.MTRThermostatClusterActiveScheduleChangeEvent
  ( MTRThermostatClusterActiveScheduleChangeEvent
  , IsMTRThermostatClusterActiveScheduleChangeEvent(..)
  , previousScheduleHandle
  , setPreviousScheduleHandle
  , currentScheduleHandle
  , setCurrentScheduleHandle
  , currentScheduleHandleSelector
  , previousScheduleHandleSelector
  , setCurrentScheduleHandleSelector
  , setPreviousScheduleHandleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousScheduleHandle@
previousScheduleHandle :: IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent => mtrThermostatClusterActiveScheduleChangeEvent -> IO (Id NSData)
previousScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent =
  sendMessage mtrThermostatClusterActiveScheduleChangeEvent previousScheduleHandleSelector

-- | @- setPreviousScheduleHandle:@
setPreviousScheduleHandle :: (IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent, IsNSData value) => mtrThermostatClusterActiveScheduleChangeEvent -> value -> IO ()
setPreviousScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent value =
  sendMessage mtrThermostatClusterActiveScheduleChangeEvent setPreviousScheduleHandleSelector (toNSData value)

-- | @- currentScheduleHandle@
currentScheduleHandle :: IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent => mtrThermostatClusterActiveScheduleChangeEvent -> IO (Id NSData)
currentScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent =
  sendMessage mtrThermostatClusterActiveScheduleChangeEvent currentScheduleHandleSelector

-- | @- setCurrentScheduleHandle:@
setCurrentScheduleHandle :: (IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent, IsNSData value) => mtrThermostatClusterActiveScheduleChangeEvent -> value -> IO ()
setCurrentScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent value =
  sendMessage mtrThermostatClusterActiveScheduleChangeEvent setCurrentScheduleHandleSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousScheduleHandle@
previousScheduleHandleSelector :: Selector '[] (Id NSData)
previousScheduleHandleSelector = mkSelector "previousScheduleHandle"

-- | @Selector@ for @setPreviousScheduleHandle:@
setPreviousScheduleHandleSelector :: Selector '[Id NSData] ()
setPreviousScheduleHandleSelector = mkSelector "setPreviousScheduleHandle:"

-- | @Selector@ for @currentScheduleHandle@
currentScheduleHandleSelector :: Selector '[] (Id NSData)
currentScheduleHandleSelector = mkSelector "currentScheduleHandle"

-- | @Selector@ for @setCurrentScheduleHandle:@
setCurrentScheduleHandleSelector :: Selector '[Id NSData] ()
setCurrentScheduleHandleSelector = mkSelector "setCurrentScheduleHandle:"

