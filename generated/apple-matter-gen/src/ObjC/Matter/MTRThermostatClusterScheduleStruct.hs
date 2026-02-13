{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterScheduleStruct@.
module ObjC.Matter.MTRThermostatClusterScheduleStruct
  ( MTRThermostatClusterScheduleStruct
  , IsMTRThermostatClusterScheduleStruct(..)
  , scheduleHandle
  , setScheduleHandle
  , systemMode
  , setSystemMode
  , name
  , setName
  , presetHandle
  , setPresetHandle
  , transitions
  , setTransitions
  , builtIn
  , setBuiltIn
  , builtInSelector
  , nameSelector
  , presetHandleSelector
  , scheduleHandleSelector
  , setBuiltInSelector
  , setNameSelector
  , setPresetHandleSelector
  , setScheduleHandleSelector
  , setSystemModeSelector
  , setTransitionsSelector
  , systemModeSelector
  , transitionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- scheduleHandle@
scheduleHandle :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSData)
scheduleHandle mtrThermostatClusterScheduleStruct =
  sendMessage mtrThermostatClusterScheduleStruct scheduleHandleSelector

-- | @- setScheduleHandle:@
setScheduleHandle :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSData value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setScheduleHandle mtrThermostatClusterScheduleStruct value =
  sendMessage mtrThermostatClusterScheduleStruct setScheduleHandleSelector (toNSData value)

-- | @- systemMode@
systemMode :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSNumber)
systemMode mtrThermostatClusterScheduleStruct =
  sendMessage mtrThermostatClusterScheduleStruct systemModeSelector

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSNumber value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setSystemMode mtrThermostatClusterScheduleStruct value =
  sendMessage mtrThermostatClusterScheduleStruct setSystemModeSelector (toNSNumber value)

-- | @- name@
name :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSString)
name mtrThermostatClusterScheduleStruct =
  sendMessage mtrThermostatClusterScheduleStruct nameSelector

-- | @- setName:@
setName :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSString value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setName mtrThermostatClusterScheduleStruct value =
  sendMessage mtrThermostatClusterScheduleStruct setNameSelector (toNSString value)

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterScheduleStruct =
  sendMessage mtrThermostatClusterScheduleStruct presetHandleSelector

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSData value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterScheduleStruct value =
  sendMessage mtrThermostatClusterScheduleStruct setPresetHandleSelector (toNSData value)

-- | @- transitions@
transitions :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSArray)
transitions mtrThermostatClusterScheduleStruct =
  sendMessage mtrThermostatClusterScheduleStruct transitionsSelector

-- | @- setTransitions:@
setTransitions :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSArray value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setTransitions mtrThermostatClusterScheduleStruct value =
  sendMessage mtrThermostatClusterScheduleStruct setTransitionsSelector (toNSArray value)

-- | @- builtIn@
builtIn :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSNumber)
builtIn mtrThermostatClusterScheduleStruct =
  sendMessage mtrThermostatClusterScheduleStruct builtInSelector

-- | @- setBuiltIn:@
setBuiltIn :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSNumber value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setBuiltIn mtrThermostatClusterScheduleStruct value =
  sendMessage mtrThermostatClusterScheduleStruct setBuiltInSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scheduleHandle@
scheduleHandleSelector :: Selector '[] (Id NSData)
scheduleHandleSelector = mkSelector "scheduleHandle"

-- | @Selector@ for @setScheduleHandle:@
setScheduleHandleSelector :: Selector '[Id NSData] ()
setScheduleHandleSelector = mkSelector "setScheduleHandle:"

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector '[] (Id NSNumber)
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector '[Id NSNumber] ()
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector '[] (Id NSData)
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector '[Id NSData] ()
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @transitions@
transitionsSelector :: Selector '[] (Id NSArray)
transitionsSelector = mkSelector "transitions"

-- | @Selector@ for @setTransitions:@
setTransitionsSelector :: Selector '[Id NSArray] ()
setTransitionsSelector = mkSelector "setTransitions:"

-- | @Selector@ for @builtIn@
builtInSelector :: Selector '[] (Id NSNumber)
builtInSelector = mkSelector "builtIn"

-- | @Selector@ for @setBuiltIn:@
setBuiltInSelector :: Selector '[Id NSNumber] ()
setBuiltInSelector = mkSelector "setBuiltIn:"

