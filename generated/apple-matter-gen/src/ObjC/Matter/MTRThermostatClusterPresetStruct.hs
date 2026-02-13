{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterPresetStruct@.
module ObjC.Matter.MTRThermostatClusterPresetStruct
  ( MTRThermostatClusterPresetStruct
  , IsMTRThermostatClusterPresetStruct(..)
  , presetHandle
  , setPresetHandle
  , presetScenario
  , setPresetScenario
  , name
  , setName
  , coolingSetpoint
  , setCoolingSetpoint
  , heatingSetpoint
  , setHeatingSetpoint
  , builtIn
  , setBuiltIn
  , builtInSelector
  , coolingSetpointSelector
  , heatingSetpointSelector
  , nameSelector
  , presetHandleSelector
  , presetScenarioSelector
  , setBuiltInSelector
  , setCoolingSetpointSelector
  , setHeatingSetpointSelector
  , setNameSelector
  , setPresetHandleSelector
  , setPresetScenarioSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterPresetStruct =
  sendMessage mtrThermostatClusterPresetStruct presetHandleSelector

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSData value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterPresetStruct value =
  sendMessage mtrThermostatClusterPresetStruct setPresetHandleSelector (toNSData value)

-- | @- presetScenario@
presetScenario :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
presetScenario mtrThermostatClusterPresetStruct =
  sendMessage mtrThermostatClusterPresetStruct presetScenarioSelector

-- | @- setPresetScenario:@
setPresetScenario :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setPresetScenario mtrThermostatClusterPresetStruct value =
  sendMessage mtrThermostatClusterPresetStruct setPresetScenarioSelector (toNSNumber value)

-- | @- name@
name :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSString)
name mtrThermostatClusterPresetStruct =
  sendMessage mtrThermostatClusterPresetStruct nameSelector

-- | @- setName:@
setName :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSString value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setName mtrThermostatClusterPresetStruct value =
  sendMessage mtrThermostatClusterPresetStruct setNameSelector (toNSString value)

-- | @- coolingSetpoint@
coolingSetpoint :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
coolingSetpoint mtrThermostatClusterPresetStruct =
  sendMessage mtrThermostatClusterPresetStruct coolingSetpointSelector

-- | @- setCoolingSetpoint:@
setCoolingSetpoint :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setCoolingSetpoint mtrThermostatClusterPresetStruct value =
  sendMessage mtrThermostatClusterPresetStruct setCoolingSetpointSelector (toNSNumber value)

-- | @- heatingSetpoint@
heatingSetpoint :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
heatingSetpoint mtrThermostatClusterPresetStruct =
  sendMessage mtrThermostatClusterPresetStruct heatingSetpointSelector

-- | @- setHeatingSetpoint:@
setHeatingSetpoint :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setHeatingSetpoint mtrThermostatClusterPresetStruct value =
  sendMessage mtrThermostatClusterPresetStruct setHeatingSetpointSelector (toNSNumber value)

-- | @- builtIn@
builtIn :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
builtIn mtrThermostatClusterPresetStruct =
  sendMessage mtrThermostatClusterPresetStruct builtInSelector

-- | @- setBuiltIn:@
setBuiltIn :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setBuiltIn mtrThermostatClusterPresetStruct value =
  sendMessage mtrThermostatClusterPresetStruct setBuiltInSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector '[] (Id NSData)
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector '[Id NSData] ()
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @presetScenario@
presetScenarioSelector :: Selector '[] (Id NSNumber)
presetScenarioSelector = mkSelector "presetScenario"

-- | @Selector@ for @setPresetScenario:@
setPresetScenarioSelector :: Selector '[Id NSNumber] ()
setPresetScenarioSelector = mkSelector "setPresetScenario:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @coolingSetpoint@
coolingSetpointSelector :: Selector '[] (Id NSNumber)
coolingSetpointSelector = mkSelector "coolingSetpoint"

-- | @Selector@ for @setCoolingSetpoint:@
setCoolingSetpointSelector :: Selector '[Id NSNumber] ()
setCoolingSetpointSelector = mkSelector "setCoolingSetpoint:"

-- | @Selector@ for @heatingSetpoint@
heatingSetpointSelector :: Selector '[] (Id NSNumber)
heatingSetpointSelector = mkSelector "heatingSetpoint"

-- | @Selector@ for @setHeatingSetpoint:@
setHeatingSetpointSelector :: Selector '[Id NSNumber] ()
setHeatingSetpointSelector = mkSelector "setHeatingSetpoint:"

-- | @Selector@ for @builtIn@
builtInSelector :: Selector '[] (Id NSNumber)
builtInSelector = mkSelector "builtIn"

-- | @Selector@ for @setBuiltIn:@
setBuiltInSelector :: Selector '[Id NSNumber] ()
setBuiltInSelector = mkSelector "setBuiltIn:"

