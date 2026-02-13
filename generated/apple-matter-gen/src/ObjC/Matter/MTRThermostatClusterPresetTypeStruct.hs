{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterPresetTypeStruct@.
module ObjC.Matter.MTRThermostatClusterPresetTypeStruct
  ( MTRThermostatClusterPresetTypeStruct
  , IsMTRThermostatClusterPresetTypeStruct(..)
  , presetScenario
  , setPresetScenario
  , numberOfPresets
  , setNumberOfPresets
  , presetTypeFeatures
  , setPresetTypeFeatures
  , numberOfPresetsSelector
  , presetScenarioSelector
  , presetTypeFeaturesSelector
  , setNumberOfPresetsSelector
  , setPresetScenarioSelector
  , setPresetTypeFeaturesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presetScenario@
presetScenario :: IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct => mtrThermostatClusterPresetTypeStruct -> IO (Id NSNumber)
presetScenario mtrThermostatClusterPresetTypeStruct =
  sendMessage mtrThermostatClusterPresetTypeStruct presetScenarioSelector

-- | @- setPresetScenario:@
setPresetScenario :: (IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct, IsNSNumber value) => mtrThermostatClusterPresetTypeStruct -> value -> IO ()
setPresetScenario mtrThermostatClusterPresetTypeStruct value =
  sendMessage mtrThermostatClusterPresetTypeStruct setPresetScenarioSelector (toNSNumber value)

-- | @- numberOfPresets@
numberOfPresets :: IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct => mtrThermostatClusterPresetTypeStruct -> IO (Id NSNumber)
numberOfPresets mtrThermostatClusterPresetTypeStruct =
  sendMessage mtrThermostatClusterPresetTypeStruct numberOfPresetsSelector

-- | @- setNumberOfPresets:@
setNumberOfPresets :: (IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct, IsNSNumber value) => mtrThermostatClusterPresetTypeStruct -> value -> IO ()
setNumberOfPresets mtrThermostatClusterPresetTypeStruct value =
  sendMessage mtrThermostatClusterPresetTypeStruct setNumberOfPresetsSelector (toNSNumber value)

-- | @- presetTypeFeatures@
presetTypeFeatures :: IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct => mtrThermostatClusterPresetTypeStruct -> IO (Id NSNumber)
presetTypeFeatures mtrThermostatClusterPresetTypeStruct =
  sendMessage mtrThermostatClusterPresetTypeStruct presetTypeFeaturesSelector

-- | @- setPresetTypeFeatures:@
setPresetTypeFeatures :: (IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct, IsNSNumber value) => mtrThermostatClusterPresetTypeStruct -> value -> IO ()
setPresetTypeFeatures mtrThermostatClusterPresetTypeStruct value =
  sendMessage mtrThermostatClusterPresetTypeStruct setPresetTypeFeaturesSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetScenario@
presetScenarioSelector :: Selector '[] (Id NSNumber)
presetScenarioSelector = mkSelector "presetScenario"

-- | @Selector@ for @setPresetScenario:@
setPresetScenarioSelector :: Selector '[Id NSNumber] ()
setPresetScenarioSelector = mkSelector "setPresetScenario:"

-- | @Selector@ for @numberOfPresets@
numberOfPresetsSelector :: Selector '[] (Id NSNumber)
numberOfPresetsSelector = mkSelector "numberOfPresets"

-- | @Selector@ for @setNumberOfPresets:@
setNumberOfPresetsSelector :: Selector '[Id NSNumber] ()
setNumberOfPresetsSelector = mkSelector "setNumberOfPresets:"

-- | @Selector@ for @presetTypeFeatures@
presetTypeFeaturesSelector :: Selector '[] (Id NSNumber)
presetTypeFeaturesSelector = mkSelector "presetTypeFeatures"

-- | @Selector@ for @setPresetTypeFeatures:@
setPresetTypeFeaturesSelector :: Selector '[Id NSNumber] ()
setPresetTypeFeaturesSelector = mkSelector "setPresetTypeFeatures:"

