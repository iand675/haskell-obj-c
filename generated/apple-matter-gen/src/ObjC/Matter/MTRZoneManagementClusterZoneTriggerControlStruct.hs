{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneTriggerControlStruct@.
module ObjC.Matter.MTRZoneManagementClusterZoneTriggerControlStruct
  ( MTRZoneManagementClusterZoneTriggerControlStruct
  , IsMTRZoneManagementClusterZoneTriggerControlStruct(..)
  , zoneID
  , setZoneID
  , initialDuration
  , setInitialDuration
  , augmentationDuration
  , setAugmentationDuration
  , maxDuration
  , setMaxDuration
  , blindDuration
  , setBlindDuration
  , sensitivity
  , setSensitivity
  , augmentationDurationSelector
  , blindDurationSelector
  , initialDurationSelector
  , maxDurationSelector
  , sensitivitySelector
  , setAugmentationDurationSelector
  , setBlindDurationSelector
  , setInitialDurationSelector
  , setMaxDurationSelector
  , setSensitivitySelector
  , setZoneIDSelector
  , zoneIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterZoneTriggerControlStruct =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct zoneIDSelector

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setZoneID mtrZoneManagementClusterZoneTriggerControlStruct value =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct setZoneIDSelector (toNSNumber value)

-- | @- initialDuration@
initialDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
initialDuration mtrZoneManagementClusterZoneTriggerControlStruct =
  sendOwnedMessage mtrZoneManagementClusterZoneTriggerControlStruct initialDurationSelector

-- | @- setInitialDuration:@
setInitialDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setInitialDuration mtrZoneManagementClusterZoneTriggerControlStruct value =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct setInitialDurationSelector (toNSNumber value)

-- | @- augmentationDuration@
augmentationDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
augmentationDuration mtrZoneManagementClusterZoneTriggerControlStruct =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct augmentationDurationSelector

-- | @- setAugmentationDuration:@
setAugmentationDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setAugmentationDuration mtrZoneManagementClusterZoneTriggerControlStruct value =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct setAugmentationDurationSelector (toNSNumber value)

-- | @- maxDuration@
maxDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
maxDuration mtrZoneManagementClusterZoneTriggerControlStruct =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct maxDurationSelector

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setMaxDuration mtrZoneManagementClusterZoneTriggerControlStruct value =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct setMaxDurationSelector (toNSNumber value)

-- | @- blindDuration@
blindDuration :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
blindDuration mtrZoneManagementClusterZoneTriggerControlStruct =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct blindDurationSelector

-- | @- setBlindDuration:@
setBlindDuration :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setBlindDuration mtrZoneManagementClusterZoneTriggerControlStruct value =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct setBlindDurationSelector (toNSNumber value)

-- | @- sensitivity@
sensitivity :: IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct => mtrZoneManagementClusterZoneTriggerControlStruct -> IO (Id NSNumber)
sensitivity mtrZoneManagementClusterZoneTriggerControlStruct =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct sensitivitySelector

-- | @- setSensitivity:@
setSensitivity :: (IsMTRZoneManagementClusterZoneTriggerControlStruct mtrZoneManagementClusterZoneTriggerControlStruct, IsNSNumber value) => mtrZoneManagementClusterZoneTriggerControlStruct -> value -> IO ()
setSensitivity mtrZoneManagementClusterZoneTriggerControlStruct value =
  sendMessage mtrZoneManagementClusterZoneTriggerControlStruct setSensitivitySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id NSNumber)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector '[Id NSNumber] ()
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @initialDuration@
initialDurationSelector :: Selector '[] (Id NSNumber)
initialDurationSelector = mkSelector "initialDuration"

-- | @Selector@ for @setInitialDuration:@
setInitialDurationSelector :: Selector '[Id NSNumber] ()
setInitialDurationSelector = mkSelector "setInitialDuration:"

-- | @Selector@ for @augmentationDuration@
augmentationDurationSelector :: Selector '[] (Id NSNumber)
augmentationDurationSelector = mkSelector "augmentationDuration"

-- | @Selector@ for @setAugmentationDuration:@
setAugmentationDurationSelector :: Selector '[Id NSNumber] ()
setAugmentationDurationSelector = mkSelector "setAugmentationDuration:"

-- | @Selector@ for @maxDuration@
maxDurationSelector :: Selector '[] (Id NSNumber)
maxDurationSelector = mkSelector "maxDuration"

-- | @Selector@ for @setMaxDuration:@
setMaxDurationSelector :: Selector '[Id NSNumber] ()
setMaxDurationSelector = mkSelector "setMaxDuration:"

-- | @Selector@ for @blindDuration@
blindDurationSelector :: Selector '[] (Id NSNumber)
blindDurationSelector = mkSelector "blindDuration"

-- | @Selector@ for @setBlindDuration:@
setBlindDurationSelector :: Selector '[Id NSNumber] ()
setBlindDurationSelector = mkSelector "setBlindDuration:"

-- | @Selector@ for @sensitivity@
sensitivitySelector :: Selector '[] (Id NSNumber)
sensitivitySelector = mkSelector "sensitivity"

-- | @Selector@ for @setSensitivity:@
setSensitivitySelector :: Selector '[Id NSNumber] ()
setSensitivitySelector = mkSelector "setSensitivity:"

