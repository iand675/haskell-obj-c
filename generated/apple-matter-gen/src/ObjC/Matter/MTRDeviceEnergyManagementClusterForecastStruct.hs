{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterForecastStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterForecastStruct
  ( MTRDeviceEnergyManagementClusterForecastStruct
  , IsMTRDeviceEnergyManagementClusterForecastStruct(..)
  , forecastID
  , setForecastID
  , activeSlotNumber
  , setActiveSlotNumber
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , earliestStartTime
  , setEarliestStartTime
  , latestEndTime
  , setLatestEndTime
  , isPausable
  , setIsPausable
  , slots
  , setSlots
  , forecastUpdateReason
  , setForecastUpdateReason
  , activeSlotNumberSelector
  , earliestStartTimeSelector
  , endTimeSelector
  , forecastIDSelector
  , forecastUpdateReasonSelector
  , isPausableSelector
  , latestEndTimeSelector
  , setActiveSlotNumberSelector
  , setEarliestStartTimeSelector
  , setEndTimeSelector
  , setForecastIDSelector
  , setForecastUpdateReasonSelector
  , setIsPausableSelector
  , setLatestEndTimeSelector
  , setSlotsSelector
  , setStartTimeSelector
  , slotsSelector
  , startTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- forecastID@
forecastID :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
forecastID mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct forecastIDSelector

-- | @- setForecastID:@
setForecastID :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setForecastID mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setForecastIDSelector (toNSNumber value)

-- | @- activeSlotNumber@
activeSlotNumber :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
activeSlotNumber mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct activeSlotNumberSelector

-- | @- setActiveSlotNumber:@
setActiveSlotNumber :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setActiveSlotNumber mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setActiveSlotNumberSelector (toNSNumber value)

-- | @- startTime@
startTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
startTime mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setStartTime mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setStartTimeSelector (toNSNumber value)

-- | @- endTime@
endTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
endTime mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct endTimeSelector

-- | @- setEndTime:@
setEndTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setEndTime mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setEndTimeSelector (toNSNumber value)

-- | @- earliestStartTime@
earliestStartTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
earliestStartTime mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct earliestStartTimeSelector

-- | @- setEarliestStartTime:@
setEarliestStartTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setEarliestStartTime mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setEarliestStartTimeSelector (toNSNumber value)

-- | @- latestEndTime@
latestEndTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
latestEndTime mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct latestEndTimeSelector

-- | @- setLatestEndTime:@
setLatestEndTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setLatestEndTime mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setLatestEndTimeSelector (toNSNumber value)

-- | @- isPausable@
isPausable :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
isPausable mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct isPausableSelector

-- | @- setIsPausable:@
setIsPausable :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setIsPausable mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setIsPausableSelector (toNSNumber value)

-- | @- slots@
slots :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSArray)
slots mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct slotsSelector

-- | @- setSlots:@
setSlots :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSArray value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setSlots mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setSlotsSelector (toNSArray value)

-- | @- forecastUpdateReason@
forecastUpdateReason :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
forecastUpdateReason mtrDeviceEnergyManagementClusterForecastStruct =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct forecastUpdateReasonSelector

-- | @- setForecastUpdateReason:@
setForecastUpdateReason :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setForecastUpdateReason mtrDeviceEnergyManagementClusterForecastStruct value =
  sendMessage mtrDeviceEnergyManagementClusterForecastStruct setForecastUpdateReasonSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forecastID@
forecastIDSelector :: Selector '[] (Id NSNumber)
forecastIDSelector = mkSelector "forecastID"

-- | @Selector@ for @setForecastID:@
setForecastIDSelector :: Selector '[Id NSNumber] ()
setForecastIDSelector = mkSelector "setForecastID:"

-- | @Selector@ for @activeSlotNumber@
activeSlotNumberSelector :: Selector '[] (Id NSNumber)
activeSlotNumberSelector = mkSelector "activeSlotNumber"

-- | @Selector@ for @setActiveSlotNumber:@
setActiveSlotNumberSelector :: Selector '[Id NSNumber] ()
setActiveSlotNumberSelector = mkSelector "setActiveSlotNumber:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector '[] (Id NSNumber)
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector '[Id NSNumber] ()
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @earliestStartTime@
earliestStartTimeSelector :: Selector '[] (Id NSNumber)
earliestStartTimeSelector = mkSelector "earliestStartTime"

-- | @Selector@ for @setEarliestStartTime:@
setEarliestStartTimeSelector :: Selector '[Id NSNumber] ()
setEarliestStartTimeSelector = mkSelector "setEarliestStartTime:"

-- | @Selector@ for @latestEndTime@
latestEndTimeSelector :: Selector '[] (Id NSNumber)
latestEndTimeSelector = mkSelector "latestEndTime"

-- | @Selector@ for @setLatestEndTime:@
setLatestEndTimeSelector :: Selector '[Id NSNumber] ()
setLatestEndTimeSelector = mkSelector "setLatestEndTime:"

-- | @Selector@ for @isPausable@
isPausableSelector :: Selector '[] (Id NSNumber)
isPausableSelector = mkSelector "isPausable"

-- | @Selector@ for @setIsPausable:@
setIsPausableSelector :: Selector '[Id NSNumber] ()
setIsPausableSelector = mkSelector "setIsPausable:"

-- | @Selector@ for @slots@
slotsSelector :: Selector '[] (Id NSArray)
slotsSelector = mkSelector "slots"

-- | @Selector@ for @setSlots:@
setSlotsSelector :: Selector '[Id NSArray] ()
setSlotsSelector = mkSelector "setSlots:"

-- | @Selector@ for @forecastUpdateReason@
forecastUpdateReasonSelector :: Selector '[] (Id NSNumber)
forecastUpdateReasonSelector = mkSelector "forecastUpdateReason"

-- | @Selector@ for @setForecastUpdateReason:@
setForecastUpdateReasonSelector :: Selector '[Id NSNumber] ()
setForecastUpdateReasonSelector = mkSelector "setForecastUpdateReason:"

