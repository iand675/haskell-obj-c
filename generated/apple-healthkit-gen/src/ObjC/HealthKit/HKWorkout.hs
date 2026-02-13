{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkout
--
-- An HKObject subclass representing a workout or activity
--
-- Generated bindings for @HKWorkout@.
module ObjC.HealthKit.HKWorkout
  ( HKWorkout
  , IsHKWorkout(..)
  , statisticsForType
  , workoutWithActivityType_startDate_endDate
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadata
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadata
  , workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadata
  , workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadata
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadata
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadata
  , workoutActivityType
  , workoutEvents
  , workoutActivities
  , duration
  , totalEnergyBurned
  , totalDistance
  , totalSwimmingStrokeCount
  , totalFlightsClimbed
  , allStatistics
  , allStatisticsSelector
  , durationSelector
  , statisticsForTypeSelector
  , totalDistanceSelector
  , totalEnergyBurnedSelector
  , totalFlightsClimbedSelector
  , totalSwimmingStrokeCountSelector
  , workoutActivitiesSelector
  , workoutActivityTypeSelector
  , workoutEventsSelector
  , workoutWithActivityType_startDate_endDateSelector
  , workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadataSelector
  , workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadataSelector
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadataSelector
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadataSelector
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadataSelector
  , workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadataSelector

  -- * Enum types
  , HKWorkoutActivityType(HKWorkoutActivityType)
  , pattern HKWorkoutActivityTypeAmericanFootball
  , pattern HKWorkoutActivityTypeArchery
  , pattern HKWorkoutActivityTypeAustralianFootball
  , pattern HKWorkoutActivityTypeBadminton
  , pattern HKWorkoutActivityTypeBaseball
  , pattern HKWorkoutActivityTypeBasketball
  , pattern HKWorkoutActivityTypeBowling
  , pattern HKWorkoutActivityTypeBoxing
  , pattern HKWorkoutActivityTypeClimbing
  , pattern HKWorkoutActivityTypeCricket
  , pattern HKWorkoutActivityTypeCrossTraining
  , pattern HKWorkoutActivityTypeCurling
  , pattern HKWorkoutActivityTypeCycling
  , pattern HKWorkoutActivityTypeDance
  , pattern HKWorkoutActivityTypeDanceInspiredTraining
  , pattern HKWorkoutActivityTypeElliptical
  , pattern HKWorkoutActivityTypeEquestrianSports
  , pattern HKWorkoutActivityTypeFencing
  , pattern HKWorkoutActivityTypeFishing
  , pattern HKWorkoutActivityTypeFunctionalStrengthTraining
  , pattern HKWorkoutActivityTypeGolf
  , pattern HKWorkoutActivityTypeGymnastics
  , pattern HKWorkoutActivityTypeHandball
  , pattern HKWorkoutActivityTypeHiking
  , pattern HKWorkoutActivityTypeHockey
  , pattern HKWorkoutActivityTypeHunting
  , pattern HKWorkoutActivityTypeLacrosse
  , pattern HKWorkoutActivityTypeMartialArts
  , pattern HKWorkoutActivityTypeMindAndBody
  , pattern HKWorkoutActivityTypeMixedMetabolicCardioTraining
  , pattern HKWorkoutActivityTypePaddleSports
  , pattern HKWorkoutActivityTypePlay
  , pattern HKWorkoutActivityTypePreparationAndRecovery
  , pattern HKWorkoutActivityTypeRacquetball
  , pattern HKWorkoutActivityTypeRowing
  , pattern HKWorkoutActivityTypeRugby
  , pattern HKWorkoutActivityTypeRunning
  , pattern HKWorkoutActivityTypeSailing
  , pattern HKWorkoutActivityTypeSkatingSports
  , pattern HKWorkoutActivityTypeSnowSports
  , pattern HKWorkoutActivityTypeSoccer
  , pattern HKWorkoutActivityTypeSoftball
  , pattern HKWorkoutActivityTypeSquash
  , pattern HKWorkoutActivityTypeStairClimbing
  , pattern HKWorkoutActivityTypeSurfingSports
  , pattern HKWorkoutActivityTypeSwimming
  , pattern HKWorkoutActivityTypeTableTennis
  , pattern HKWorkoutActivityTypeTennis
  , pattern HKWorkoutActivityTypeTrackAndField
  , pattern HKWorkoutActivityTypeTraditionalStrengthTraining
  , pattern HKWorkoutActivityTypeVolleyball
  , pattern HKWorkoutActivityTypeWalking
  , pattern HKWorkoutActivityTypeWaterFitness
  , pattern HKWorkoutActivityTypeWaterPolo
  , pattern HKWorkoutActivityTypeWaterSports
  , pattern HKWorkoutActivityTypeWrestling
  , pattern HKWorkoutActivityTypeYoga
  , pattern HKWorkoutActivityTypeBarre
  , pattern HKWorkoutActivityTypeCoreTraining
  , pattern HKWorkoutActivityTypeCrossCountrySkiing
  , pattern HKWorkoutActivityTypeDownhillSkiing
  , pattern HKWorkoutActivityTypeFlexibility
  , pattern HKWorkoutActivityTypeHighIntensityIntervalTraining
  , pattern HKWorkoutActivityTypeJumpRope
  , pattern HKWorkoutActivityTypeKickboxing
  , pattern HKWorkoutActivityTypePilates
  , pattern HKWorkoutActivityTypeSnowboarding
  , pattern HKWorkoutActivityTypeStairs
  , pattern HKWorkoutActivityTypeStepTraining
  , pattern HKWorkoutActivityTypeWheelchairWalkPace
  , pattern HKWorkoutActivityTypeWheelchairRunPace
  , pattern HKWorkoutActivityTypeTaiChi
  , pattern HKWorkoutActivityTypeMixedCardio
  , pattern HKWorkoutActivityTypeHandCycling
  , pattern HKWorkoutActivityTypeDiscSports
  , pattern HKWorkoutActivityTypeFitnessGaming
  , pattern HKWorkoutActivityTypeCardioDance
  , pattern HKWorkoutActivityTypeSocialDance
  , pattern HKWorkoutActivityTypePickleball
  , pattern HKWorkoutActivityTypeCooldown
  , pattern HKWorkoutActivityTypeSwimBikeRun
  , pattern HKWorkoutActivityTypeTransition
  , pattern HKWorkoutActivityTypeUnderwaterDiving
  , pattern HKWorkoutActivityTypeOther

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | statisticsForType:
--
-- Returns an HKStatistics object containing the statistics for all the samples of the given type that                have been added to the workout. If there are no samples of the given type then nil is returned.
--
-- @quantityType@ — The quantity type to gather statistics about.
--
-- ObjC selector: @- statisticsForType:@
statisticsForType :: (IsHKWorkout hkWorkout, IsHKQuantityType quantityType) => hkWorkout -> quantityType -> IO (Id HKStatistics)
statisticsForType hkWorkout quantityType =
  sendMessage hkWorkout statisticsForTypeSelector (toHKQuantityType quantityType)

-- | workoutWithActivityType:startDate:endDate:
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:@
workoutWithActivityType_startDate_endDate :: (IsNSDate startDate, IsNSDate endDate) => HKWorkoutActivityType -> startDate -> endDate -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate workoutActivityType startDate endDate =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDateSelector workoutActivityType (toNSDate startDate) (toNSDate endDate)

-- | workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:metadata
--
-- If the optional total parameters are specified, matching samples that add up to the calculated total quantities                should be associated with this workout using addSamples:toWorkout:completion: in HKHealthStore.
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- @workoutEvents@ — An array of HKWorkoutEvents. The workout's duration is derived from these events. (Optional)
--
-- @totalEnergyBurned@ — The amount of energy that was burned during the workout. (Optional)
--
-- @totalDistance@ — The total distance that was traveled during the workout. (Optional)
--
-- @metadata@ — Metadata for the workout. (Optional)
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadata :: (IsNSDate startDate, IsNSDate endDate, IsNSArray workoutEvents, IsHKQuantity totalEnergyBurned, IsHKQuantity totalDistance, IsNSDictionary metadata) => HKWorkoutActivityType -> startDate -> endDate -> workoutEvents -> totalEnergyBurned -> totalDistance -> metadata -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadata workoutActivityType startDate endDate workoutEvents totalEnergyBurned totalDistance metadata =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadataSelector workoutActivityType (toNSDate startDate) (toNSDate endDate) (toNSArray workoutEvents) (toHKQuantity totalEnergyBurned) (toHKQuantity totalDistance) (toNSDictionary metadata)

-- | workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:metadata
--
-- If the optional total parameters are specified, matching samples that add up to the calculated total quantities                should be associated with this workout using addSamples:toWorkout:completion: in HKHealthStore.
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- @workoutEvents@ — An array of HKWorkoutEvents. The workout's duration is derived from these events. (Optional)
--
-- @totalEnergyBurned@ — The amount of energy that was burned during the workout. (Optional)
--
-- @totalDistance@ — The total distance that was traveled during the workout. (Optional)
--
-- @device@ — The HKDevice associated with the workout. (Optional)
--
-- @metadata@ — Metadata for the workout. (Optional)
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:device:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadata :: (IsNSDate startDate, IsNSDate endDate, IsNSArray workoutEvents, IsHKQuantity totalEnergyBurned, IsHKQuantity totalDistance, IsHKDevice device, IsNSDictionary metadata) => HKWorkoutActivityType -> startDate -> endDate -> workoutEvents -> totalEnergyBurned -> totalDistance -> device -> metadata -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadata workoutActivityType startDate endDate workoutEvents totalEnergyBurned totalDistance device metadata =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadataSelector workoutActivityType (toNSDate startDate) (toNSDate endDate) (toNSArray workoutEvents) (toHKQuantity totalEnergyBurned) (toHKQuantity totalDistance) (toHKDevice device) (toNSDictionary metadata)

-- | workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:metadata:
--
-- If the optional total parameters are specified, matching samples that add up to the calculated total quantities                should be associated with this workout using addSamples:toWorkout:completion: in HKHealthStore.
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- @duration@ — The duration of the workout. If 0, the difference between startDate and endDate is used.
--
-- @totalEnergyBurned@ — The amount of energy that was burned during the workout. (Optional)
--
-- @totalDistance@ — The total distance that was traveled during the workout. (Optional)
--
-- @metadata@ — Metadata for the workout. (Optional)
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:metadata:@
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadata :: (IsNSDate startDate, IsNSDate endDate, IsHKQuantity totalEnergyBurned, IsHKQuantity totalDistance, IsNSDictionary metadata) => HKWorkoutActivityType -> startDate -> endDate -> CDouble -> totalEnergyBurned -> totalDistance -> metadata -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadata workoutActivityType startDate endDate duration totalEnergyBurned totalDistance metadata =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadataSelector workoutActivityType (toNSDate startDate) (toNSDate endDate) duration (toHKQuantity totalEnergyBurned) (toHKQuantity totalDistance) (toNSDictionary metadata)

-- | workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:device:metadata:
--
-- If the optional total parameters are specified, matching samples that add up to the calculated total quantities                should be associated with this workout using addSamples:toWorkout:completion: in HKHealthStore.
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- @duration@ — The duration of the workout. If 0, the difference between startDate and endDate is used.
--
-- @totalEnergyBurned@ — The amount of energy that was burned during the workout. (Optional)
--
-- @totalDistance@ — The total distance that was traveled during the workout. (Optional)
--
-- @device@ — The HKDevice associated with the workout. (Optional)
--
-- @metadata@ — Metadata for the workout. (Optional)
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:device:metadata:@
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadata :: (IsNSDate startDate, IsNSDate endDate, IsHKQuantity totalEnergyBurned, IsHKQuantity totalDistance, IsHKDevice device, IsNSDictionary metadata) => HKWorkoutActivityType -> startDate -> endDate -> CDouble -> totalEnergyBurned -> totalDistance -> device -> metadata -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadata workoutActivityType startDate endDate duration totalEnergyBurned totalDistance device metadata =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadataSelector workoutActivityType (toNSDate startDate) (toNSDate endDate) duration (toHKQuantity totalEnergyBurned) (toHKQuantity totalDistance) (toHKDevice device) (toNSDictionary metadata)

-- | workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalSwimmingStrokeCount:device:metadata:
--
-- If the optional total parameters are specified, matching samples that add up to the calculated total quantities                should be associated with this workout using addSamples:toWorkout:completion: in HKHealthStore.
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- @workoutEvents@ — An array of HKWorkoutEvents. The workout's duration is derived from these events. (Optional)
--
-- @totalEnergyBurned@ — The amount of energy that was burned during the workout. (Optional)
--
-- @totalDistance@ — The total distance that was traveled during the workout. (Optional)
--
-- @totalSwimmingStrokeCount@ — The total count of swimming strokes that was accumulated during the workout. (Optional)
--
-- @device@ — The HKDevice associated with the workout. (Optional)
--
-- @metadata@ — Metadata for the workout. (Optional)
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalSwimmingStrokeCount:device:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadata :: (IsNSDate startDate, IsNSDate endDate, IsNSArray workoutEvents, IsHKQuantity totalEnergyBurned, IsHKQuantity totalDistance, IsHKQuantity totalSwimmingStrokeCount, IsHKDevice device, IsNSDictionary metadata) => HKWorkoutActivityType -> startDate -> endDate -> workoutEvents -> totalEnergyBurned -> totalDistance -> totalSwimmingStrokeCount -> device -> metadata -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadata workoutActivityType startDate endDate workoutEvents totalEnergyBurned totalDistance totalSwimmingStrokeCount device metadata =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadataSelector workoutActivityType (toNSDate startDate) (toNSDate endDate) (toNSArray workoutEvents) (toHKQuantity totalEnergyBurned) (toHKQuantity totalDistance) (toHKQuantity totalSwimmingStrokeCount) (toHKDevice device) (toNSDictionary metadata)

-- | workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalFlightsClimbed:device:metadata:
--
-- If the optional total parameters are specified, matching samples that add up to the calculated total                quantities should be associated with this workout using addSamples:toWorkout:completion: in                HKHealthStore.
--
-- @workoutActivityType@ — The activity type of the workout
--
-- @startDate@ — The point in time that the workout was started
--
-- @endDate@ — The point in time that the workout was ended
--
-- @workoutEvents@ — An array of HKWorkoutEvents. The workout's duration is derived from these events. (Optional)
--
-- @totalEnergyBurned@ — The amount of energy that was burned during the workout. (Optional)
--
-- @totalDistance@ — The total distance that was traveled during the workout. (Optional)
--
-- @totalFlightsClimbed@ — The total count of flights climbed that was accumulated during the workout. (Optional)
--
-- @device@ — The HKDevice associated with the workout. (Optional)
--
-- @metadata@ — Metadata for the workout. (Optional)
--
-- ObjC selector: @+ workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalFlightsClimbed:device:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadata :: (IsNSDate startDate, IsNSDate endDate, IsNSArray workoutEvents, IsHKQuantity totalEnergyBurned, IsHKQuantity totalDistance, IsHKQuantity totalFlightsClimbed, IsHKDevice device, IsNSDictionary metadata) => HKWorkoutActivityType -> startDate -> endDate -> workoutEvents -> totalEnergyBurned -> totalDistance -> totalFlightsClimbed -> device -> metadata -> IO (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadata workoutActivityType startDate endDate workoutEvents totalEnergyBurned totalDistance totalFlightsClimbed device metadata =
  do
    cls' <- getRequiredClass "HKWorkout"
    sendClassMessage cls' workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadataSelector workoutActivityType (toNSDate startDate) (toNSDate endDate) (toNSArray workoutEvents) (toHKQuantity totalEnergyBurned) (toHKQuantity totalDistance) (toHKQuantity totalFlightsClimbed) (toHKDevice device) (toNSDictionary metadata)

-- | workoutActivityType
--
-- Represents the activity that the user was performing during a workout
--
-- ObjC selector: @- workoutActivityType@
workoutActivityType :: IsHKWorkout hkWorkout => hkWorkout -> IO HKWorkoutActivityType
workoutActivityType hkWorkout =
  sendMessage hkWorkout workoutActivityTypeSelector

-- | workoutEvents
--
-- An array of HKWorkoutEvents that occurred during a workout.
--
-- These events will be ordered by date in ascending order. All events must take place                between the start date and end date of the workout. The first workout event should never be a resume event                because it is assumed that the workout begins in a running state.
--
-- ObjC selector: @- workoutEvents@
workoutEvents :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id NSArray)
workoutEvents hkWorkout =
  sendMessage hkWorkout workoutEventsSelector

-- | workoutActivities
--
-- An array of HKWorkoutActivities that were performed during a workout.
--
-- These activities will be ordered by date in ascending order. All activities must take place                between the start date and end date of the workout.
--
-- ObjC selector: @- workoutActivities@
workoutActivities :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id NSArray)
workoutActivities hkWorkout =
  sendMessage hkWorkout workoutActivitiesSelector

-- | duration
--
-- The length of time that a workout was recording
--
-- The duration is derived from the start and end dates of the workout and takes into account periods that the                workout was paused. Periods that the workout was paused are based off of the workoutEvents property.
--
-- ObjC selector: @- duration@
duration :: IsHKWorkout hkWorkout => hkWorkout -> IO CDouble
duration hkWorkout =
  sendMessage hkWorkout durationSelector

-- | totalEnergyBurned
--
-- The amount of energy that was burned during a workout
--
-- This metric should represent the total active energy burned during the course of the workout. It should be a                quantity with a unit representing energy.
--
-- ObjC selector: @- totalEnergyBurned@
totalEnergyBurned :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id HKQuantity)
totalEnergyBurned hkWorkout =
  sendMessage hkWorkout totalEnergyBurnedSelector

-- | totalDistance
--
-- The total distance that was traveled during a workout
--
-- This metric should represent the total distance traveled during the course of the workout. It should be a                quantity with a unit representing length.
--
-- ObjC selector: @- totalDistance@
totalDistance :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id HKQuantity)
totalDistance hkWorkout =
  sendMessage hkWorkout totalDistanceSelector

-- | totalSwimmingStrokeCount
--
-- The total count of swimming strokes that was accumulated during a workout
--
-- This metric should represent the total count of swimming strokes accumulated during the course of the                workout. It should be a quantity with a unit representing count.
--
-- ObjC selector: @- totalSwimmingStrokeCount@
totalSwimmingStrokeCount :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id HKQuantity)
totalSwimmingStrokeCount hkWorkout =
  sendMessage hkWorkout totalSwimmingStrokeCountSelector

-- | totalFlightsClimbed
--
-- The total count of flights climbed during a workout
--
-- This metric should represent the total count of flights accumulated during the course of the                workout. It should be a quantity with a unit representing count.
--
-- ObjC selector: @- totalFlightsClimbed@
totalFlightsClimbed :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id HKQuantity)
totalFlightsClimbed hkWorkout =
  sendMessage hkWorkout totalFlightsClimbedSelector

-- | allStatistics
--
-- A dictionary of statistics per quantity type during the workout
--
-- This dictionary will contain HKStatistics objects containing the statistics by quantity                sample type for all of the samples that have been added to the workout.
--
-- ObjC selector: @- allStatistics@
allStatistics :: IsHKWorkout hkWorkout => hkWorkout -> IO (Id NSDictionary)
allStatistics hkWorkout =
  sendMessage hkWorkout allStatisticsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @statisticsForType:@
statisticsForTypeSelector :: Selector '[Id HKQuantityType] (Id HKStatistics)
statisticsForTypeSelector = mkSelector "statisticsForType:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:@
workoutWithActivityType_startDate_endDateSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate] (Id HKWorkout)
workoutWithActivityType_startDate_endDateSelector = mkSelector "workoutWithActivityType:startDate:endDate:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadataSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate, Id NSArray, Id HKQuantity, Id HKQuantity, Id NSDictionary] (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_metadataSelector = mkSelector "workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:metadata:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:device:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadataSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate, Id NSArray, Id HKQuantity, Id HKQuantity, Id HKDevice, Id NSDictionary] (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_device_metadataSelector = mkSelector "workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:device:metadata:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:metadata:@
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadataSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate, CDouble, Id HKQuantity, Id HKQuantity, Id NSDictionary] (Id HKWorkout)
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_metadataSelector = mkSelector "workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:metadata:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:device:metadata:@
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadataSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate, CDouble, Id HKQuantity, Id HKQuantity, Id HKDevice, Id NSDictionary] (Id HKWorkout)
workoutWithActivityType_startDate_endDate_duration_totalEnergyBurned_totalDistance_device_metadataSelector = mkSelector "workoutWithActivityType:startDate:endDate:duration:totalEnergyBurned:totalDistance:device:metadata:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalSwimmingStrokeCount:device:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadataSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate, Id NSArray, Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKDevice, Id NSDictionary] (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalSwimmingStrokeCount_device_metadataSelector = mkSelector "workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalSwimmingStrokeCount:device:metadata:"

-- | @Selector@ for @workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalFlightsClimbed:device:metadata:@
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadataSelector :: Selector '[HKWorkoutActivityType, Id NSDate, Id NSDate, Id NSArray, Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKDevice, Id NSDictionary] (Id HKWorkout)
workoutWithActivityType_startDate_endDate_workoutEvents_totalEnergyBurned_totalDistance_totalFlightsClimbed_device_metadataSelector = mkSelector "workoutWithActivityType:startDate:endDate:workoutEvents:totalEnergyBurned:totalDistance:totalFlightsClimbed:device:metadata:"

-- | @Selector@ for @workoutActivityType@
workoutActivityTypeSelector :: Selector '[] HKWorkoutActivityType
workoutActivityTypeSelector = mkSelector "workoutActivityType"

-- | @Selector@ for @workoutEvents@
workoutEventsSelector :: Selector '[] (Id NSArray)
workoutEventsSelector = mkSelector "workoutEvents"

-- | @Selector@ for @workoutActivities@
workoutActivitiesSelector :: Selector '[] (Id NSArray)
workoutActivitiesSelector = mkSelector "workoutActivities"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @totalEnergyBurned@
totalEnergyBurnedSelector :: Selector '[] (Id HKQuantity)
totalEnergyBurnedSelector = mkSelector "totalEnergyBurned"

-- | @Selector@ for @totalDistance@
totalDistanceSelector :: Selector '[] (Id HKQuantity)
totalDistanceSelector = mkSelector "totalDistance"

-- | @Selector@ for @totalSwimmingStrokeCount@
totalSwimmingStrokeCountSelector :: Selector '[] (Id HKQuantity)
totalSwimmingStrokeCountSelector = mkSelector "totalSwimmingStrokeCount"

-- | @Selector@ for @totalFlightsClimbed@
totalFlightsClimbedSelector :: Selector '[] (Id HKQuantity)
totalFlightsClimbedSelector = mkSelector "totalFlightsClimbed"

-- | @Selector@ for @allStatistics@
allStatisticsSelector :: Selector '[] (Id NSDictionary)
allStatisticsSelector = mkSelector "allStatistics"

