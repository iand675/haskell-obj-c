{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutActivity
--
-- An HKWorkoutActivity is an object describing the properties of an activity within an HKWorkout.
--
-- Generated bindings for @HKWorkoutActivity@.
module ObjC.HealthKit.HKWorkoutActivity
  ( HKWorkoutActivity
  , IsHKWorkoutActivity(..)
  , statisticsForType
  , init_
  , new
  , initWithWorkoutConfiguration_startDate_endDate_metadata
  , uuid
  , workoutConfiguration
  , startDate
  , endDate
  , metadata
  , duration
  , workoutEvents
  , allStatistics
  , allStatisticsSelector
  , durationSelector
  , endDateSelector
  , initSelector
  , initWithWorkoutConfiguration_startDate_endDate_metadataSelector
  , metadataSelector
  , newSelector
  , startDateSelector
  , statisticsForTypeSelector
  , uuidSelector
  , workoutConfigurationSelector
  , workoutEventsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | statisticsForType:
--
-- Returns an HKStatistics object containing the statistics for all the samples of the given type that                have been added to the workout within the date interval of this activity. If there are no samples of                the given type then nil is returned.
--
-- @quantityType@ — The quantity type to gather statistics about.
--
-- ObjC selector: @- statisticsForType:@
statisticsForType :: (IsHKWorkoutActivity hkWorkoutActivity, IsHKQuantityType quantityType) => hkWorkoutActivity -> quantityType -> IO (Id HKStatistics)
statisticsForType hkWorkoutActivity quantityType =
  sendMessage hkWorkoutActivity statisticsForTypeSelector (toHKQuantityType quantityType)

-- | @- init@
init_ :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id HKWorkoutActivity)
init_ hkWorkoutActivity =
  sendOwnedMessage hkWorkoutActivity initSelector

-- | @+ new@
new :: IO (Id HKWorkoutActivity)
new  =
  do
    cls' <- getRequiredClass "HKWorkoutActivity"
    sendOwnedClassMessage cls' newSelector

-- | initWithWorkoutConfiguration:startDate:endDate:metadata:
--
-- Initialize a new HKWorkoutActivity with the specified values.
--
-- @workoutConfiguration@ — The configuration object describing the workout activity.
--
-- @startDate@ — The point in time when the workout activity was started.
--
-- @endDate@ — The point in time when the workout activity was ended.
--
-- @metadata@ — Metadata for the workout activity. (Optional)
--
-- ObjC selector: @- initWithWorkoutConfiguration:startDate:endDate:metadata:@
initWithWorkoutConfiguration_startDate_endDate_metadata :: (IsHKWorkoutActivity hkWorkoutActivity, IsHKWorkoutConfiguration workoutConfiguration, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata) => hkWorkoutActivity -> workoutConfiguration -> startDate -> endDate -> metadata -> IO (Id HKWorkoutActivity)
initWithWorkoutConfiguration_startDate_endDate_metadata hkWorkoutActivity workoutConfiguration startDate endDate metadata =
  sendOwnedMessage hkWorkoutActivity initWithWorkoutConfiguration_startDate_endDate_metadataSelector (toHKWorkoutConfiguration workoutConfiguration) (toNSDate startDate) (toNSDate endDate) (toNSDictionary metadata)

-- | UUID
--
-- A unique identifier of the activity in the HealthKit database.
--
-- ObjC selector: @- UUID@
uuid :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id NSUUID)
uuid hkWorkoutActivity =
  sendMessage hkWorkoutActivity uuidSelector

-- | workoutConfiguration
--
-- The configuration object describing the workout activity.
--
-- ObjC selector: @- workoutConfiguration@
workoutConfiguration :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id HKWorkoutConfiguration)
workoutConfiguration hkWorkoutActivity =
  sendMessage hkWorkoutActivity workoutConfigurationSelector

-- | startDate
--
-- The point in time when the workout activity was started.
--
-- ObjC selector: @- startDate@
startDate :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id NSDate)
startDate hkWorkoutActivity =
  sendMessage hkWorkoutActivity startDateSelector

-- | endDate
--
-- The point in time when the workout activity was ended.
--
-- This value is nil when a workout activity is in progress.
--
-- ObjC selector: @- endDate@
endDate :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id NSDate)
endDate hkWorkoutActivity =
  sendMessage hkWorkoutActivity endDateSelector

-- | metadata
--
-- Extra information describing properties of the workout activity.
--
-- Keys must be NSString and values must be either NSString, NSNumber, NSDate, or                HKQuantity. See HKMetadata.h for potential metadata keys and values.
--
-- ObjC selector: @- metadata@
metadata :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id NSDictionary)
metadata hkWorkoutActivity =
  sendMessage hkWorkoutActivity metadataSelector

-- | duration
--
-- The length of time that the workout activity was recording
--
-- The duration is derived from the start and end dates of the activity and takes                into account periods that the activity was paused. Periods that the activity was                paused are based off of the workoutEvents property of the parent workout object.
--
-- ObjC selector: @- duration@
duration :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO CDouble
duration hkWorkoutActivity =
  sendMessage hkWorkoutActivity durationSelector

-- | workoutEvents
--
-- An array of HKWorkoutEvents that occurred during the workout activity.
--
-- These events will be ordered by date in ascending order. These events are a subset                of the workout events that take place between the start date and end date of the                activity. This includes any event that overlaps the activity, even partially.                Consequently, some events may be included in more than one activity.
--
-- ObjC selector: @- workoutEvents@
workoutEvents :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id NSArray)
workoutEvents hkWorkoutActivity =
  sendMessage hkWorkoutActivity workoutEventsSelector

-- | allStatistics
--
-- A dictionary of statistics per quantity type during the activity
--
-- This dictionary will contain HKStatistics objects containing the statistics by quantity                sample type for all of the samples that have been added to the workout within the date                interval of this activity.
--
-- ObjC selector: @- allStatistics@
allStatistics :: IsHKWorkoutActivity hkWorkoutActivity => hkWorkoutActivity -> IO (Id NSDictionary)
allStatistics hkWorkoutActivity =
  sendMessage hkWorkoutActivity allStatisticsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @statisticsForType:@
statisticsForTypeSelector :: Selector '[Id HKQuantityType] (Id HKStatistics)
statisticsForTypeSelector = mkSelector "statisticsForType:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKWorkoutActivity)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKWorkoutActivity)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithWorkoutConfiguration:startDate:endDate:metadata:@
initWithWorkoutConfiguration_startDate_endDate_metadataSelector :: Selector '[Id HKWorkoutConfiguration, Id NSDate, Id NSDate, Id NSDictionary] (Id HKWorkoutActivity)
initWithWorkoutConfiguration_startDate_endDate_metadataSelector = mkSelector "initWithWorkoutConfiguration:startDate:endDate:metadata:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @workoutConfiguration@
workoutConfigurationSelector :: Selector '[] (Id HKWorkoutConfiguration)
workoutConfigurationSelector = mkSelector "workoutConfiguration"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @workoutEvents@
workoutEventsSelector :: Selector '[] (Id NSArray)
workoutEventsSelector = mkSelector "workoutEvents"

-- | @Selector@ for @allStatistics@
allStatisticsSelector :: Selector '[] (Id NSDictionary)
allStatisticsSelector = mkSelector "allStatistics"

