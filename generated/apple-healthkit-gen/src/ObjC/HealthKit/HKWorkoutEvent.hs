{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutEvent
--
-- Represents a particular event that occurred during a workout.
--
-- Generated bindings for @HKWorkoutEvent@.
module ObjC.HealthKit.HKWorkoutEvent
  ( HKWorkoutEvent
  , IsHKWorkoutEvent(..)
  , workoutEventWithType_date
  , workoutEventWithType_date_metadata
  , workoutEventWithType_dateInterval_metadata
  , init_
  , type_
  , date
  , dateInterval
  , metadata
  , dateIntervalSelector
  , dateSelector
  , initSelector
  , metadataSelector
  , typeSelector
  , workoutEventWithType_dateInterval_metadataSelector
  , workoutEventWithType_dateSelector
  , workoutEventWithType_date_metadataSelector

  -- * Enum types
  , HKWorkoutEventType(HKWorkoutEventType)
  , pattern HKWorkoutEventTypePause
  , pattern HKWorkoutEventTypeResume
  , pattern HKWorkoutEventTypeLap
  , pattern HKWorkoutEventTypeMarker
  , pattern HKWorkoutEventTypeMotionPaused
  , pattern HKWorkoutEventTypeMotionResumed
  , pattern HKWorkoutEventTypeSegment
  , pattern HKWorkoutEventTypePauseOrResumeRequest

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

-- | @+ workoutEventWithType:date:@
workoutEventWithType_date :: IsNSDate date => HKWorkoutEventType -> date -> IO (Id HKWorkoutEvent)
workoutEventWithType_date type_ date =
  do
    cls' <- getRequiredClass "HKWorkoutEvent"
    sendClassMessage cls' workoutEventWithType_dateSelector type_ (toNSDate date)

-- | @+ workoutEventWithType:date:metadata:@
workoutEventWithType_date_metadata :: (IsNSDate date, IsNSDictionary metadata) => HKWorkoutEventType -> date -> metadata -> IO (Id HKWorkoutEvent)
workoutEventWithType_date_metadata type_ date metadata =
  do
    cls' <- getRequiredClass "HKWorkoutEvent"
    sendClassMessage cls' workoutEventWithType_date_metadataSelector type_ (toNSDate date) (toNSDictionary metadata)

-- | workoutEventWithType:dateInterval:metadata:
--
-- Creates an event with a date interval with or without a duration.
--
-- @type@ — The type of event to create
--
-- @dateInterval@ — The dateInterval over which the event occurs
--
-- @metadata@ — Dictionary of metadata associated with the event, nullable
--
-- ObjC selector: @+ workoutEventWithType:dateInterval:metadata:@
workoutEventWithType_dateInterval_metadata :: (IsNSDateInterval dateInterval, IsNSDictionary metadata) => HKWorkoutEventType -> dateInterval -> metadata -> IO (Id HKWorkoutEvent)
workoutEventWithType_dateInterval_metadata type_ dateInterval metadata =
  do
    cls' <- getRequiredClass "HKWorkoutEvent"
    sendClassMessage cls' workoutEventWithType_dateInterval_metadataSelector type_ (toNSDateInterval dateInterval) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id HKWorkoutEvent)
init_ hkWorkoutEvent =
  sendOwnedMessage hkWorkoutEvent initSelector

-- | workoutEventType
--
-- Represents the type of event that occurred during a workout.
--
-- ObjC selector: @- type@
type_ :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO HKWorkoutEventType
type_ hkWorkoutEvent =
  sendMessage hkWorkoutEvent typeSelector

-- | @- date@
date :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id NSDate)
date hkWorkoutEvent =
  sendMessage hkWorkoutEvent dateSelector

-- | dateInterval
--
-- Date interval representing the time period for which the event is valid.
--
-- Most event types only support date intervals with zero duration. Events of type HKWorkoutEventTypeLap                and HKWorkoutEventTypeSegment are currently the only events that support a nonzero duration.
--
-- ObjC selector: @- dateInterval@
dateInterval :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id NSDateInterval)
dateInterval hkWorkoutEvent =
  sendMessage hkWorkoutEvent dateIntervalSelector

-- | metadata
--
-- Extra information describing properties of the receiver.
--
-- Keys must be NSString and values must be either NSString, NSNumber, NSDate, or                HKQuantity. See HKMetadata.h for potential metadata keys and values.
--
-- ObjC selector: @- metadata@
metadata :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id NSDictionary)
metadata hkWorkoutEvent =
  sendMessage hkWorkoutEvent metadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workoutEventWithType:date:@
workoutEventWithType_dateSelector :: Selector '[HKWorkoutEventType, Id NSDate] (Id HKWorkoutEvent)
workoutEventWithType_dateSelector = mkSelector "workoutEventWithType:date:"

-- | @Selector@ for @workoutEventWithType:date:metadata:@
workoutEventWithType_date_metadataSelector :: Selector '[HKWorkoutEventType, Id NSDate, Id NSDictionary] (Id HKWorkoutEvent)
workoutEventWithType_date_metadataSelector = mkSelector "workoutEventWithType:date:metadata:"

-- | @Selector@ for @workoutEventWithType:dateInterval:metadata:@
workoutEventWithType_dateInterval_metadataSelector :: Selector '[HKWorkoutEventType, Id NSDateInterval, Id NSDictionary] (Id HKWorkoutEvent)
workoutEventWithType_dateInterval_metadataSelector = mkSelector "workoutEventWithType:dateInterval:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKWorkoutEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @type@
typeSelector :: Selector '[] HKWorkoutEventType
typeSelector = mkSelector "type"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @dateInterval@
dateIntervalSelector :: Selector '[] (Id NSDateInterval)
dateIntervalSelector = mkSelector "dateInterval"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

