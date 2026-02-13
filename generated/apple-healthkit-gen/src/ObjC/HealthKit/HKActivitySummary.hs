{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKActivitySummary
--
-- An object that represents a summary of a user's activity for a given day.
--
-- Generated bindings for @HKActivitySummary@.
module ObjC.HealthKit.HKActivitySummary
  ( HKActivitySummary
  , IsHKActivitySummary(..)
  , dateComponentsForCalendar
  , activityMoveMode
  , setActivityMoveMode
  , paused
  , setPaused
  , activeEnergyBurned
  , setActiveEnergyBurned
  , appleMoveTime
  , setAppleMoveTime
  , appleExerciseTime
  , setAppleExerciseTime
  , appleStandHours
  , setAppleStandHours
  , activeEnergyBurnedGoal
  , setActiveEnergyBurnedGoal
  , appleMoveTimeGoal
  , setAppleMoveTimeGoal
  , appleExerciseTimeGoal
  , setAppleExerciseTimeGoal
  , exerciseTimeGoal
  , setExerciseTimeGoal
  , appleStandHoursGoal
  , setAppleStandHoursGoal
  , standHoursGoal
  , setStandHoursGoal
  , activeEnergyBurnedGoalSelector
  , activeEnergyBurnedSelector
  , activityMoveModeSelector
  , appleExerciseTimeGoalSelector
  , appleExerciseTimeSelector
  , appleMoveTimeGoalSelector
  , appleMoveTimeSelector
  , appleStandHoursGoalSelector
  , appleStandHoursSelector
  , dateComponentsForCalendarSelector
  , exerciseTimeGoalSelector
  , pausedSelector
  , setActiveEnergyBurnedGoalSelector
  , setActiveEnergyBurnedSelector
  , setActivityMoveModeSelector
  , setAppleExerciseTimeGoalSelector
  , setAppleExerciseTimeSelector
  , setAppleMoveTimeGoalSelector
  , setAppleMoveTimeSelector
  , setAppleStandHoursGoalSelector
  , setAppleStandHoursSelector
  , setExerciseTimeGoalSelector
  , setPausedSelector
  , setStandHoursGoalSelector
  , standHoursGoalSelector

  -- * Enum types
  , HKActivityMoveMode(HKActivityMoveMode)
  , pattern HKActivityMoveModeActiveEnergy
  , pattern HKActivityMoveModeAppleMoveTime

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

-- | dateComponentsForCalendar:
--
-- The date components representing the day for this particular HKActivitySummary.
--
-- These date components will contain era, year, month, and day components in the provided calendar.
--
-- ObjC selector: @- dateComponentsForCalendar:@
dateComponentsForCalendar :: (IsHKActivitySummary hkActivitySummary, IsNSCalendar calendar) => hkActivitySummary -> calendar -> IO (Id NSDateComponents)
dateComponentsForCalendar hkActivitySummary calendar =
  sendMessage hkActivitySummary dateComponentsForCalendarSelector (toNSCalendar calendar)

-- | activityMoveMode
--
-- The move mode of this activity summary
--
-- The move mode of an activity summary determines if activeEnergyBurned or appleMoveTime are used for the move ring.
--
-- ObjC selector: @- activityMoveMode@
activityMoveMode :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO HKActivityMoveMode
activityMoveMode hkActivitySummary =
  sendMessage hkActivitySummary activityMoveModeSelector

-- | activityMoveMode
--
-- The move mode of this activity summary
--
-- The move mode of an activity summary determines if activeEnergyBurned or appleMoveTime are used for the move ring.
--
-- ObjC selector: @- setActivityMoveMode:@
setActivityMoveMode :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> HKActivityMoveMode -> IO ()
setActivityMoveMode hkActivitySummary value =
  sendMessage hkActivitySummary setActivityMoveModeSelector value

-- | paused
--
-- The paused state of this activity summary
--
-- The paused state of an activity summary indicates if the user is tracking their rings for the given day.
--
-- ObjC selector: @- paused@
paused :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO Bool
paused hkActivitySummary =
  sendMessage hkActivitySummary pausedSelector

-- | paused
--
-- The paused state of this activity summary
--
-- The paused state of an activity summary indicates if the user is tracking their rings for the given day.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> Bool -> IO ()
setPaused hkActivitySummary value =
  sendMessage hkActivitySummary setPausedSelector value

-- | activeEnergyBurned
--
-- The amount of active energy that the user burned.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- activeEnergyBurned@
activeEnergyBurned :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
activeEnergyBurned hkActivitySummary =
  sendMessage hkActivitySummary activeEnergyBurnedSelector

-- | activeEnergyBurned
--
-- The amount of active energy that the user burned.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- setActiveEnergyBurned:@
setActiveEnergyBurned :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setActiveEnergyBurned hkActivitySummary value =
  sendMessage hkActivitySummary setActiveEnergyBurnedSelector (toHKQuantity value)

-- | appleMoveTime
--
-- The amount of move time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 move time time is defined by Apple.
--
-- ObjC selector: @- appleMoveTime@
appleMoveTime :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleMoveTime hkActivitySummary =
  sendMessage hkActivitySummary appleMoveTimeSelector

-- | appleMoveTime
--
-- The amount of move time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 move time time is defined by Apple.
--
-- ObjC selector: @- setAppleMoveTime:@
setAppleMoveTime :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleMoveTime hkActivitySummary value =
  sendMessage hkActivitySummary setAppleMoveTimeSelector (toHKQuantity value)

-- | appleExerciseTime
--
-- The amount of exercise time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 exercise time is defined by Apple.
--
-- ObjC selector: @- appleExerciseTime@
appleExerciseTime :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleExerciseTime hkActivitySummary =
  sendMessage hkActivitySummary appleExerciseTimeSelector

-- | appleExerciseTime
--
-- The amount of exercise time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 exercise time is defined by Apple.
--
-- ObjC selector: @- setAppleExerciseTime:@
setAppleExerciseTime :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleExerciseTime hkActivitySummary value =
  sendMessage hkActivitySummary setAppleExerciseTimeSelector (toHKQuantity value)

-- | appleStandHours
--
-- The number of stand hours that the user earned.
--
-- This quantity is compatible with the count unit. The measurement criteria of                 stand hours is defined by Apple.
--
-- ObjC selector: @- appleStandHours@
appleStandHours :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleStandHours hkActivitySummary =
  sendMessage hkActivitySummary appleStandHoursSelector

-- | appleStandHours
--
-- The number of stand hours that the user earned.
--
-- This quantity is compatible with the count unit. The measurement criteria of                 stand hours is defined by Apple.
--
-- ObjC selector: @- setAppleStandHours:@
setAppleStandHours :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleStandHours hkActivitySummary value =
  sendMessage hkActivitySummary setAppleStandHoursSelector (toHKQuantity value)

-- | activeEnergyBurnedGoal
--
-- The user's active energy goal for the day.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- activeEnergyBurnedGoal@
activeEnergyBurnedGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
activeEnergyBurnedGoal hkActivitySummary =
  sendMessage hkActivitySummary activeEnergyBurnedGoalSelector

-- | activeEnergyBurnedGoal
--
-- The user's active energy goal for the day.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- setActiveEnergyBurnedGoal:@
setActiveEnergyBurnedGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setActiveEnergyBurnedGoal hkActivitySummary value =
  sendMessage hkActivitySummary setActiveEnergyBurnedGoalSelector (toHKQuantity value)

-- | appleMoveTimeGoal
--
-- The user's move time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- appleMoveTimeGoal@
appleMoveTimeGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleMoveTimeGoal hkActivitySummary =
  sendMessage hkActivitySummary appleMoveTimeGoalSelector

-- | appleMoveTimeGoal
--
-- The user's move time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- setAppleMoveTimeGoal:@
setAppleMoveTimeGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleMoveTimeGoal hkActivitySummary value =
  sendMessage hkActivitySummary setAppleMoveTimeGoalSelector (toHKQuantity value)

-- | appleExerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- appleExerciseTimeGoal@
appleExerciseTimeGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleExerciseTimeGoal hkActivitySummary =
  sendMessage hkActivitySummary appleExerciseTimeGoalSelector

-- | appleExerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- setAppleExerciseTimeGoal:@
setAppleExerciseTimeGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleExerciseTimeGoal hkActivitySummary value =
  sendMessage hkActivitySummary setAppleExerciseTimeGoalSelector (toHKQuantity value)

-- | exerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- exerciseTimeGoal@
exerciseTimeGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
exerciseTimeGoal hkActivitySummary =
  sendMessage hkActivitySummary exerciseTimeGoalSelector

-- | exerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- setExerciseTimeGoal:@
setExerciseTimeGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setExerciseTimeGoal hkActivitySummary value =
  sendMessage hkActivitySummary setExerciseTimeGoalSelector (toHKQuantity value)

-- | appleStandHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- appleStandHoursGoal@
appleStandHoursGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleStandHoursGoal hkActivitySummary =
  sendMessage hkActivitySummary appleStandHoursGoalSelector

-- | appleStandHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- setAppleStandHoursGoal:@
setAppleStandHoursGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleStandHoursGoal hkActivitySummary value =
  sendMessage hkActivitySummary setAppleStandHoursGoalSelector (toHKQuantity value)

-- | standHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- standHoursGoal@
standHoursGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
standHoursGoal hkActivitySummary =
  sendMessage hkActivitySummary standHoursGoalSelector

-- | standHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- setStandHoursGoal:@
setStandHoursGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setStandHoursGoal hkActivitySummary value =
  sendMessage hkActivitySummary setStandHoursGoalSelector (toHKQuantity value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dateComponentsForCalendar:@
dateComponentsForCalendarSelector :: Selector '[Id NSCalendar] (Id NSDateComponents)
dateComponentsForCalendarSelector = mkSelector "dateComponentsForCalendar:"

-- | @Selector@ for @activityMoveMode@
activityMoveModeSelector :: Selector '[] HKActivityMoveMode
activityMoveModeSelector = mkSelector "activityMoveMode"

-- | @Selector@ for @setActivityMoveMode:@
setActivityMoveModeSelector :: Selector '[HKActivityMoveMode] ()
setActivityMoveModeSelector = mkSelector "setActivityMoveMode:"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @activeEnergyBurned@
activeEnergyBurnedSelector :: Selector '[] (Id HKQuantity)
activeEnergyBurnedSelector = mkSelector "activeEnergyBurned"

-- | @Selector@ for @setActiveEnergyBurned:@
setActiveEnergyBurnedSelector :: Selector '[Id HKQuantity] ()
setActiveEnergyBurnedSelector = mkSelector "setActiveEnergyBurned:"

-- | @Selector@ for @appleMoveTime@
appleMoveTimeSelector :: Selector '[] (Id HKQuantity)
appleMoveTimeSelector = mkSelector "appleMoveTime"

-- | @Selector@ for @setAppleMoveTime:@
setAppleMoveTimeSelector :: Selector '[Id HKQuantity] ()
setAppleMoveTimeSelector = mkSelector "setAppleMoveTime:"

-- | @Selector@ for @appleExerciseTime@
appleExerciseTimeSelector :: Selector '[] (Id HKQuantity)
appleExerciseTimeSelector = mkSelector "appleExerciseTime"

-- | @Selector@ for @setAppleExerciseTime:@
setAppleExerciseTimeSelector :: Selector '[Id HKQuantity] ()
setAppleExerciseTimeSelector = mkSelector "setAppleExerciseTime:"

-- | @Selector@ for @appleStandHours@
appleStandHoursSelector :: Selector '[] (Id HKQuantity)
appleStandHoursSelector = mkSelector "appleStandHours"

-- | @Selector@ for @setAppleStandHours:@
setAppleStandHoursSelector :: Selector '[Id HKQuantity] ()
setAppleStandHoursSelector = mkSelector "setAppleStandHours:"

-- | @Selector@ for @activeEnergyBurnedGoal@
activeEnergyBurnedGoalSelector :: Selector '[] (Id HKQuantity)
activeEnergyBurnedGoalSelector = mkSelector "activeEnergyBurnedGoal"

-- | @Selector@ for @setActiveEnergyBurnedGoal:@
setActiveEnergyBurnedGoalSelector :: Selector '[Id HKQuantity] ()
setActiveEnergyBurnedGoalSelector = mkSelector "setActiveEnergyBurnedGoal:"

-- | @Selector@ for @appleMoveTimeGoal@
appleMoveTimeGoalSelector :: Selector '[] (Id HKQuantity)
appleMoveTimeGoalSelector = mkSelector "appleMoveTimeGoal"

-- | @Selector@ for @setAppleMoveTimeGoal:@
setAppleMoveTimeGoalSelector :: Selector '[Id HKQuantity] ()
setAppleMoveTimeGoalSelector = mkSelector "setAppleMoveTimeGoal:"

-- | @Selector@ for @appleExerciseTimeGoal@
appleExerciseTimeGoalSelector :: Selector '[] (Id HKQuantity)
appleExerciseTimeGoalSelector = mkSelector "appleExerciseTimeGoal"

-- | @Selector@ for @setAppleExerciseTimeGoal:@
setAppleExerciseTimeGoalSelector :: Selector '[Id HKQuantity] ()
setAppleExerciseTimeGoalSelector = mkSelector "setAppleExerciseTimeGoal:"

-- | @Selector@ for @exerciseTimeGoal@
exerciseTimeGoalSelector :: Selector '[] (Id HKQuantity)
exerciseTimeGoalSelector = mkSelector "exerciseTimeGoal"

-- | @Selector@ for @setExerciseTimeGoal:@
setExerciseTimeGoalSelector :: Selector '[Id HKQuantity] ()
setExerciseTimeGoalSelector = mkSelector "setExerciseTimeGoal:"

-- | @Selector@ for @appleStandHoursGoal@
appleStandHoursGoalSelector :: Selector '[] (Id HKQuantity)
appleStandHoursGoalSelector = mkSelector "appleStandHoursGoal"

-- | @Selector@ for @setAppleStandHoursGoal:@
setAppleStandHoursGoalSelector :: Selector '[Id HKQuantity] ()
setAppleStandHoursGoalSelector = mkSelector "setAppleStandHoursGoal:"

-- | @Selector@ for @standHoursGoal@
standHoursGoalSelector :: Selector '[] (Id HKQuantity)
standHoursGoalSelector = mkSelector "standHoursGoal"

-- | @Selector@ for @setStandHoursGoal:@
setStandHoursGoalSelector :: Selector '[Id HKQuantity] ()
setStandHoursGoalSelector = mkSelector "setStandHoursGoal:"

