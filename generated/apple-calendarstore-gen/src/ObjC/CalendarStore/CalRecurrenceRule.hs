{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalRecurrenceRule@.
module ObjC.CalendarStore.CalRecurrenceRule
  ( CalRecurrenceRule
  , IsCalRecurrenceRule(..)
  , initDailyRecurrenceWithInterval_end
  , initWeeklyRecurrenceWithInterval_end
  , initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_end
  , initMonthlyRecurrenceWithInterval_end
  , initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_end
  , initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_end
  , initYearlyRecurrenceWithInterval_end
  , initYearlyRecurrenceWithInterval_forMonthsOfTheYear_end
  , initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_end
  , recurrenceEnd
  , recurrenceInterval
  , firstDayOfTheWeek
  , daysOfTheWeek
  , daysOfTheMonth
  , nthWeekDaysOfTheMonth
  , monthsOfTheYear
  , daysOfTheMonthSelector
  , daysOfTheWeekSelector
  , firstDayOfTheWeekSelector
  , initDailyRecurrenceWithInterval_endSelector
  , initMonthlyRecurrenceWithInterval_endSelector
  , initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector
  , initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector
  , initWeeklyRecurrenceWithInterval_endSelector
  , initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector
  , initYearlyRecurrenceWithInterval_endSelector
  , initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector
  , initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector
  , monthsOfTheYearSelector
  , nthWeekDaysOfTheMonthSelector
  , recurrenceEndSelector
  , recurrenceIntervalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initDailyRecurrenceWithInterval:end:@
initDailyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initDailyRecurrenceWithInterval_end calRecurrenceRule interval end =
  sendOwnedMessage calRecurrenceRule initDailyRecurrenceWithInterval_endSelector interval (toCalRecurrenceEnd end)

-- | @- initWeeklyRecurrenceWithInterval:end:@
initWeeklyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initWeeklyRecurrenceWithInterval_end calRecurrenceRule interval end =
  sendOwnedMessage calRecurrenceRule initWeeklyRecurrenceWithInterval_endSelector interval (toCalRecurrenceEnd end)

-- | @- initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:@
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray days, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> days -> end -> IO RawId
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_end calRecurrenceRule interval days end =
  sendOwnedMessage calRecurrenceRule initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector interval (toNSArray days) (toCalRecurrenceEnd end)

-- | @- initMonthlyRecurrenceWithInterval:end:@
initMonthlyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initMonthlyRecurrenceWithInterval_end calRecurrenceRule interval end =
  sendOwnedMessage calRecurrenceRule initMonthlyRecurrenceWithInterval_endSelector interval (toCalRecurrenceEnd end)

-- | @- initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray monthDays, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> monthDays -> end -> IO RawId
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_end calRecurrenceRule interval monthDays end =
  sendOwnedMessage calRecurrenceRule initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector interval (toNSArray monthDays) (toCalRecurrenceEnd end)

-- | @- initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> CULong -> CLong -> end -> IO RawId
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_end calRecurrenceRule interval weekDay monthWeek end =
  sendOwnedMessage calRecurrenceRule initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector interval weekDay monthWeek (toCalRecurrenceEnd end)

-- | @- initYearlyRecurrenceWithInterval:end:@
initYearlyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initYearlyRecurrenceWithInterval_end calRecurrenceRule interval end =
  sendOwnedMessage calRecurrenceRule initYearlyRecurrenceWithInterval_endSelector interval (toCalRecurrenceEnd end)

-- | @- initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray months, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> months -> end -> IO RawId
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_end calRecurrenceRule interval months end =
  sendOwnedMessage calRecurrenceRule initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector interval (toNSArray months) (toCalRecurrenceEnd end)

-- | @- initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray months, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> CULong -> CLong -> months -> end -> IO RawId
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_end calRecurrenceRule interval weekDay monthWeek months end =
  sendOwnedMessage calRecurrenceRule initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector interval weekDay monthWeek (toNSArray months) (toCalRecurrenceEnd end)

-- | @- recurrenceEnd@
recurrenceEnd :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id CalRecurrenceEnd)
recurrenceEnd calRecurrenceRule =
  sendMessage calRecurrenceRule recurrenceEndSelector

-- | @- recurrenceInterval@
recurrenceInterval :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO CULong
recurrenceInterval calRecurrenceRule =
  sendMessage calRecurrenceRule recurrenceIntervalSelector

-- | @- firstDayOfTheWeek@
firstDayOfTheWeek :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO CULong
firstDayOfTheWeek calRecurrenceRule =
  sendMessage calRecurrenceRule firstDayOfTheWeekSelector

-- | @- daysOfTheWeek@
daysOfTheWeek :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
daysOfTheWeek calRecurrenceRule =
  sendMessage calRecurrenceRule daysOfTheWeekSelector

-- | @- daysOfTheMonth@
daysOfTheMonth :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
daysOfTheMonth calRecurrenceRule =
  sendMessage calRecurrenceRule daysOfTheMonthSelector

-- | @- nthWeekDaysOfTheMonth@
nthWeekDaysOfTheMonth :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
nthWeekDaysOfTheMonth calRecurrenceRule =
  sendMessage calRecurrenceRule nthWeekDaysOfTheMonthSelector

-- | @- monthsOfTheYear@
monthsOfTheYear :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
monthsOfTheYear calRecurrenceRule =
  sendMessage calRecurrenceRule monthsOfTheYearSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initDailyRecurrenceWithInterval:end:@
initDailyRecurrenceWithInterval_endSelector :: Selector '[CULong, Id CalRecurrenceEnd] RawId
initDailyRecurrenceWithInterval_endSelector = mkSelector "initDailyRecurrenceWithInterval:end:"

-- | @Selector@ for @initWeeklyRecurrenceWithInterval:end:@
initWeeklyRecurrenceWithInterval_endSelector :: Selector '[CULong, Id CalRecurrenceEnd] RawId
initWeeklyRecurrenceWithInterval_endSelector = mkSelector "initWeeklyRecurrenceWithInterval:end:"

-- | @Selector@ for @initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:@
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector :: Selector '[CULong, Id NSArray, Id CalRecurrenceEnd] RawId
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector = mkSelector "initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:"

-- | @Selector@ for @initMonthlyRecurrenceWithInterval:end:@
initMonthlyRecurrenceWithInterval_endSelector :: Selector '[CULong, Id CalRecurrenceEnd] RawId
initMonthlyRecurrenceWithInterval_endSelector = mkSelector "initMonthlyRecurrenceWithInterval:end:"

-- | @Selector@ for @initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector :: Selector '[CULong, Id NSArray, Id CalRecurrenceEnd] RawId
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector = mkSelector "initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:"

-- | @Selector@ for @initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector :: Selector '[CULong, CULong, CLong, Id CalRecurrenceEnd] RawId
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector = mkSelector "initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:"

-- | @Selector@ for @initYearlyRecurrenceWithInterval:end:@
initYearlyRecurrenceWithInterval_endSelector :: Selector '[CULong, Id CalRecurrenceEnd] RawId
initYearlyRecurrenceWithInterval_endSelector = mkSelector "initYearlyRecurrenceWithInterval:end:"

-- | @Selector@ for @initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector :: Selector '[CULong, Id NSArray, Id CalRecurrenceEnd] RawId
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector = mkSelector "initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:"

-- | @Selector@ for @initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector :: Selector '[CULong, CULong, CLong, Id NSArray, Id CalRecurrenceEnd] RawId
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector = mkSelector "initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:"

-- | @Selector@ for @recurrenceEnd@
recurrenceEndSelector :: Selector '[] (Id CalRecurrenceEnd)
recurrenceEndSelector = mkSelector "recurrenceEnd"

-- | @Selector@ for @recurrenceInterval@
recurrenceIntervalSelector :: Selector '[] CULong
recurrenceIntervalSelector = mkSelector "recurrenceInterval"

-- | @Selector@ for @firstDayOfTheWeek@
firstDayOfTheWeekSelector :: Selector '[] CULong
firstDayOfTheWeekSelector = mkSelector "firstDayOfTheWeek"

-- | @Selector@ for @daysOfTheWeek@
daysOfTheWeekSelector :: Selector '[] (Id NSArray)
daysOfTheWeekSelector = mkSelector "daysOfTheWeek"

-- | @Selector@ for @daysOfTheMonth@
daysOfTheMonthSelector :: Selector '[] (Id NSArray)
daysOfTheMonthSelector = mkSelector "daysOfTheMonth"

-- | @Selector@ for @nthWeekDaysOfTheMonth@
nthWeekDaysOfTheMonthSelector :: Selector '[] (Id NSArray)
nthWeekDaysOfTheMonthSelector = mkSelector "nthWeekDaysOfTheMonth"

-- | @Selector@ for @monthsOfTheYear@
monthsOfTheYearSelector :: Selector '[] (Id NSArray)
monthsOfTheYearSelector = mkSelector "monthsOfTheYear"

