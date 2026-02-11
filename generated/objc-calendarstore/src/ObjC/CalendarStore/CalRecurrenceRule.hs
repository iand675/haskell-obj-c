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
  , initDailyRecurrenceWithInterval_endSelector
  , initWeeklyRecurrenceWithInterval_endSelector
  , initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector
  , initMonthlyRecurrenceWithInterval_endSelector
  , initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector
  , initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector
  , initYearlyRecurrenceWithInterval_endSelector
  , initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector
  , initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector
  , recurrenceEndSelector
  , recurrenceIntervalSelector
  , firstDayOfTheWeekSelector
  , daysOfTheWeekSelector
  , daysOfTheMonthSelector
  , nthWeekDaysOfTheMonthSelector
  , monthsOfTheYearSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initDailyRecurrenceWithInterval:end:@
initDailyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initDailyRecurrenceWithInterval_end calRecurrenceRule  interval end =
withObjCPtr end $ \raw_end ->
    fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initDailyRecurrenceWithInterval:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initWeeklyRecurrenceWithInterval:end:@
initWeeklyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initWeeklyRecurrenceWithInterval_end calRecurrenceRule  interval end =
withObjCPtr end $ \raw_end ->
    fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initWeeklyRecurrenceWithInterval:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:@
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray days, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> days -> end -> IO RawId
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_end calRecurrenceRule  interval days end =
withObjCPtr days $ \raw_days ->
  withObjCPtr end $ \raw_end ->
      fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_days :: Ptr ()), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initMonthlyRecurrenceWithInterval:end:@
initMonthlyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initMonthlyRecurrenceWithInterval_end calRecurrenceRule  interval end =
withObjCPtr end $ \raw_end ->
    fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initMonthlyRecurrenceWithInterval:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray monthDays, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> monthDays -> end -> IO RawId
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_end calRecurrenceRule  interval monthDays end =
withObjCPtr monthDays $ \raw_monthDays ->
  withObjCPtr end $ \raw_end ->
      fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_monthDays :: Ptr ()), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> CULong -> CLong -> end -> IO RawId
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_end calRecurrenceRule  interval weekDay monthWeek end =
withObjCPtr end $ \raw_end ->
    fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argCULong (fromIntegral weekDay), argCLong (fromIntegral monthWeek), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initYearlyRecurrenceWithInterval:end:@
initYearlyRecurrenceWithInterval_end :: (IsCalRecurrenceRule calRecurrenceRule, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> end -> IO RawId
initYearlyRecurrenceWithInterval_end calRecurrenceRule  interval end =
withObjCPtr end $ \raw_end ->
    fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initYearlyRecurrenceWithInterval:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray months, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> months -> end -> IO RawId
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_end calRecurrenceRule  interval months end =
withObjCPtr months $ \raw_months ->
  withObjCPtr end $ \raw_end ->
      fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argPtr (castPtr raw_months :: Ptr ()), argPtr (castPtr raw_end :: Ptr ())]

-- | @- initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_end :: (IsCalRecurrenceRule calRecurrenceRule, IsNSArray months, IsCalRecurrenceEnd end) => calRecurrenceRule -> CULong -> CULong -> CLong -> months -> end -> IO RawId
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_end calRecurrenceRule  interval weekDay monthWeek months end =
withObjCPtr months $ \raw_months ->
  withObjCPtr end $ \raw_end ->
      fmap (RawId . castPtr) $ sendMsg calRecurrenceRule (mkSelector "initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:") (retPtr retVoid) [argCULong (fromIntegral interval), argCULong (fromIntegral weekDay), argCLong (fromIntegral monthWeek), argPtr (castPtr raw_months :: Ptr ()), argPtr (castPtr raw_end :: Ptr ())]

-- | @- recurrenceEnd@
recurrenceEnd :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id CalRecurrenceEnd)
recurrenceEnd calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "recurrenceEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recurrenceInterval@
recurrenceInterval :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO CULong
recurrenceInterval calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "recurrenceInterval") retCULong []

-- | @- firstDayOfTheWeek@
firstDayOfTheWeek :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO CULong
firstDayOfTheWeek calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "firstDayOfTheWeek") retCULong []

-- | @- daysOfTheWeek@
daysOfTheWeek :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
daysOfTheWeek calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "daysOfTheWeek") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- daysOfTheMonth@
daysOfTheMonth :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
daysOfTheMonth calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "daysOfTheMonth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nthWeekDaysOfTheMonth@
nthWeekDaysOfTheMonth :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
nthWeekDaysOfTheMonth calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "nthWeekDaysOfTheMonth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- monthsOfTheYear@
monthsOfTheYear :: IsCalRecurrenceRule calRecurrenceRule => calRecurrenceRule -> IO (Id NSArray)
monthsOfTheYear calRecurrenceRule  =
  sendMsg calRecurrenceRule (mkSelector "monthsOfTheYear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initDailyRecurrenceWithInterval:end:@
initDailyRecurrenceWithInterval_endSelector :: Selector
initDailyRecurrenceWithInterval_endSelector = mkSelector "initDailyRecurrenceWithInterval:end:"

-- | @Selector@ for @initWeeklyRecurrenceWithInterval:end:@
initWeeklyRecurrenceWithInterval_endSelector :: Selector
initWeeklyRecurrenceWithInterval_endSelector = mkSelector "initWeeklyRecurrenceWithInterval:end:"

-- | @Selector@ for @initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:@
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector :: Selector
initWeeklyRecurrenceWithInterval_forDaysOfTheWeek_endSelector = mkSelector "initWeeklyRecurrenceWithInterval:forDaysOfTheWeek:end:"

-- | @Selector@ for @initMonthlyRecurrenceWithInterval:end:@
initMonthlyRecurrenceWithInterval_endSelector :: Selector
initMonthlyRecurrenceWithInterval_endSelector = mkSelector "initMonthlyRecurrenceWithInterval:end:"

-- | @Selector@ for @initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector :: Selector
initMonthlyRecurrenceWithInterval_forDaysOfTheMonth_endSelector = mkSelector "initMonthlyRecurrenceWithInterval:forDaysOfTheMonth:end:"

-- | @Selector@ for @initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:@
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector :: Selector
initMonthlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_endSelector = mkSelector "initMonthlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:end:"

-- | @Selector@ for @initYearlyRecurrenceWithInterval:end:@
initYearlyRecurrenceWithInterval_endSelector :: Selector
initYearlyRecurrenceWithInterval_endSelector = mkSelector "initYearlyRecurrenceWithInterval:end:"

-- | @Selector@ for @initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector :: Selector
initYearlyRecurrenceWithInterval_forMonthsOfTheYear_endSelector = mkSelector "initYearlyRecurrenceWithInterval:forMonthsOfTheYear:end:"

-- | @Selector@ for @initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:@
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector :: Selector
initYearlyRecurrenceWithInterval_forDayOfTheWeek_forWeekOfTheMonth_forMonthsOfTheYear_endSelector = mkSelector "initYearlyRecurrenceWithInterval:forDayOfTheWeek:forWeekOfTheMonth:forMonthsOfTheYear:end:"

-- | @Selector@ for @recurrenceEnd@
recurrenceEndSelector :: Selector
recurrenceEndSelector = mkSelector "recurrenceEnd"

-- | @Selector@ for @recurrenceInterval@
recurrenceIntervalSelector :: Selector
recurrenceIntervalSelector = mkSelector "recurrenceInterval"

-- | @Selector@ for @firstDayOfTheWeek@
firstDayOfTheWeekSelector :: Selector
firstDayOfTheWeekSelector = mkSelector "firstDayOfTheWeek"

-- | @Selector@ for @daysOfTheWeek@
daysOfTheWeekSelector :: Selector
daysOfTheWeekSelector = mkSelector "daysOfTheWeek"

-- | @Selector@ for @daysOfTheMonth@
daysOfTheMonthSelector :: Selector
daysOfTheMonthSelector = mkSelector "daysOfTheMonth"

-- | @Selector@ for @nthWeekDaysOfTheMonth@
nthWeekDaysOfTheMonthSelector :: Selector
nthWeekDaysOfTheMonthSelector = mkSelector "nthWeekDaysOfTheMonth"

-- | @Selector@ for @monthsOfTheYear@
monthsOfTheYearSelector :: Selector
monthsOfTheYearSelector = mkSelector "monthsOfTheYear"

