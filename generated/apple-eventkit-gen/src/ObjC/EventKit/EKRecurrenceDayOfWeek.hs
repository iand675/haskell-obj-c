{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKRecurrenceDayOfWeek
--
-- Class which represents a day of the week this recurrence will occur.
--
-- EKRecurrenceDayOfWeek specifies either a simple day of the week, or the nth instance                of a particular day of the week, such as the third Tuesday of every month. The week                number is only valid when used with monthly or yearly recurrences, since it would                be otherwise meaningless.
--
-- Valid values for dayOfTheWeek are integers 1-7, which correspond to days of the week                with Sunday = 1. Valid values for weekNumber portion are (+/-)1-53, where a negative                value indicates a value from the end of the range. For example, in a yearly event -1                means last week of the year. -1 in a Monthly recurrence indicates the last week of                the month.
--
-- The value 0 also indicates the weekNumber is irrelevant (every Sunday, etc.).
--
-- Day-of-week weekNumber values that are out of bounds for the recurrence type will                result in an exception when trying to initialize the recurrence. In particular,                weekNumber must be zero when passing EKRecurrenceDayOfWeek objects to initialize a weekly                 recurrence.
--
-- Generated bindings for @EKRecurrenceDayOfWeek@.
module ObjC.EventKit.EKRecurrenceDayOfWeek
  ( EKRecurrenceDayOfWeek
  , IsEKRecurrenceDayOfWeek(..)
  , dayOfWeek
  , dayOfWeek_weekNumber
  , initWithDayOfTheWeek_weekNumber
  , dayOfTheWeek
  , weekNumber
  , dayOfTheWeekSelector
  , dayOfWeekSelector
  , dayOfWeek_weekNumberSelector
  , initWithDayOfTheWeek_weekNumberSelector
  , weekNumberSelector

  -- * Enum types
  , EKWeekday(EKWeekday)
  , pattern EKWeekdaySunday
  , pattern EKWeekdayMonday
  , pattern EKWeekdayTuesday
  , pattern EKWeekdayWednesday
  , pattern EKWeekdayThursday
  , pattern EKWeekdayFriday
  , pattern EKWeekdaySaturday
  , pattern EKSunday
  , pattern EKMonday
  , pattern EKTuesday
  , pattern EKWednesday
  , pattern EKThursday
  , pattern EKFriday
  , pattern EKSaturday

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.EventKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | dayOfWeek:
--
-- Creates an autoreleased object with a day of the week and week number of zero.
--
-- ObjC selector: @+ dayOfWeek:@
dayOfWeek :: EKWeekday -> IO (Id EKRecurrenceDayOfWeek)
dayOfWeek dayOfTheWeek =
  do
    cls' <- getRequiredClass "EKRecurrenceDayOfWeek"
    sendClassMessage cls' dayOfWeekSelector dayOfTheWeek

-- | dayOfWeek:weekNumber:
--
-- Creates an autoreleased object with a specific day of week and week number.
--
-- ObjC selector: @+ dayOfWeek:weekNumber:@
dayOfWeek_weekNumber :: EKWeekday -> CLong -> IO (Id EKRecurrenceDayOfWeek)
dayOfWeek_weekNumber dayOfTheWeek weekNumber =
  do
    cls' <- getRequiredClass "EKRecurrenceDayOfWeek"
    sendClassMessage cls' dayOfWeek_weekNumberSelector dayOfTheWeek weekNumber

-- | initWithDayOfTheWeek:weekNumber:
--
-- Creates an day-of-week object with a specific day of week and week number.
--
-- ObjC selector: @- initWithDayOfTheWeek:weekNumber:@
initWithDayOfTheWeek_weekNumber :: IsEKRecurrenceDayOfWeek ekRecurrenceDayOfWeek => ekRecurrenceDayOfWeek -> EKWeekday -> CLong -> IO RawId
initWithDayOfTheWeek_weekNumber ekRecurrenceDayOfWeek dayOfTheWeek weekNumber =
  sendOwnedMessage ekRecurrenceDayOfWeek initWithDayOfTheWeek_weekNumberSelector dayOfTheWeek weekNumber

-- | dayOfTheWeek
--
-- The day of the week.
--
-- ObjC selector: @- dayOfTheWeek@
dayOfTheWeek :: IsEKRecurrenceDayOfWeek ekRecurrenceDayOfWeek => ekRecurrenceDayOfWeek -> IO EKWeekday
dayOfTheWeek ekRecurrenceDayOfWeek =
  sendMessage ekRecurrenceDayOfWeek dayOfTheWeekSelector

-- | weekNumber
--
-- The week number.
--
-- ObjC selector: @- weekNumber@
weekNumber :: IsEKRecurrenceDayOfWeek ekRecurrenceDayOfWeek => ekRecurrenceDayOfWeek -> IO CLong
weekNumber ekRecurrenceDayOfWeek =
  sendMessage ekRecurrenceDayOfWeek weekNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfWeek:@
dayOfWeekSelector :: Selector '[EKWeekday] (Id EKRecurrenceDayOfWeek)
dayOfWeekSelector = mkSelector "dayOfWeek:"

-- | @Selector@ for @dayOfWeek:weekNumber:@
dayOfWeek_weekNumberSelector :: Selector '[EKWeekday, CLong] (Id EKRecurrenceDayOfWeek)
dayOfWeek_weekNumberSelector = mkSelector "dayOfWeek:weekNumber:"

-- | @Selector@ for @initWithDayOfTheWeek:weekNumber:@
initWithDayOfTheWeek_weekNumberSelector :: Selector '[EKWeekday, CLong] RawId
initWithDayOfTheWeek_weekNumberSelector = mkSelector "initWithDayOfTheWeek:weekNumber:"

-- | @Selector@ for @dayOfTheWeek@
dayOfTheWeekSelector :: Selector '[] EKWeekday
dayOfTheWeekSelector = mkSelector "dayOfTheWeek"

-- | @Selector@ for @weekNumber@
weekNumberSelector :: Selector '[] CLong
weekNumberSelector = mkSelector "weekNumber"

