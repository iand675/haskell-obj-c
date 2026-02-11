{-# LANGUAGE PatternSynonyms #-}
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
  , dayOfWeekSelector
  , dayOfWeek_weekNumberSelector
  , initWithDayOfTheWeek_weekNumberSelector
  , dayOfTheWeekSelector
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
    sendClassMsg cls' (mkSelector "dayOfWeek:") (retPtr retVoid) [argCLong (coerce dayOfTheWeek)] >>= retainedObject . castPtr

-- | dayOfWeek:weekNumber:
--
-- Creates an autoreleased object with a specific day of week and week number.
--
-- ObjC selector: @+ dayOfWeek:weekNumber:@
dayOfWeek_weekNumber :: EKWeekday -> CLong -> IO (Id EKRecurrenceDayOfWeek)
dayOfWeek_weekNumber dayOfTheWeek weekNumber =
  do
    cls' <- getRequiredClass "EKRecurrenceDayOfWeek"
    sendClassMsg cls' (mkSelector "dayOfWeek:weekNumber:") (retPtr retVoid) [argCLong (coerce dayOfTheWeek), argCLong (fromIntegral weekNumber)] >>= retainedObject . castPtr

-- | initWithDayOfTheWeek:weekNumber:
--
-- Creates an day-of-week object with a specific day of week and week number.
--
-- ObjC selector: @- initWithDayOfTheWeek:weekNumber:@
initWithDayOfTheWeek_weekNumber :: IsEKRecurrenceDayOfWeek ekRecurrenceDayOfWeek => ekRecurrenceDayOfWeek -> EKWeekday -> CLong -> IO RawId
initWithDayOfTheWeek_weekNumber ekRecurrenceDayOfWeek  dayOfTheWeek weekNumber =
  fmap (RawId . castPtr) $ sendMsg ekRecurrenceDayOfWeek (mkSelector "initWithDayOfTheWeek:weekNumber:") (retPtr retVoid) [argCLong (coerce dayOfTheWeek), argCLong (fromIntegral weekNumber)]

-- | dayOfTheWeek
--
-- The day of the week.
--
-- ObjC selector: @- dayOfTheWeek@
dayOfTheWeek :: IsEKRecurrenceDayOfWeek ekRecurrenceDayOfWeek => ekRecurrenceDayOfWeek -> IO EKWeekday
dayOfTheWeek ekRecurrenceDayOfWeek  =
  fmap (coerce :: CLong -> EKWeekday) $ sendMsg ekRecurrenceDayOfWeek (mkSelector "dayOfTheWeek") retCLong []

-- | weekNumber
--
-- The week number.
--
-- ObjC selector: @- weekNumber@
weekNumber :: IsEKRecurrenceDayOfWeek ekRecurrenceDayOfWeek => ekRecurrenceDayOfWeek -> IO CLong
weekNumber ekRecurrenceDayOfWeek  =
  sendMsg ekRecurrenceDayOfWeek (mkSelector "weekNumber") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfWeek:@
dayOfWeekSelector :: Selector
dayOfWeekSelector = mkSelector "dayOfWeek:"

-- | @Selector@ for @dayOfWeek:weekNumber:@
dayOfWeek_weekNumberSelector :: Selector
dayOfWeek_weekNumberSelector = mkSelector "dayOfWeek:weekNumber:"

-- | @Selector@ for @initWithDayOfTheWeek:weekNumber:@
initWithDayOfTheWeek_weekNumberSelector :: Selector
initWithDayOfTheWeek_weekNumberSelector = mkSelector "initWithDayOfTheWeek:weekNumber:"

-- | @Selector@ for @dayOfTheWeek@
dayOfTheWeekSelector :: Selector
dayOfTheWeekSelector = mkSelector "dayOfTheWeek"

-- | @Selector@ for @weekNumber@
weekNumberSelector :: Selector
weekNumberSelector = mkSelector "weekNumber"

