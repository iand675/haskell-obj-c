{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKRecurrenceRule
--
-- Represents how an event repeats.
--
-- This class describes the recurrence pattern for a repeating event. The recurrence rules that                 can be expressed are not restricted to the recurrence patterns that can be set in Calendar's UI.
--
-- It is currently not possible to directly modify a EKRecurrenceRule or any of its properties.                 This functionality is achieved by creating a new EKRecurrenceRule, and setting an event to use the new rule.                 When a new recurrence rule is set on an EKEvent, that change is not saved until the client                 has passed the modified event to EKEventStore's saveEvent: method.
--
-- Generated bindings for @EKRecurrenceRule@.
module ObjC.EventKit.EKRecurrenceRule
  ( EKRecurrenceRule
  , IsEKRecurrenceRule(..)
  , initRecurrenceWithFrequency_interval_end
  , initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_end
  , calendarIdentifier
  , recurrenceEnd
  , setRecurrenceEnd
  , frequency
  , interval
  , firstDayOfTheWeek
  , daysOfTheWeek
  , daysOfTheMonth
  , daysOfTheYear
  , weeksOfTheYear
  , monthsOfTheYear
  , setPositions
  , initRecurrenceWithFrequency_interval_endSelector
  , initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_endSelector
  , calendarIdentifierSelector
  , recurrenceEndSelector
  , setRecurrenceEndSelector
  , frequencySelector
  , intervalSelector
  , firstDayOfTheWeekSelector
  , daysOfTheWeekSelector
  , daysOfTheMonthSelector
  , daysOfTheYearSelector
  , weeksOfTheYearSelector
  , monthsOfTheYearSelector
  , setPositionsSelector

  -- * Enum types
  , EKRecurrenceFrequency(EKRecurrenceFrequency)
  , pattern EKRecurrenceFrequencyDaily
  , pattern EKRecurrenceFrequencyWeekly
  , pattern EKRecurrenceFrequencyMonthly
  , pattern EKRecurrenceFrequencyYearly

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

-- | initRecurrenceWithFrequency:interval:end:
--
-- Simple initializer to create a recurrence.
--
-- This is used to create a simple recurrence with a specific type, interval and end. If interval is                0, an exception is raised. The end parameter can be nil.
--
-- ObjC selector: @- initRecurrenceWithFrequency:interval:end:@
initRecurrenceWithFrequency_interval_end :: (IsEKRecurrenceRule ekRecurrenceRule, IsEKRecurrenceEnd end) => ekRecurrenceRule -> EKRecurrenceFrequency -> CLong -> end -> IO (Id EKRecurrenceRule)
initRecurrenceWithFrequency_interval_end ekRecurrenceRule  type_ interval end =
withObjCPtr end $ \raw_end ->
    sendMsg ekRecurrenceRule (mkSelector "initRecurrenceWithFrequency:interval:end:") (retPtr retVoid) [argCLong (coerce type_), argCLong (fromIntegral interval), argPtr (castPtr raw_end :: Ptr ())] >>= ownedObject . castPtr

-- | initRecurrenceWithFrequency:interval:daysOfTheWeek:daysOfTheMonth:monthsOfTheYear:weeksOfTheYear:daysOfTheYear:setPositions:end:
--
-- The designated initializer.
--
-- This can be used to build any kind of recurrence rule. But be aware that certain combinations make                no sense and will be ignored. For example, if you pass daysOfTheWeek for a daily recurrence, they                will be ignored.
--
-- @type@ — The type of recurrence
--
-- @interval@ — The interval. Passing zero will raise an exception.
--
-- @daysOfTheWeek@ — An array of EKRecurrenceDayOfWeek objects. Valid for all recurrence types except daily. Ignored otherwise.                                Corresponds to the BYDAY value in the iCalendar specification.
--
-- @daysOfTheMonth@ — An array of NSNumbers ([+/-] 1 to 31). Negative numbers infer counting from the end of the month.                                For example, -1 means the last day of the month. Valid only for monthly recurrences. Ignored otherwise.                                Corresponds to the BYMONTHDAY value in the iCalendar specification.
--
-- @monthsOfTheYear@ — An array of NSNumbers (1 to 12). Valid only for yearly recurrences. Ignored otherwise. Corresponds to                                the BYMONTH value in the iCalendar specification.
--
-- @weeksOfTheYear@ — An array of NSNumbers ([+/1] 1 to 53). Negative numbers infer counting from the end of the year.                                For example, -1 means the last week of the year. Valid only for yearly recurrences. Ignored otherwise.                                Corresponds to the BYWEEKNO value in the iCalendar specification.
--
-- @daysOfTheYear@ — An array of NSNumbers ([+/1] 1 to 366). Negative numbers infer counting from the end of the year.                                For example, -1 means the last day of the year. Valid only for yearly recurrences. Ignored otherwise.                                Corresponds to the BYYEARDAY value in the iCalendar specification.
--
-- @setPositions@ — An array of NSNumbers ([+/1] 1 to 366). Used at the end of recurrence computation to filter the list                                to the positions specified. Negative numbers indicate starting at the end, i.e. -1 indicates taking the                                last result of the set. Valid when daysOfTheWeek, daysOfTheMonth, monthsOfTheYear, weeksOfTheYear, or                                 daysOfTheYear is passed. Ignored otherwise. Corresponds to the BYSETPOS value in the iCalendar specification.
--
-- @end@ — The recurrence end, or nil.
--
-- ObjC selector: @- initRecurrenceWithFrequency:interval:daysOfTheWeek:daysOfTheMonth:monthsOfTheYear:weeksOfTheYear:daysOfTheYear:setPositions:end:@
initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_end :: (IsEKRecurrenceRule ekRecurrenceRule, IsNSArray days, IsNSArray monthDays, IsNSArray months, IsNSArray weeksOfTheYear, IsNSArray daysOfTheYear, IsNSArray setPositions, IsEKRecurrenceEnd end) => ekRecurrenceRule -> EKRecurrenceFrequency -> CLong -> days -> monthDays -> months -> weeksOfTheYear -> daysOfTheYear -> setPositions -> end -> IO (Id EKRecurrenceRule)
initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_end ekRecurrenceRule  type_ interval days monthDays months weeksOfTheYear daysOfTheYear setPositions end =
withObjCPtr days $ \raw_days ->
  withObjCPtr monthDays $ \raw_monthDays ->
    withObjCPtr months $ \raw_months ->
      withObjCPtr weeksOfTheYear $ \raw_weeksOfTheYear ->
        withObjCPtr daysOfTheYear $ \raw_daysOfTheYear ->
          withObjCPtr setPositions $ \raw_setPositions ->
            withObjCPtr end $ \raw_end ->
                sendMsg ekRecurrenceRule (mkSelector "initRecurrenceWithFrequency:interval:daysOfTheWeek:daysOfTheMonth:monthsOfTheYear:weeksOfTheYear:daysOfTheYear:setPositions:end:") (retPtr retVoid) [argCLong (coerce type_), argCLong (fromIntegral interval), argPtr (castPtr raw_days :: Ptr ()), argPtr (castPtr raw_monthDays :: Ptr ()), argPtr (castPtr raw_months :: Ptr ()), argPtr (castPtr raw_weeksOfTheYear :: Ptr ()), argPtr (castPtr raw_daysOfTheYear :: Ptr ()), argPtr (castPtr raw_setPositions :: Ptr ()), argPtr (castPtr raw_end :: Ptr ())] >>= ownedObject . castPtr

-- | calendarIdentifier;
--
-- Calendar used by this recurrence rule.
--
-- ObjC selector: @- calendarIdentifier@
calendarIdentifier :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSString)
calendarIdentifier ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "calendarIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | recurrenceEnd
--
-- This property defines when the the repeating event is scheduled to end. The end date can be specified by a number of                    occurrences, or with an end date.
--
-- ObjC selector: @- recurrenceEnd@
recurrenceEnd :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id EKRecurrenceEnd)
recurrenceEnd ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "recurrenceEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | recurrenceEnd
--
-- This property defines when the the repeating event is scheduled to end. The end date can be specified by a number of                    occurrences, or with an end date.
--
-- ObjC selector: @- setRecurrenceEnd:@
setRecurrenceEnd :: (IsEKRecurrenceRule ekRecurrenceRule, IsEKRecurrenceEnd value) => ekRecurrenceRule -> value -> IO ()
setRecurrenceEnd ekRecurrenceRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekRecurrenceRule (mkSelector "setRecurrenceEnd:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | frequency
--
-- This property designates the unit of time used to describe the recurrence pattern.
--
-- ObjC selector: @- frequency@
frequency :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO EKRecurrenceFrequency
frequency ekRecurrenceRule  =
  fmap (coerce :: CLong -> EKRecurrenceFrequency) $ sendMsg ekRecurrenceRule (mkSelector "frequency") retCLong []

-- | interval
--
-- The interval of a EKRecurrenceRule is an integer value which specifies how often the recurrence rule repeats                    over the unit of time described by the EKRecurrenceFrequency. For example, if the EKRecurrenceFrequency is                    EKRecurrenceWeekly, then an interval of 1 means the pattern is repeated every week. A value of 2                    indicates it is repeated every other week, 3 means every third week, and so on. The value must be a                    positive integer; 0 is not a valid value, and nil will be returned if the client attempts to initialize a                    rule with a negative or zero interval.
--
-- ObjC selector: @- interval@
interval :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO CLong
interval ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "interval") retCLong []

-- | firstDayOfTheWeek
--
-- Recurrence patterns can specify which day of the week should be treated as the first day. Possible values for this                    property are integers 0 and 1-7, which correspond to days of the week with Sunday = 1. Zero indicates that the                     property is not set for this recurrence. The first day of the week only affects the way the recurrence is expanded                    for weekly recurrence patterns with an interval greater than 1. For those types of recurrence patterns, the                     Calendar framework will set firstDayOfTheWeek to be 2 (Monday). In all other cases, this property will be set                     to zero. The iCalendar spec stipulates that the default value is Monday if this property is not set.
--
-- ObjC selector: @- firstDayOfTheWeek@
firstDayOfTheWeek :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO CLong
firstDayOfTheWeek ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "firstDayOfTheWeek") retCLong []

-- | daysOfTheWeek
--
-- This property is valid for rules whose EKRecurrenceFrequency is EKRecurrenceFrequencyWeekly, EKRecurrenceFrequencyMonthly, or                     EKRecurrenceFrequencyYearly. This property can be accessed as an array containing one or more EKRecurrenceDayOfWeek objects                    corresponding to the days of the week the event recurs. For all other EKRecurrenceRules, this property is nil.                    This property corresponds to BYDAY in the iCalendar specification.
--
-- ObjC selector: @- daysOfTheWeek@
daysOfTheWeek :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSArray)
daysOfTheWeek ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "daysOfTheWeek") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | daysOfTheMonth
--
-- This property is valid for rules whose EKRecurrenceFrequency is EKRecurrenceFrequencyMonthly, and that were initialized                     with one or more specific days of the month (not with a day of the week and week of the month). This property can be                    accessed as an array containing one or more NSNumbers corresponding to the days of the month the event recurs.                    For all other EKRecurrenceRules, this property is nil. This property corresponds to BYMONTHDAY in the iCalendar                     specification.
--
-- ObjC selector: @- daysOfTheMonth@
daysOfTheMonth :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSArray)
daysOfTheMonth ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "daysOfTheMonth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | daysOfTheYear
--
-- This property is valid for rules whose EKRecurrenceFrequency is EKRecurrenceFrequencyYearly. This property can be accessed                     as an array containing one or more NSNumbers corresponding to the days of the year the event recurs. For all other                     EKRecurrenceRules, this property is nil. This property corresponds to BYYEARDAY in the iCalendar specification. It should                    contain values between 1 to 366 or -366 to -1.
--
-- ObjC selector: @- daysOfTheYear@
daysOfTheYear :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSArray)
daysOfTheYear ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "daysOfTheYear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weeksOfTheYear
--
-- This property is valid for rules whose EKRecurrenceFrequency is EKRecurrenceFrequencyYearly. This property can be accessed                     as an array containing one or more NSNumbers corresponding to the weeks of the year the event recurs. For all other                     EKRecurrenceRules, this property is nil. This property corresponds to BYWEEK in the iCalendar specification. It should                    contain integers from 1 to 53 or -1 to -53.
--
-- ObjC selector: @- weeksOfTheYear@
weeksOfTheYear :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSArray)
weeksOfTheYear ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "weeksOfTheYear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | monthsOfTheYear
--
-- This property is valid for rules whose EKRecurrenceFrequency is EKRecurrenceFrequencyYearly. This property can be accessed                     as an array containing one or more NSNumbers corresponding to the months of the year the event recurs. For all other                     EKRecurrenceRules, this property is nil. This property corresponds to BYMONTH in the iCalendar specification.
--
-- ObjC selector: @- monthsOfTheYear@
monthsOfTheYear :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSArray)
monthsOfTheYear ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "monthsOfTheYear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setPositions
--
-- This property is valid for rules which have a valid daysOfTheWeek, daysOfTheMonth, weeksOfTheYear, or monthsOfTheYear property.                     It allows you to specify a set of ordinal numbers to help choose which objects out of the set of selected events should be                    included. For example, setting the daysOfTheWeek to Monday-Friday and including a value of -1 in the array would indicate                    the last weekday in the recurrence range (month, year, etc). This value corresponds to the iCalendar BYSETPOS property.
--
-- ObjC selector: @- setPositions@
setPositions :: IsEKRecurrenceRule ekRecurrenceRule => ekRecurrenceRule -> IO (Id NSArray)
setPositions ekRecurrenceRule  =
  sendMsg ekRecurrenceRule (mkSelector "setPositions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initRecurrenceWithFrequency:interval:end:@
initRecurrenceWithFrequency_interval_endSelector :: Selector
initRecurrenceWithFrequency_interval_endSelector = mkSelector "initRecurrenceWithFrequency:interval:end:"

-- | @Selector@ for @initRecurrenceWithFrequency:interval:daysOfTheWeek:daysOfTheMonth:monthsOfTheYear:weeksOfTheYear:daysOfTheYear:setPositions:end:@
initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_endSelector :: Selector
initRecurrenceWithFrequency_interval_daysOfTheWeek_daysOfTheMonth_monthsOfTheYear_weeksOfTheYear_daysOfTheYear_setPositions_endSelector = mkSelector "initRecurrenceWithFrequency:interval:daysOfTheWeek:daysOfTheMonth:monthsOfTheYear:weeksOfTheYear:daysOfTheYear:setPositions:end:"

-- | @Selector@ for @calendarIdentifier@
calendarIdentifierSelector :: Selector
calendarIdentifierSelector = mkSelector "calendarIdentifier"

-- | @Selector@ for @recurrenceEnd@
recurrenceEndSelector :: Selector
recurrenceEndSelector = mkSelector "recurrenceEnd"

-- | @Selector@ for @setRecurrenceEnd:@
setRecurrenceEndSelector :: Selector
setRecurrenceEndSelector = mkSelector "setRecurrenceEnd:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @interval@
intervalSelector :: Selector
intervalSelector = mkSelector "interval"

-- | @Selector@ for @firstDayOfTheWeek@
firstDayOfTheWeekSelector :: Selector
firstDayOfTheWeekSelector = mkSelector "firstDayOfTheWeek"

-- | @Selector@ for @daysOfTheWeek@
daysOfTheWeekSelector :: Selector
daysOfTheWeekSelector = mkSelector "daysOfTheWeek"

-- | @Selector@ for @daysOfTheMonth@
daysOfTheMonthSelector :: Selector
daysOfTheMonthSelector = mkSelector "daysOfTheMonth"

-- | @Selector@ for @daysOfTheYear@
daysOfTheYearSelector :: Selector
daysOfTheYearSelector = mkSelector "daysOfTheYear"

-- | @Selector@ for @weeksOfTheYear@
weeksOfTheYearSelector :: Selector
weeksOfTheYearSelector = mkSelector "weeksOfTheYear"

-- | @Selector@ for @monthsOfTheYear@
monthsOfTheYearSelector :: Selector
monthsOfTheYearSelector = mkSelector "monthsOfTheYear"

-- | @Selector@ for @setPositions@
setPositionsSelector :: Selector
setPositionsSelector = mkSelector "setPositions"

