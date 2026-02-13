{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDateComponents@.
module ObjC.Foundation.NSDateComponents
  ( NSDateComponents
  , IsNSDateComponents(..)
  , week
  , setWeek
  , setValue_forComponent
  , valueForComponent
  , isValidDateInCalendar
  , calendar
  , setCalendar
  , timeZone
  , setTimeZone
  , era
  , setEra
  , year
  , setYear
  , month
  , setMonth
  , day
  , setDay
  , hour
  , setHour
  , minute
  , setMinute
  , second
  , setSecond
  , nanosecond
  , setNanosecond
  , weekday
  , setWeekday
  , weekdayOrdinal
  , setWeekdayOrdinal
  , quarter
  , setQuarter
  , weekOfMonth
  , setWeekOfMonth
  , weekOfYear
  , setWeekOfYear
  , yearForWeekOfYear
  , setYearForWeekOfYear
  , dayOfYear
  , setDayOfYear
  , leapMonth
  , setLeapMonth
  , repeatedDay
  , setRepeatedDay
  , date
  , validDate
  , calendarSelector
  , dateSelector
  , dayOfYearSelector
  , daySelector
  , eraSelector
  , hourSelector
  , isValidDateInCalendarSelector
  , leapMonthSelector
  , minuteSelector
  , monthSelector
  , nanosecondSelector
  , quarterSelector
  , repeatedDaySelector
  , secondSelector
  , setCalendarSelector
  , setDayOfYearSelector
  , setDaySelector
  , setEraSelector
  , setHourSelector
  , setLeapMonthSelector
  , setMinuteSelector
  , setMonthSelector
  , setNanosecondSelector
  , setQuarterSelector
  , setRepeatedDaySelector
  , setSecondSelector
  , setTimeZoneSelector
  , setValue_forComponentSelector
  , setWeekOfMonthSelector
  , setWeekOfYearSelector
  , setWeekSelector
  , setWeekdayOrdinalSelector
  , setWeekdaySelector
  , setYearForWeekOfYearSelector
  , setYearSelector
  , timeZoneSelector
  , validDateSelector
  , valueForComponentSelector
  , weekOfMonthSelector
  , weekOfYearSelector
  , weekSelector
  , weekdayOrdinalSelector
  , weekdaySelector
  , yearForWeekOfYearSelector
  , yearSelector

  -- * Enum types
  , NSCalendarUnit(NSCalendarUnit)
  , pattern NSCalendarUnitEra
  , pattern NSCalendarUnitYear
  , pattern NSCalendarUnitMonth
  , pattern NSCalendarUnitDay
  , pattern NSCalendarUnitHour
  , pattern NSCalendarUnitMinute
  , pattern NSCalendarUnitSecond
  , pattern NSCalendarUnitWeekday
  , pattern NSCalendarUnitWeekdayOrdinal
  , pattern NSCalendarUnitQuarter
  , pattern NSCalendarUnitWeekOfMonth
  , pattern NSCalendarUnitWeekOfYear
  , pattern NSCalendarUnitYearForWeekOfYear
  , pattern NSCalendarUnitNanosecond
  , pattern NSCalendarUnitDayOfYear
  , pattern NSCalendarUnitCalendar
  , pattern NSCalendarUnitTimeZone
  , pattern NSCalendarUnitIsLeapMonth
  , pattern NSCalendarUnitIsRepeatedDay
  , pattern NSEraCalendarUnit
  , pattern NSYearCalendarUnit
  , pattern NSMonthCalendarUnit
  , pattern NSDayCalendarUnit
  , pattern NSHourCalendarUnit
  , pattern NSMinuteCalendarUnit
  , pattern NSSecondCalendarUnit
  , pattern NSWeekCalendarUnit
  , pattern NSWeekdayCalendarUnit
  , pattern NSWeekdayOrdinalCalendarUnit
  , pattern NSQuarterCalendarUnit
  , pattern NSWeekOfMonthCalendarUnit
  , pattern NSWeekOfYearCalendarUnit
  , pattern NSYearForWeekOfYearCalendarUnit
  , pattern NSCalendarCalendarUnit
  , pattern NSTimeZoneCalendarUnit

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- week@
week :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
week nsDateComponents =
  sendMessage nsDateComponents weekSelector

-- | @- setWeek:@
setWeek :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeek nsDateComponents v =
  sendMessage nsDateComponents setWeekSelector v

-- | @- setValue:forComponent:@
setValue_forComponent :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> NSCalendarUnit -> IO ()
setValue_forComponent nsDateComponents value unit =
  sendMessage nsDateComponents setValue_forComponentSelector value unit

-- | @- valueForComponent:@
valueForComponent :: IsNSDateComponents nsDateComponents => nsDateComponents -> NSCalendarUnit -> IO CLong
valueForComponent nsDateComponents unit =
  sendMessage nsDateComponents valueForComponentSelector unit

-- | @- isValidDateInCalendar:@
isValidDateInCalendar :: (IsNSDateComponents nsDateComponents, IsNSCalendar calendar) => nsDateComponents -> calendar -> IO Bool
isValidDateInCalendar nsDateComponents calendar =
  sendMessage nsDateComponents isValidDateInCalendarSelector (toNSCalendar calendar)

-- | @- calendar@
calendar :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO (Id NSCalendar)
calendar nsDateComponents =
  sendMessage nsDateComponents calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSDateComponents nsDateComponents, IsNSCalendar value) => nsDateComponents -> value -> IO ()
setCalendar nsDateComponents value =
  sendMessage nsDateComponents setCalendarSelector (toNSCalendar value)

-- | @- timeZone@
timeZone :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO (Id NSTimeZone)
timeZone nsDateComponents =
  sendMessage nsDateComponents timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSDateComponents nsDateComponents, IsNSTimeZone value) => nsDateComponents -> value -> IO ()
setTimeZone nsDateComponents value =
  sendMessage nsDateComponents setTimeZoneSelector (toNSTimeZone value)

-- | @- era@
era :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
era nsDateComponents =
  sendMessage nsDateComponents eraSelector

-- | @- setEra:@
setEra :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setEra nsDateComponents value =
  sendMessage nsDateComponents setEraSelector value

-- | @- year@
year :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
year nsDateComponents =
  sendMessage nsDateComponents yearSelector

-- | @- setYear:@
setYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setYear nsDateComponents value =
  sendMessage nsDateComponents setYearSelector value

-- | @- month@
month :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
month nsDateComponents =
  sendMessage nsDateComponents monthSelector

-- | @- setMonth:@
setMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setMonth nsDateComponents value =
  sendMessage nsDateComponents setMonthSelector value

-- | @- day@
day :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
day nsDateComponents =
  sendMessage nsDateComponents daySelector

-- | @- setDay:@
setDay :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setDay nsDateComponents value =
  sendMessage nsDateComponents setDaySelector value

-- | @- hour@
hour :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
hour nsDateComponents =
  sendMessage nsDateComponents hourSelector

-- | @- setHour:@
setHour :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setHour nsDateComponents value =
  sendMessage nsDateComponents setHourSelector value

-- | @- minute@
minute :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
minute nsDateComponents =
  sendMessage nsDateComponents minuteSelector

-- | @- setMinute:@
setMinute :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setMinute nsDateComponents value =
  sendMessage nsDateComponents setMinuteSelector value

-- | @- second@
second :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
second nsDateComponents =
  sendMessage nsDateComponents secondSelector

-- | @- setSecond:@
setSecond :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setSecond nsDateComponents value =
  sendMessage nsDateComponents setSecondSelector value

-- | @- nanosecond@
nanosecond :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
nanosecond nsDateComponents =
  sendMessage nsDateComponents nanosecondSelector

-- | @- setNanosecond:@
setNanosecond :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setNanosecond nsDateComponents value =
  sendMessage nsDateComponents setNanosecondSelector value

-- | @- weekday@
weekday :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekday nsDateComponents =
  sendMessage nsDateComponents weekdaySelector

-- | @- setWeekday:@
setWeekday :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekday nsDateComponents value =
  sendMessage nsDateComponents setWeekdaySelector value

-- | @- weekdayOrdinal@
weekdayOrdinal :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekdayOrdinal nsDateComponents =
  sendMessage nsDateComponents weekdayOrdinalSelector

-- | @- setWeekdayOrdinal:@
setWeekdayOrdinal :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekdayOrdinal nsDateComponents value =
  sendMessage nsDateComponents setWeekdayOrdinalSelector value

-- | @- quarter@
quarter :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
quarter nsDateComponents =
  sendMessage nsDateComponents quarterSelector

-- | @- setQuarter:@
setQuarter :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setQuarter nsDateComponents value =
  sendMessage nsDateComponents setQuarterSelector value

-- | @- weekOfMonth@
weekOfMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekOfMonth nsDateComponents =
  sendMessage nsDateComponents weekOfMonthSelector

-- | @- setWeekOfMonth:@
setWeekOfMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekOfMonth nsDateComponents value =
  sendMessage nsDateComponents setWeekOfMonthSelector value

-- | @- weekOfYear@
weekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekOfYear nsDateComponents =
  sendMessage nsDateComponents weekOfYearSelector

-- | @- setWeekOfYear:@
setWeekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekOfYear nsDateComponents value =
  sendMessage nsDateComponents setWeekOfYearSelector value

-- | @- yearForWeekOfYear@
yearForWeekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
yearForWeekOfYear nsDateComponents =
  sendMessage nsDateComponents yearForWeekOfYearSelector

-- | @- setYearForWeekOfYear:@
setYearForWeekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setYearForWeekOfYear nsDateComponents value =
  sendMessage nsDateComponents setYearForWeekOfYearSelector value

-- | @- dayOfYear@
dayOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
dayOfYear nsDateComponents =
  sendMessage nsDateComponents dayOfYearSelector

-- | @- setDayOfYear:@
setDayOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setDayOfYear nsDateComponents value =
  sendMessage nsDateComponents setDayOfYearSelector value

-- | @- leapMonth@
leapMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO Bool
leapMonth nsDateComponents =
  sendMessage nsDateComponents leapMonthSelector

-- | @- setLeapMonth:@
setLeapMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> Bool -> IO ()
setLeapMonth nsDateComponents value =
  sendMessage nsDateComponents setLeapMonthSelector value

-- | @- repeatedDay@
repeatedDay :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO Bool
repeatedDay nsDateComponents =
  sendMessage nsDateComponents repeatedDaySelector

-- | @- setRepeatedDay:@
setRepeatedDay :: IsNSDateComponents nsDateComponents => nsDateComponents -> Bool -> IO ()
setRepeatedDay nsDateComponents value =
  sendMessage nsDateComponents setRepeatedDaySelector value

-- | @- date@
date :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO (Id NSDate)
date nsDateComponents =
  sendMessage nsDateComponents dateSelector

-- | @- validDate@
validDate :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO Bool
validDate nsDateComponents =
  sendMessage nsDateComponents validDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @week@
weekSelector :: Selector '[] CLong
weekSelector = mkSelector "week"

-- | @Selector@ for @setWeek:@
setWeekSelector :: Selector '[CLong] ()
setWeekSelector = mkSelector "setWeek:"

-- | @Selector@ for @setValue:forComponent:@
setValue_forComponentSelector :: Selector '[CLong, NSCalendarUnit] ()
setValue_forComponentSelector = mkSelector "setValue:forComponent:"

-- | @Selector@ for @valueForComponent:@
valueForComponentSelector :: Selector '[NSCalendarUnit] CLong
valueForComponentSelector = mkSelector "valueForComponent:"

-- | @Selector@ for @isValidDateInCalendar:@
isValidDateInCalendarSelector :: Selector '[Id NSCalendar] Bool
isValidDateInCalendarSelector = mkSelector "isValidDateInCalendar:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id NSCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id NSCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @era@
eraSelector :: Selector '[] CLong
eraSelector = mkSelector "era"

-- | @Selector@ for @setEra:@
setEraSelector :: Selector '[CLong] ()
setEraSelector = mkSelector "setEra:"

-- | @Selector@ for @year@
yearSelector :: Selector '[] CLong
yearSelector = mkSelector "year"

-- | @Selector@ for @setYear:@
setYearSelector :: Selector '[CLong] ()
setYearSelector = mkSelector "setYear:"

-- | @Selector@ for @month@
monthSelector :: Selector '[] CLong
monthSelector = mkSelector "month"

-- | @Selector@ for @setMonth:@
setMonthSelector :: Selector '[CLong] ()
setMonthSelector = mkSelector "setMonth:"

-- | @Selector@ for @day@
daySelector :: Selector '[] CLong
daySelector = mkSelector "day"

-- | @Selector@ for @setDay:@
setDaySelector :: Selector '[CLong] ()
setDaySelector = mkSelector "setDay:"

-- | @Selector@ for @hour@
hourSelector :: Selector '[] CLong
hourSelector = mkSelector "hour"

-- | @Selector@ for @setHour:@
setHourSelector :: Selector '[CLong] ()
setHourSelector = mkSelector "setHour:"

-- | @Selector@ for @minute@
minuteSelector :: Selector '[] CLong
minuteSelector = mkSelector "minute"

-- | @Selector@ for @setMinute:@
setMinuteSelector :: Selector '[CLong] ()
setMinuteSelector = mkSelector "setMinute:"

-- | @Selector@ for @second@
secondSelector :: Selector '[] CLong
secondSelector = mkSelector "second"

-- | @Selector@ for @setSecond:@
setSecondSelector :: Selector '[CLong] ()
setSecondSelector = mkSelector "setSecond:"

-- | @Selector@ for @nanosecond@
nanosecondSelector :: Selector '[] CLong
nanosecondSelector = mkSelector "nanosecond"

-- | @Selector@ for @setNanosecond:@
setNanosecondSelector :: Selector '[CLong] ()
setNanosecondSelector = mkSelector "setNanosecond:"

-- | @Selector@ for @weekday@
weekdaySelector :: Selector '[] CLong
weekdaySelector = mkSelector "weekday"

-- | @Selector@ for @setWeekday:@
setWeekdaySelector :: Selector '[CLong] ()
setWeekdaySelector = mkSelector "setWeekday:"

-- | @Selector@ for @weekdayOrdinal@
weekdayOrdinalSelector :: Selector '[] CLong
weekdayOrdinalSelector = mkSelector "weekdayOrdinal"

-- | @Selector@ for @setWeekdayOrdinal:@
setWeekdayOrdinalSelector :: Selector '[CLong] ()
setWeekdayOrdinalSelector = mkSelector "setWeekdayOrdinal:"

-- | @Selector@ for @quarter@
quarterSelector :: Selector '[] CLong
quarterSelector = mkSelector "quarter"

-- | @Selector@ for @setQuarter:@
setQuarterSelector :: Selector '[CLong] ()
setQuarterSelector = mkSelector "setQuarter:"

-- | @Selector@ for @weekOfMonth@
weekOfMonthSelector :: Selector '[] CLong
weekOfMonthSelector = mkSelector "weekOfMonth"

-- | @Selector@ for @setWeekOfMonth:@
setWeekOfMonthSelector :: Selector '[CLong] ()
setWeekOfMonthSelector = mkSelector "setWeekOfMonth:"

-- | @Selector@ for @weekOfYear@
weekOfYearSelector :: Selector '[] CLong
weekOfYearSelector = mkSelector "weekOfYear"

-- | @Selector@ for @setWeekOfYear:@
setWeekOfYearSelector :: Selector '[CLong] ()
setWeekOfYearSelector = mkSelector "setWeekOfYear:"

-- | @Selector@ for @yearForWeekOfYear@
yearForWeekOfYearSelector :: Selector '[] CLong
yearForWeekOfYearSelector = mkSelector "yearForWeekOfYear"

-- | @Selector@ for @setYearForWeekOfYear:@
setYearForWeekOfYearSelector :: Selector '[CLong] ()
setYearForWeekOfYearSelector = mkSelector "setYearForWeekOfYear:"

-- | @Selector@ for @dayOfYear@
dayOfYearSelector :: Selector '[] CLong
dayOfYearSelector = mkSelector "dayOfYear"

-- | @Selector@ for @setDayOfYear:@
setDayOfYearSelector :: Selector '[CLong] ()
setDayOfYearSelector = mkSelector "setDayOfYear:"

-- | @Selector@ for @leapMonth@
leapMonthSelector :: Selector '[] Bool
leapMonthSelector = mkSelector "leapMonth"

-- | @Selector@ for @setLeapMonth:@
setLeapMonthSelector :: Selector '[Bool] ()
setLeapMonthSelector = mkSelector "setLeapMonth:"

-- | @Selector@ for @repeatedDay@
repeatedDaySelector :: Selector '[] Bool
repeatedDaySelector = mkSelector "repeatedDay"

-- | @Selector@ for @setRepeatedDay:@
setRepeatedDaySelector :: Selector '[Bool] ()
setRepeatedDaySelector = mkSelector "setRepeatedDay:"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @validDate@
validDateSelector :: Selector '[] Bool
validDateSelector = mkSelector "validDate"

