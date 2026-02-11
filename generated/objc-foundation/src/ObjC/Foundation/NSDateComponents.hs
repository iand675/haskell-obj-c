{-# LANGUAGE PatternSynonyms #-}
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
  , weekSelector
  , setWeekSelector
  , setValue_forComponentSelector
  , valueForComponentSelector
  , isValidDateInCalendarSelector
  , calendarSelector
  , setCalendarSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , eraSelector
  , setEraSelector
  , yearSelector
  , setYearSelector
  , monthSelector
  , setMonthSelector
  , daySelector
  , setDaySelector
  , hourSelector
  , setHourSelector
  , minuteSelector
  , setMinuteSelector
  , secondSelector
  , setSecondSelector
  , nanosecondSelector
  , setNanosecondSelector
  , weekdaySelector
  , setWeekdaySelector
  , weekdayOrdinalSelector
  , setWeekdayOrdinalSelector
  , quarterSelector
  , setQuarterSelector
  , weekOfMonthSelector
  , setWeekOfMonthSelector
  , weekOfYearSelector
  , setWeekOfYearSelector
  , yearForWeekOfYearSelector
  , setYearForWeekOfYearSelector
  , dayOfYearSelector
  , setDayOfYearSelector
  , leapMonthSelector
  , setLeapMonthSelector
  , repeatedDaySelector
  , setRepeatedDaySelector
  , dateSelector
  , validDateSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- week@
week :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
week nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "week") retCLong []

-- | @- setWeek:@
setWeek :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeek nsDateComponents  v =
  sendMsg nsDateComponents (mkSelector "setWeek:") retVoid [argCLong (fromIntegral v)]

-- | @- setValue:forComponent:@
setValue_forComponent :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> NSCalendarUnit -> IO ()
setValue_forComponent nsDateComponents  value unit =
  sendMsg nsDateComponents (mkSelector "setValue:forComponent:") retVoid [argCLong (fromIntegral value), argCULong (coerce unit)]

-- | @- valueForComponent:@
valueForComponent :: IsNSDateComponents nsDateComponents => nsDateComponents -> NSCalendarUnit -> IO CLong
valueForComponent nsDateComponents  unit =
  sendMsg nsDateComponents (mkSelector "valueForComponent:") retCLong [argCULong (coerce unit)]

-- | @- isValidDateInCalendar:@
isValidDateInCalendar :: (IsNSDateComponents nsDateComponents, IsNSCalendar calendar) => nsDateComponents -> calendar -> IO Bool
isValidDateInCalendar nsDateComponents  calendar =
withObjCPtr calendar $ \raw_calendar ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponents (mkSelector "isValidDateInCalendar:") retCULong [argPtr (castPtr raw_calendar :: Ptr ())]

-- | @- calendar@
calendar :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO (Id NSCalendar)
calendar nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSDateComponents nsDateComponents, IsNSCalendar value) => nsDateComponents -> value -> IO ()
setCalendar nsDateComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateComponents (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeZone@
timeZone :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO (Id NSTimeZone)
timeZone nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSDateComponents nsDateComponents, IsNSTimeZone value) => nsDateComponents -> value -> IO ()
setTimeZone nsDateComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateComponents (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- era@
era :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
era nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "era") retCLong []

-- | @- setEra:@
setEra :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setEra nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setEra:") retVoid [argCLong (fromIntegral value)]

-- | @- year@
year :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
year nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "year") retCLong []

-- | @- setYear:@
setYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setYear nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setYear:") retVoid [argCLong (fromIntegral value)]

-- | @- month@
month :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
month nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "month") retCLong []

-- | @- setMonth:@
setMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setMonth nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setMonth:") retVoid [argCLong (fromIntegral value)]

-- | @- day@
day :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
day nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "day") retCLong []

-- | @- setDay:@
setDay :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setDay nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setDay:") retVoid [argCLong (fromIntegral value)]

-- | @- hour@
hour :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
hour nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "hour") retCLong []

-- | @- setHour:@
setHour :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setHour nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setHour:") retVoid [argCLong (fromIntegral value)]

-- | @- minute@
minute :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
minute nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "minute") retCLong []

-- | @- setMinute:@
setMinute :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setMinute nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setMinute:") retVoid [argCLong (fromIntegral value)]

-- | @- second@
second :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
second nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "second") retCLong []

-- | @- setSecond:@
setSecond :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setSecond nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setSecond:") retVoid [argCLong (fromIntegral value)]

-- | @- nanosecond@
nanosecond :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
nanosecond nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "nanosecond") retCLong []

-- | @- setNanosecond:@
setNanosecond :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setNanosecond nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setNanosecond:") retVoid [argCLong (fromIntegral value)]

-- | @- weekday@
weekday :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekday nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "weekday") retCLong []

-- | @- setWeekday:@
setWeekday :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekday nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setWeekday:") retVoid [argCLong (fromIntegral value)]

-- | @- weekdayOrdinal@
weekdayOrdinal :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekdayOrdinal nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "weekdayOrdinal") retCLong []

-- | @- setWeekdayOrdinal:@
setWeekdayOrdinal :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekdayOrdinal nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setWeekdayOrdinal:") retVoid [argCLong (fromIntegral value)]

-- | @- quarter@
quarter :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
quarter nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "quarter") retCLong []

-- | @- setQuarter:@
setQuarter :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setQuarter nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setQuarter:") retVoid [argCLong (fromIntegral value)]

-- | @- weekOfMonth@
weekOfMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekOfMonth nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "weekOfMonth") retCLong []

-- | @- setWeekOfMonth:@
setWeekOfMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekOfMonth nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setWeekOfMonth:") retVoid [argCLong (fromIntegral value)]

-- | @- weekOfYear@
weekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
weekOfYear nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "weekOfYear") retCLong []

-- | @- setWeekOfYear:@
setWeekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setWeekOfYear nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setWeekOfYear:") retVoid [argCLong (fromIntegral value)]

-- | @- yearForWeekOfYear@
yearForWeekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
yearForWeekOfYear nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "yearForWeekOfYear") retCLong []

-- | @- setYearForWeekOfYear:@
setYearForWeekOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setYearForWeekOfYear nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setYearForWeekOfYear:") retVoid [argCLong (fromIntegral value)]

-- | @- dayOfYear@
dayOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO CLong
dayOfYear nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "dayOfYear") retCLong []

-- | @- setDayOfYear:@
setDayOfYear :: IsNSDateComponents nsDateComponents => nsDateComponents -> CLong -> IO ()
setDayOfYear nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setDayOfYear:") retVoid [argCLong (fromIntegral value)]

-- | @- leapMonth@
leapMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO Bool
leapMonth nsDateComponents  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponents (mkSelector "leapMonth") retCULong []

-- | @- setLeapMonth:@
setLeapMonth :: IsNSDateComponents nsDateComponents => nsDateComponents -> Bool -> IO ()
setLeapMonth nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setLeapMonth:") retVoid [argCULong (if value then 1 else 0)]

-- | @- repeatedDay@
repeatedDay :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO Bool
repeatedDay nsDateComponents  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponents (mkSelector "repeatedDay") retCULong []

-- | @- setRepeatedDay:@
setRepeatedDay :: IsNSDateComponents nsDateComponents => nsDateComponents -> Bool -> IO ()
setRepeatedDay nsDateComponents  value =
  sendMsg nsDateComponents (mkSelector "setRepeatedDay:") retVoid [argCULong (if value then 1 else 0)]

-- | @- date@
date :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO (Id NSDate)
date nsDateComponents  =
  sendMsg nsDateComponents (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- validDate@
validDate :: IsNSDateComponents nsDateComponents => nsDateComponents -> IO Bool
validDate nsDateComponents  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponents (mkSelector "validDate") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @week@
weekSelector :: Selector
weekSelector = mkSelector "week"

-- | @Selector@ for @setWeek:@
setWeekSelector :: Selector
setWeekSelector = mkSelector "setWeek:"

-- | @Selector@ for @setValue:forComponent:@
setValue_forComponentSelector :: Selector
setValue_forComponentSelector = mkSelector "setValue:forComponent:"

-- | @Selector@ for @valueForComponent:@
valueForComponentSelector :: Selector
valueForComponentSelector = mkSelector "valueForComponent:"

-- | @Selector@ for @isValidDateInCalendar:@
isValidDateInCalendarSelector :: Selector
isValidDateInCalendarSelector = mkSelector "isValidDateInCalendar:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @era@
eraSelector :: Selector
eraSelector = mkSelector "era"

-- | @Selector@ for @setEra:@
setEraSelector :: Selector
setEraSelector = mkSelector "setEra:"

-- | @Selector@ for @year@
yearSelector :: Selector
yearSelector = mkSelector "year"

-- | @Selector@ for @setYear:@
setYearSelector :: Selector
setYearSelector = mkSelector "setYear:"

-- | @Selector@ for @month@
monthSelector :: Selector
monthSelector = mkSelector "month"

-- | @Selector@ for @setMonth:@
setMonthSelector :: Selector
setMonthSelector = mkSelector "setMonth:"

-- | @Selector@ for @day@
daySelector :: Selector
daySelector = mkSelector "day"

-- | @Selector@ for @setDay:@
setDaySelector :: Selector
setDaySelector = mkSelector "setDay:"

-- | @Selector@ for @hour@
hourSelector :: Selector
hourSelector = mkSelector "hour"

-- | @Selector@ for @setHour:@
setHourSelector :: Selector
setHourSelector = mkSelector "setHour:"

-- | @Selector@ for @minute@
minuteSelector :: Selector
minuteSelector = mkSelector "minute"

-- | @Selector@ for @setMinute:@
setMinuteSelector :: Selector
setMinuteSelector = mkSelector "setMinute:"

-- | @Selector@ for @second@
secondSelector :: Selector
secondSelector = mkSelector "second"

-- | @Selector@ for @setSecond:@
setSecondSelector :: Selector
setSecondSelector = mkSelector "setSecond:"

-- | @Selector@ for @nanosecond@
nanosecondSelector :: Selector
nanosecondSelector = mkSelector "nanosecond"

-- | @Selector@ for @setNanosecond:@
setNanosecondSelector :: Selector
setNanosecondSelector = mkSelector "setNanosecond:"

-- | @Selector@ for @weekday@
weekdaySelector :: Selector
weekdaySelector = mkSelector "weekday"

-- | @Selector@ for @setWeekday:@
setWeekdaySelector :: Selector
setWeekdaySelector = mkSelector "setWeekday:"

-- | @Selector@ for @weekdayOrdinal@
weekdayOrdinalSelector :: Selector
weekdayOrdinalSelector = mkSelector "weekdayOrdinal"

-- | @Selector@ for @setWeekdayOrdinal:@
setWeekdayOrdinalSelector :: Selector
setWeekdayOrdinalSelector = mkSelector "setWeekdayOrdinal:"

-- | @Selector@ for @quarter@
quarterSelector :: Selector
quarterSelector = mkSelector "quarter"

-- | @Selector@ for @setQuarter:@
setQuarterSelector :: Selector
setQuarterSelector = mkSelector "setQuarter:"

-- | @Selector@ for @weekOfMonth@
weekOfMonthSelector :: Selector
weekOfMonthSelector = mkSelector "weekOfMonth"

-- | @Selector@ for @setWeekOfMonth:@
setWeekOfMonthSelector :: Selector
setWeekOfMonthSelector = mkSelector "setWeekOfMonth:"

-- | @Selector@ for @weekOfYear@
weekOfYearSelector :: Selector
weekOfYearSelector = mkSelector "weekOfYear"

-- | @Selector@ for @setWeekOfYear:@
setWeekOfYearSelector :: Selector
setWeekOfYearSelector = mkSelector "setWeekOfYear:"

-- | @Selector@ for @yearForWeekOfYear@
yearForWeekOfYearSelector :: Selector
yearForWeekOfYearSelector = mkSelector "yearForWeekOfYear"

-- | @Selector@ for @setYearForWeekOfYear:@
setYearForWeekOfYearSelector :: Selector
setYearForWeekOfYearSelector = mkSelector "setYearForWeekOfYear:"

-- | @Selector@ for @dayOfYear@
dayOfYearSelector :: Selector
dayOfYearSelector = mkSelector "dayOfYear"

-- | @Selector@ for @setDayOfYear:@
setDayOfYearSelector :: Selector
setDayOfYearSelector = mkSelector "setDayOfYear:"

-- | @Selector@ for @leapMonth@
leapMonthSelector :: Selector
leapMonthSelector = mkSelector "leapMonth"

-- | @Selector@ for @setLeapMonth:@
setLeapMonthSelector :: Selector
setLeapMonthSelector = mkSelector "setLeapMonth:"

-- | @Selector@ for @repeatedDay@
repeatedDaySelector :: Selector
repeatedDaySelector = mkSelector "repeatedDay"

-- | @Selector@ for @setRepeatedDay:@
setRepeatedDaySelector :: Selector
setRepeatedDaySelector = mkSelector "setRepeatedDay:"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @validDate@
validDateSelector :: Selector
validDateSelector = mkSelector "validDate"

