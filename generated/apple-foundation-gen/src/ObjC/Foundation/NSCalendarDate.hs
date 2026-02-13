{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCalendarDate@.
module ObjC.Foundation.NSCalendarDate
  ( NSCalendarDate
  , IsNSCalendarDate(..)
  , calendarDate
  , dateWithString_calendarFormat_locale
  , dateWithString_calendarFormat
  , dateWithYear_month_day_hour_minute_second_timeZone
  , dateByAddingYears_months_days_hours_minutes_seconds
  , dayOfCommonEra
  , dayOfMonth
  , dayOfWeek
  , dayOfYear
  , hourOfDay
  , minuteOfHour
  , monthOfYear
  , secondOfMinute
  , yearOfCommonEra
  , calendarFormat
  , descriptionWithCalendarFormat_locale
  , descriptionWithCalendarFormat
  , descriptionWithLocale
  , timeZone
  , initWithString_calendarFormat_locale
  , initWithString_calendarFormat
  , initWithString
  , initWithYear_month_day_hour_minute_second_timeZone
  , setCalendarFormat
  , setTimeZone
  , years_months_days_hours_minutes_seconds_sinceDate
  , distantFuture
  , distantPast
  , calendarDateSelector
  , calendarFormatSelector
  , dateByAddingYears_months_days_hours_minutes_secondsSelector
  , dateWithString_calendarFormatSelector
  , dateWithString_calendarFormat_localeSelector
  , dateWithYear_month_day_hour_minute_second_timeZoneSelector
  , dayOfCommonEraSelector
  , dayOfMonthSelector
  , dayOfWeekSelector
  , dayOfYearSelector
  , descriptionWithCalendarFormatSelector
  , descriptionWithCalendarFormat_localeSelector
  , descriptionWithLocaleSelector
  , distantFutureSelector
  , distantPastSelector
  , hourOfDaySelector
  , initWithStringSelector
  , initWithString_calendarFormatSelector
  , initWithString_calendarFormat_localeSelector
  , initWithYear_month_day_hour_minute_second_timeZoneSelector
  , minuteOfHourSelector
  , monthOfYearSelector
  , secondOfMinuteSelector
  , setCalendarFormatSelector
  , setTimeZoneSelector
  , timeZoneSelector
  , yearOfCommonEraSelector
  , years_months_days_hours_minutes_seconds_sinceDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ calendarDate@
calendarDate :: IO RawId
calendarDate  =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMessage cls' calendarDateSelector

-- | @+ dateWithString:calendarFormat:locale:@
dateWithString_calendarFormat_locale :: (IsNSString description, IsNSString format) => description -> format -> RawId -> IO RawId
dateWithString_calendarFormat_locale description format locale =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMessage cls' dateWithString_calendarFormat_localeSelector (toNSString description) (toNSString format) locale

-- | @+ dateWithString:calendarFormat:@
dateWithString_calendarFormat :: (IsNSString description, IsNSString format) => description -> format -> IO RawId
dateWithString_calendarFormat description format =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMessage cls' dateWithString_calendarFormatSelector (toNSString description) (toNSString format)

-- | @+ dateWithYear:month:day:hour:minute:second:timeZone:@
dateWithYear_month_day_hour_minute_second_timeZone :: IsNSTimeZone aTimeZone => CLong -> CULong -> CULong -> CULong -> CULong -> CULong -> aTimeZone -> IO RawId
dateWithYear_month_day_hour_minute_second_timeZone year month day hour minute second aTimeZone =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMessage cls' dateWithYear_month_day_hour_minute_second_timeZoneSelector year month day hour minute second (toNSTimeZone aTimeZone)

-- | @- dateByAddingYears:months:days:hours:minutes:seconds:@
dateByAddingYears_months_days_hours_minutes_seconds :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id NSCalendarDate)
dateByAddingYears_months_days_hours_minutes_seconds nsCalendarDate year month day hour minute second =
  sendMessage nsCalendarDate dateByAddingYears_months_days_hours_minutes_secondsSelector year month day hour minute second

-- | @- dayOfCommonEra@
dayOfCommonEra :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfCommonEra nsCalendarDate =
  sendMessage nsCalendarDate dayOfCommonEraSelector

-- | @- dayOfMonth@
dayOfMonth :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfMonth nsCalendarDate =
  sendMessage nsCalendarDate dayOfMonthSelector

-- | @- dayOfWeek@
dayOfWeek :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfWeek nsCalendarDate =
  sendMessage nsCalendarDate dayOfWeekSelector

-- | @- dayOfYear@
dayOfYear :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfYear nsCalendarDate =
  sendMessage nsCalendarDate dayOfYearSelector

-- | @- hourOfDay@
hourOfDay :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
hourOfDay nsCalendarDate =
  sendMessage nsCalendarDate hourOfDaySelector

-- | @- minuteOfHour@
minuteOfHour :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
minuteOfHour nsCalendarDate =
  sendMessage nsCalendarDate minuteOfHourSelector

-- | @- monthOfYear@
monthOfYear :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
monthOfYear nsCalendarDate =
  sendMessage nsCalendarDate monthOfYearSelector

-- | @- secondOfMinute@
secondOfMinute :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
secondOfMinute nsCalendarDate =
  sendMessage nsCalendarDate secondOfMinuteSelector

-- | @- yearOfCommonEra@
yearOfCommonEra :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
yearOfCommonEra nsCalendarDate =
  sendMessage nsCalendarDate yearOfCommonEraSelector

-- | @- calendarFormat@
calendarFormat :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO (Id NSString)
calendarFormat nsCalendarDate =
  sendMessage nsCalendarDate calendarFormatSelector

-- | @- descriptionWithCalendarFormat:locale:@
descriptionWithCalendarFormat_locale :: (IsNSCalendarDate nsCalendarDate, IsNSString format) => nsCalendarDate -> format -> RawId -> IO (Id NSString)
descriptionWithCalendarFormat_locale nsCalendarDate format locale =
  sendMessage nsCalendarDate descriptionWithCalendarFormat_localeSelector (toNSString format) locale

-- | @- descriptionWithCalendarFormat:@
descriptionWithCalendarFormat :: (IsNSCalendarDate nsCalendarDate, IsNSString format) => nsCalendarDate -> format -> IO (Id NSString)
descriptionWithCalendarFormat nsCalendarDate format =
  sendMessage nsCalendarDate descriptionWithCalendarFormatSelector (toNSString format)

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> RawId -> IO (Id NSString)
descriptionWithLocale nsCalendarDate locale =
  sendMessage nsCalendarDate descriptionWithLocaleSelector locale

-- | @- timeZone@
timeZone :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO (Id NSTimeZone)
timeZone nsCalendarDate =
  sendMessage nsCalendarDate timeZoneSelector

-- | @- initWithString:calendarFormat:locale:@
initWithString_calendarFormat_locale :: (IsNSCalendarDate nsCalendarDate, IsNSString description, IsNSString format) => nsCalendarDate -> description -> format -> RawId -> IO RawId
initWithString_calendarFormat_locale nsCalendarDate description format locale =
  sendOwnedMessage nsCalendarDate initWithString_calendarFormat_localeSelector (toNSString description) (toNSString format) locale

-- | @- initWithString:calendarFormat:@
initWithString_calendarFormat :: (IsNSCalendarDate nsCalendarDate, IsNSString description, IsNSString format) => nsCalendarDate -> description -> format -> IO RawId
initWithString_calendarFormat nsCalendarDate description format =
  sendOwnedMessage nsCalendarDate initWithString_calendarFormatSelector (toNSString description) (toNSString format)

-- | @- initWithString:@
initWithString :: (IsNSCalendarDate nsCalendarDate, IsNSString description) => nsCalendarDate -> description -> IO RawId
initWithString nsCalendarDate description =
  sendOwnedMessage nsCalendarDate initWithStringSelector (toNSString description)

-- | @- initWithYear:month:day:hour:minute:second:timeZone:@
initWithYear_month_day_hour_minute_second_timeZone :: (IsNSCalendarDate nsCalendarDate, IsNSTimeZone aTimeZone) => nsCalendarDate -> CLong -> CULong -> CULong -> CULong -> CULong -> CULong -> aTimeZone -> IO RawId
initWithYear_month_day_hour_minute_second_timeZone nsCalendarDate year month day hour minute second aTimeZone =
  sendOwnedMessage nsCalendarDate initWithYear_month_day_hour_minute_second_timeZoneSelector year month day hour minute second (toNSTimeZone aTimeZone)

-- | @- setCalendarFormat:@
setCalendarFormat :: (IsNSCalendarDate nsCalendarDate, IsNSString format) => nsCalendarDate -> format -> IO ()
setCalendarFormat nsCalendarDate format =
  sendMessage nsCalendarDate setCalendarFormatSelector (toNSString format)

-- | @- setTimeZone:@
setTimeZone :: (IsNSCalendarDate nsCalendarDate, IsNSTimeZone aTimeZone) => nsCalendarDate -> aTimeZone -> IO ()
setTimeZone nsCalendarDate aTimeZone =
  sendMessage nsCalendarDate setTimeZoneSelector (toNSTimeZone aTimeZone)

-- | @- years:months:days:hours:minutes:seconds:sinceDate:@
years_months_days_hours_minutes_seconds_sinceDate :: (IsNSCalendarDate nsCalendarDate, IsNSCalendarDate date) => nsCalendarDate -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
years_months_days_hours_minutes_seconds_sinceDate nsCalendarDate yp mop dp hp mip sp date =
  sendMessage nsCalendarDate years_months_days_hours_minutes_seconds_sinceDateSelector yp mop dp hp mip sp (toNSCalendarDate date)

-- | @+ distantFuture@
distantFuture :: IO (Id NSCalendarDate)
distantFuture  =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMessage cls' distantFutureSelector

-- | @+ distantPast@
distantPast :: IO (Id NSCalendarDate)
distantPast  =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMessage cls' distantPastSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarDate@
calendarDateSelector :: Selector '[] RawId
calendarDateSelector = mkSelector "calendarDate"

-- | @Selector@ for @dateWithString:calendarFormat:locale:@
dateWithString_calendarFormat_localeSelector :: Selector '[Id NSString, Id NSString, RawId] RawId
dateWithString_calendarFormat_localeSelector = mkSelector "dateWithString:calendarFormat:locale:"

-- | @Selector@ for @dateWithString:calendarFormat:@
dateWithString_calendarFormatSelector :: Selector '[Id NSString, Id NSString] RawId
dateWithString_calendarFormatSelector = mkSelector "dateWithString:calendarFormat:"

-- | @Selector@ for @dateWithYear:month:day:hour:minute:second:timeZone:@
dateWithYear_month_day_hour_minute_second_timeZoneSelector :: Selector '[CLong, CULong, CULong, CULong, CULong, CULong, Id NSTimeZone] RawId
dateWithYear_month_day_hour_minute_second_timeZoneSelector = mkSelector "dateWithYear:month:day:hour:minute:second:timeZone:"

-- | @Selector@ for @dateByAddingYears:months:days:hours:minutes:seconds:@
dateByAddingYears_months_days_hours_minutes_secondsSelector :: Selector '[CLong, CLong, CLong, CLong, CLong, CLong] (Id NSCalendarDate)
dateByAddingYears_months_days_hours_minutes_secondsSelector = mkSelector "dateByAddingYears:months:days:hours:minutes:seconds:"

-- | @Selector@ for @dayOfCommonEra@
dayOfCommonEraSelector :: Selector '[] CLong
dayOfCommonEraSelector = mkSelector "dayOfCommonEra"

-- | @Selector@ for @dayOfMonth@
dayOfMonthSelector :: Selector '[] CLong
dayOfMonthSelector = mkSelector "dayOfMonth"

-- | @Selector@ for @dayOfWeek@
dayOfWeekSelector :: Selector '[] CLong
dayOfWeekSelector = mkSelector "dayOfWeek"

-- | @Selector@ for @dayOfYear@
dayOfYearSelector :: Selector '[] CLong
dayOfYearSelector = mkSelector "dayOfYear"

-- | @Selector@ for @hourOfDay@
hourOfDaySelector :: Selector '[] CLong
hourOfDaySelector = mkSelector "hourOfDay"

-- | @Selector@ for @minuteOfHour@
minuteOfHourSelector :: Selector '[] CLong
minuteOfHourSelector = mkSelector "minuteOfHour"

-- | @Selector@ for @monthOfYear@
monthOfYearSelector :: Selector '[] CLong
monthOfYearSelector = mkSelector "monthOfYear"

-- | @Selector@ for @secondOfMinute@
secondOfMinuteSelector :: Selector '[] CLong
secondOfMinuteSelector = mkSelector "secondOfMinute"

-- | @Selector@ for @yearOfCommonEra@
yearOfCommonEraSelector :: Selector '[] CLong
yearOfCommonEraSelector = mkSelector "yearOfCommonEra"

-- | @Selector@ for @calendarFormat@
calendarFormatSelector :: Selector '[] (Id NSString)
calendarFormatSelector = mkSelector "calendarFormat"

-- | @Selector@ for @descriptionWithCalendarFormat:locale:@
descriptionWithCalendarFormat_localeSelector :: Selector '[Id NSString, RawId] (Id NSString)
descriptionWithCalendarFormat_localeSelector = mkSelector "descriptionWithCalendarFormat:locale:"

-- | @Selector@ for @descriptionWithCalendarFormat:@
descriptionWithCalendarFormatSelector :: Selector '[Id NSString] (Id NSString)
descriptionWithCalendarFormatSelector = mkSelector "descriptionWithCalendarFormat:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @initWithString:calendarFormat:locale:@
initWithString_calendarFormat_localeSelector :: Selector '[Id NSString, Id NSString, RawId] RawId
initWithString_calendarFormat_localeSelector = mkSelector "initWithString:calendarFormat:locale:"

-- | @Selector@ for @initWithString:calendarFormat:@
initWithString_calendarFormatSelector :: Selector '[Id NSString, Id NSString] RawId
initWithString_calendarFormatSelector = mkSelector "initWithString:calendarFormat:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] RawId
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithYear:month:day:hour:minute:second:timeZone:@
initWithYear_month_day_hour_minute_second_timeZoneSelector :: Selector '[CLong, CULong, CULong, CULong, CULong, CULong, Id NSTimeZone] RawId
initWithYear_month_day_hour_minute_second_timeZoneSelector = mkSelector "initWithYear:month:day:hour:minute:second:timeZone:"

-- | @Selector@ for @setCalendarFormat:@
setCalendarFormatSelector :: Selector '[Id NSString] ()
setCalendarFormatSelector = mkSelector "setCalendarFormat:"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @years:months:days:hours:minutes:seconds:sinceDate:@
years_months_days_hours_minutes_seconds_sinceDateSelector :: Selector '[Ptr CLong, Ptr CLong, Ptr CLong, Ptr CLong, Ptr CLong, Ptr CLong, Id NSCalendarDate] ()
years_months_days_hours_minutes_seconds_sinceDateSelector = mkSelector "years:months:days:hours:minutes:seconds:sinceDate:"

-- | @Selector@ for @distantFuture@
distantFutureSelector :: Selector '[] (Id NSCalendarDate)
distantFutureSelector = mkSelector "distantFuture"

-- | @Selector@ for @distantPast@
distantPastSelector :: Selector '[] (Id NSCalendarDate)
distantPastSelector = mkSelector "distantPast"

