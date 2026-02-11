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
  , dateWithString_calendarFormat_localeSelector
  , dateWithString_calendarFormatSelector
  , dateWithYear_month_day_hour_minute_second_timeZoneSelector
  , dateByAddingYears_months_days_hours_minutes_secondsSelector
  , dayOfCommonEraSelector
  , dayOfMonthSelector
  , dayOfWeekSelector
  , dayOfYearSelector
  , hourOfDaySelector
  , minuteOfHourSelector
  , monthOfYearSelector
  , secondOfMinuteSelector
  , yearOfCommonEraSelector
  , calendarFormatSelector
  , descriptionWithCalendarFormat_localeSelector
  , descriptionWithCalendarFormatSelector
  , descriptionWithLocaleSelector
  , timeZoneSelector
  , initWithString_calendarFormat_localeSelector
  , initWithString_calendarFormatSelector
  , initWithStringSelector
  , initWithYear_month_day_hour_minute_second_timeZoneSelector
  , setCalendarFormatSelector
  , setTimeZoneSelector
  , years_months_days_hours_minutes_seconds_sinceDateSelector
  , distantFutureSelector
  , distantPastSelector


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

-- | @+ calendarDate@
calendarDate :: IO RawId
calendarDate  =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "calendarDate") (retPtr retVoid) []

-- | @+ dateWithString:calendarFormat:locale:@
dateWithString_calendarFormat_locale :: (IsNSString description, IsNSString format) => description -> format -> RawId -> IO RawId
dateWithString_calendarFormat_locale description format locale =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    withObjCPtr description $ \raw_description ->
      withObjCPtr format $ \raw_format ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dateWithString:calendarFormat:locale:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())]

-- | @+ dateWithString:calendarFormat:@
dateWithString_calendarFormat :: (IsNSString description, IsNSString format) => description -> format -> IO RawId
dateWithString_calendarFormat description format =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    withObjCPtr description $ \raw_description ->
      withObjCPtr format $ \raw_format ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dateWithString:calendarFormat:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_format :: Ptr ())]

-- | @+ dateWithYear:month:day:hour:minute:second:timeZone:@
dateWithYear_month_day_hour_minute_second_timeZone :: IsNSTimeZone aTimeZone => CLong -> CULong -> CULong -> CULong -> CULong -> CULong -> aTimeZone -> IO RawId
dateWithYear_month_day_hour_minute_second_timeZone year month day hour minute second aTimeZone =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    withObjCPtr aTimeZone $ \raw_aTimeZone ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dateWithYear:month:day:hour:minute:second:timeZone:") (retPtr retVoid) [argCLong (fromIntegral year), argCULong (fromIntegral month), argCULong (fromIntegral day), argCULong (fromIntegral hour), argCULong (fromIntegral minute), argCULong (fromIntegral second), argPtr (castPtr raw_aTimeZone :: Ptr ())]

-- | @- dateByAddingYears:months:days:hours:minutes:seconds:@
dateByAddingYears_months_days_hours_minutes_seconds :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id NSCalendarDate)
dateByAddingYears_months_days_hours_minutes_seconds nsCalendarDate  year month day hour minute second =
  sendMsg nsCalendarDate (mkSelector "dateByAddingYears:months:days:hours:minutes:seconds:") (retPtr retVoid) [argCLong (fromIntegral year), argCLong (fromIntegral month), argCLong (fromIntegral day), argCLong (fromIntegral hour), argCLong (fromIntegral minute), argCLong (fromIntegral second)] >>= retainedObject . castPtr

-- | @- dayOfCommonEra@
dayOfCommonEra :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfCommonEra nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "dayOfCommonEra") retCLong []

-- | @- dayOfMonth@
dayOfMonth :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfMonth nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "dayOfMonth") retCLong []

-- | @- dayOfWeek@
dayOfWeek :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfWeek nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "dayOfWeek") retCLong []

-- | @- dayOfYear@
dayOfYear :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
dayOfYear nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "dayOfYear") retCLong []

-- | @- hourOfDay@
hourOfDay :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
hourOfDay nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "hourOfDay") retCLong []

-- | @- minuteOfHour@
minuteOfHour :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
minuteOfHour nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "minuteOfHour") retCLong []

-- | @- monthOfYear@
monthOfYear :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
monthOfYear nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "monthOfYear") retCLong []

-- | @- secondOfMinute@
secondOfMinute :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
secondOfMinute nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "secondOfMinute") retCLong []

-- | @- yearOfCommonEra@
yearOfCommonEra :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO CLong
yearOfCommonEra nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "yearOfCommonEra") retCLong []

-- | @- calendarFormat@
calendarFormat :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO (Id NSString)
calendarFormat nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "calendarFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- descriptionWithCalendarFormat:locale:@
descriptionWithCalendarFormat_locale :: (IsNSCalendarDate nsCalendarDate, IsNSString format) => nsCalendarDate -> format -> RawId -> IO (Id NSString)
descriptionWithCalendarFormat_locale nsCalendarDate  format locale =
withObjCPtr format $ \raw_format ->
    sendMsg nsCalendarDate (mkSelector "descriptionWithCalendarFormat:locale:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithCalendarFormat:@
descriptionWithCalendarFormat :: (IsNSCalendarDate nsCalendarDate, IsNSString format) => nsCalendarDate -> format -> IO (Id NSString)
descriptionWithCalendarFormat nsCalendarDate  format =
withObjCPtr format $ \raw_format ->
    sendMsg nsCalendarDate (mkSelector "descriptionWithCalendarFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> RawId -> IO (Id NSString)
descriptionWithLocale nsCalendarDate  locale =
  sendMsg nsCalendarDate (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- timeZone@
timeZone :: IsNSCalendarDate nsCalendarDate => nsCalendarDate -> IO (Id NSTimeZone)
timeZone nsCalendarDate  =
  sendMsg nsCalendarDate (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithString:calendarFormat:locale:@
initWithString_calendarFormat_locale :: (IsNSCalendarDate nsCalendarDate, IsNSString description, IsNSString format) => nsCalendarDate -> description -> format -> RawId -> IO RawId
initWithString_calendarFormat_locale nsCalendarDate  description format locale =
withObjCPtr description $ \raw_description ->
  withObjCPtr format $ \raw_format ->
      fmap (RawId . castPtr) $ sendMsg nsCalendarDate (mkSelector "initWithString:calendarFormat:locale:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())]

-- | @- initWithString:calendarFormat:@
initWithString_calendarFormat :: (IsNSCalendarDate nsCalendarDate, IsNSString description, IsNSString format) => nsCalendarDate -> description -> format -> IO RawId
initWithString_calendarFormat nsCalendarDate  description format =
withObjCPtr description $ \raw_description ->
  withObjCPtr format $ \raw_format ->
      fmap (RawId . castPtr) $ sendMsg nsCalendarDate (mkSelector "initWithString:calendarFormat:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_format :: Ptr ())]

-- | @- initWithString:@
initWithString :: (IsNSCalendarDate nsCalendarDate, IsNSString description) => nsCalendarDate -> description -> IO RawId
initWithString nsCalendarDate  description =
withObjCPtr description $ \raw_description ->
    fmap (RawId . castPtr) $ sendMsg nsCalendarDate (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ())]

-- | @- initWithYear:month:day:hour:minute:second:timeZone:@
initWithYear_month_day_hour_minute_second_timeZone :: (IsNSCalendarDate nsCalendarDate, IsNSTimeZone aTimeZone) => nsCalendarDate -> CLong -> CULong -> CULong -> CULong -> CULong -> CULong -> aTimeZone -> IO RawId
initWithYear_month_day_hour_minute_second_timeZone nsCalendarDate  year month day hour minute second aTimeZone =
withObjCPtr aTimeZone $ \raw_aTimeZone ->
    fmap (RawId . castPtr) $ sendMsg nsCalendarDate (mkSelector "initWithYear:month:day:hour:minute:second:timeZone:") (retPtr retVoid) [argCLong (fromIntegral year), argCULong (fromIntegral month), argCULong (fromIntegral day), argCULong (fromIntegral hour), argCULong (fromIntegral minute), argCULong (fromIntegral second), argPtr (castPtr raw_aTimeZone :: Ptr ())]

-- | @- setCalendarFormat:@
setCalendarFormat :: (IsNSCalendarDate nsCalendarDate, IsNSString format) => nsCalendarDate -> format -> IO ()
setCalendarFormat nsCalendarDate  format =
withObjCPtr format $ \raw_format ->
    sendMsg nsCalendarDate (mkSelector "setCalendarFormat:") retVoid [argPtr (castPtr raw_format :: Ptr ())]

-- | @- setTimeZone:@
setTimeZone :: (IsNSCalendarDate nsCalendarDate, IsNSTimeZone aTimeZone) => nsCalendarDate -> aTimeZone -> IO ()
setTimeZone nsCalendarDate  aTimeZone =
withObjCPtr aTimeZone $ \raw_aTimeZone ->
    sendMsg nsCalendarDate (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_aTimeZone :: Ptr ())]

-- | @- years:months:days:hours:minutes:seconds:sinceDate:@
years_months_days_hours_minutes_seconds_sinceDate :: (IsNSCalendarDate nsCalendarDate, IsNSCalendarDate date) => nsCalendarDate -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
years_months_days_hours_minutes_seconds_sinceDate nsCalendarDate  yp mop dp hp mip sp date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendarDate (mkSelector "years:months:days:hours:minutes:seconds:sinceDate:") retVoid [argPtr yp, argPtr mop, argPtr dp, argPtr hp, argPtr mip, argPtr sp, argPtr (castPtr raw_date :: Ptr ())]

-- | @+ distantFuture@
distantFuture :: IO (Id NSCalendarDate)
distantFuture  =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMsg cls' (mkSelector "distantFuture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ distantPast@
distantPast :: IO (Id NSCalendarDate)
distantPast  =
  do
    cls' <- getRequiredClass "NSCalendarDate"
    sendClassMsg cls' (mkSelector "distantPast") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarDate@
calendarDateSelector :: Selector
calendarDateSelector = mkSelector "calendarDate"

-- | @Selector@ for @dateWithString:calendarFormat:locale:@
dateWithString_calendarFormat_localeSelector :: Selector
dateWithString_calendarFormat_localeSelector = mkSelector "dateWithString:calendarFormat:locale:"

-- | @Selector@ for @dateWithString:calendarFormat:@
dateWithString_calendarFormatSelector :: Selector
dateWithString_calendarFormatSelector = mkSelector "dateWithString:calendarFormat:"

-- | @Selector@ for @dateWithYear:month:day:hour:minute:second:timeZone:@
dateWithYear_month_day_hour_minute_second_timeZoneSelector :: Selector
dateWithYear_month_day_hour_minute_second_timeZoneSelector = mkSelector "dateWithYear:month:day:hour:minute:second:timeZone:"

-- | @Selector@ for @dateByAddingYears:months:days:hours:minutes:seconds:@
dateByAddingYears_months_days_hours_minutes_secondsSelector :: Selector
dateByAddingYears_months_days_hours_minutes_secondsSelector = mkSelector "dateByAddingYears:months:days:hours:minutes:seconds:"

-- | @Selector@ for @dayOfCommonEra@
dayOfCommonEraSelector :: Selector
dayOfCommonEraSelector = mkSelector "dayOfCommonEra"

-- | @Selector@ for @dayOfMonth@
dayOfMonthSelector :: Selector
dayOfMonthSelector = mkSelector "dayOfMonth"

-- | @Selector@ for @dayOfWeek@
dayOfWeekSelector :: Selector
dayOfWeekSelector = mkSelector "dayOfWeek"

-- | @Selector@ for @dayOfYear@
dayOfYearSelector :: Selector
dayOfYearSelector = mkSelector "dayOfYear"

-- | @Selector@ for @hourOfDay@
hourOfDaySelector :: Selector
hourOfDaySelector = mkSelector "hourOfDay"

-- | @Selector@ for @minuteOfHour@
minuteOfHourSelector :: Selector
minuteOfHourSelector = mkSelector "minuteOfHour"

-- | @Selector@ for @monthOfYear@
monthOfYearSelector :: Selector
monthOfYearSelector = mkSelector "monthOfYear"

-- | @Selector@ for @secondOfMinute@
secondOfMinuteSelector :: Selector
secondOfMinuteSelector = mkSelector "secondOfMinute"

-- | @Selector@ for @yearOfCommonEra@
yearOfCommonEraSelector :: Selector
yearOfCommonEraSelector = mkSelector "yearOfCommonEra"

-- | @Selector@ for @calendarFormat@
calendarFormatSelector :: Selector
calendarFormatSelector = mkSelector "calendarFormat"

-- | @Selector@ for @descriptionWithCalendarFormat:locale:@
descriptionWithCalendarFormat_localeSelector :: Selector
descriptionWithCalendarFormat_localeSelector = mkSelector "descriptionWithCalendarFormat:locale:"

-- | @Selector@ for @descriptionWithCalendarFormat:@
descriptionWithCalendarFormatSelector :: Selector
descriptionWithCalendarFormatSelector = mkSelector "descriptionWithCalendarFormat:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @initWithString:calendarFormat:locale:@
initWithString_calendarFormat_localeSelector :: Selector
initWithString_calendarFormat_localeSelector = mkSelector "initWithString:calendarFormat:locale:"

-- | @Selector@ for @initWithString:calendarFormat:@
initWithString_calendarFormatSelector :: Selector
initWithString_calendarFormatSelector = mkSelector "initWithString:calendarFormat:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithYear:month:day:hour:minute:second:timeZone:@
initWithYear_month_day_hour_minute_second_timeZoneSelector :: Selector
initWithYear_month_day_hour_minute_second_timeZoneSelector = mkSelector "initWithYear:month:day:hour:minute:second:timeZone:"

-- | @Selector@ for @setCalendarFormat:@
setCalendarFormatSelector :: Selector
setCalendarFormatSelector = mkSelector "setCalendarFormat:"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @years:months:days:hours:minutes:seconds:sinceDate:@
years_months_days_hours_minutes_seconds_sinceDateSelector :: Selector
years_months_days_hours_minutes_seconds_sinceDateSelector = mkSelector "years:months:days:hours:minutes:seconds:sinceDate:"

-- | @Selector@ for @distantFuture@
distantFutureSelector :: Selector
distantFutureSelector = mkSelector "distantFuture"

-- | @Selector@ for @distantPast@
distantPastSelector :: Selector
distantPastSelector = mkSelector "distantPast"

