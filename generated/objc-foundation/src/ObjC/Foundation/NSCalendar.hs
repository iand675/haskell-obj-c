{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCalendar@.
module ObjC.Foundation.NSCalendar
  ( NSCalendar
  , IsNSCalendar(..)
  , calendarWithIdentifier
  , init_
  , initWithCalendarIdentifier
  , minimumRangeOfUnit
  , maximumRangeOfUnit
  , rangeOfUnit_inUnit_forDate
  , ordinalityOfUnit_inUnit_forDate
  , rangeOfUnit_startDate_interval_forDate
  , dateFromComponents
  , components_fromDate
  , dateByAddingComponents_toDate_options
  , components_fromDate_toDate_options
  , getEra_year_month_day_fromDate
  , getEra_yearForWeekOfYear_weekOfYear_weekday_fromDate
  , getHour_minute_second_nanosecond_fromDate
  , component_fromDate
  , dateWithEra_year_month_day_hour_minute_second_nanosecond
  , dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecond
  , startOfDayForDate
  , componentsInTimeZone_fromDate
  , compareDate_toDate_toUnitGranularity
  , isDate_equalToDate_toUnitGranularity
  , isDate_inSameDayAsDate
  , isDateInToday
  , isDateInYesterday
  , isDateInTomorrow
  , isDateInWeekend
  , rangeOfWeekendStartDate_interval_containingDate
  , nextWeekendStartDate_interval_options_afterDate
  , components_fromDateComponents_toDateComponents_options
  , dateByAddingUnit_value_toDate_options
  , enumerateDatesStartingAfterDate_matchingComponents_options_usingBlock
  , nextDateAfterDate_matchingComponents_options
  , nextDateAfterDate_matchingUnit_value_options
  , nextDateAfterDate_matchingHour_minute_second_options
  , dateBySettingUnit_value_ofDate_options
  , dateBySettingHour_minute_second_ofDate_options
  , date_matchesComponents
  , currentCalendar
  , autoupdatingCurrentCalendar
  , calendarIdentifier
  , locale
  , setLocale
  , timeZone
  , setTimeZone
  , firstWeekday
  , setFirstWeekday
  , minimumDaysInFirstWeek
  , setMinimumDaysInFirstWeek
  , eraSymbols
  , longEraSymbols
  , monthSymbols
  , shortMonthSymbols
  , veryShortMonthSymbols
  , standaloneMonthSymbols
  , shortStandaloneMonthSymbols
  , veryShortStandaloneMonthSymbols
  , weekdaySymbols
  , shortWeekdaySymbols
  , veryShortWeekdaySymbols
  , standaloneWeekdaySymbols
  , shortStandaloneWeekdaySymbols
  , veryShortStandaloneWeekdaySymbols
  , quarterSymbols
  , shortQuarterSymbols
  , standaloneQuarterSymbols
  , shortStandaloneQuarterSymbols
  , amSymbol
  , pmSymbol
  , calendarWithIdentifierSelector
  , initSelector
  , initWithCalendarIdentifierSelector
  , minimumRangeOfUnitSelector
  , maximumRangeOfUnitSelector
  , rangeOfUnit_inUnit_forDateSelector
  , ordinalityOfUnit_inUnit_forDateSelector
  , rangeOfUnit_startDate_interval_forDateSelector
  , dateFromComponentsSelector
  , components_fromDateSelector
  , dateByAddingComponents_toDate_optionsSelector
  , components_fromDate_toDate_optionsSelector
  , getEra_year_month_day_fromDateSelector
  , getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector
  , getHour_minute_second_nanosecond_fromDateSelector
  , component_fromDateSelector
  , dateWithEra_year_month_day_hour_minute_second_nanosecondSelector
  , dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector
  , startOfDayForDateSelector
  , componentsInTimeZone_fromDateSelector
  , compareDate_toDate_toUnitGranularitySelector
  , isDate_equalToDate_toUnitGranularitySelector
  , isDate_inSameDayAsDateSelector
  , isDateInTodaySelector
  , isDateInYesterdaySelector
  , isDateInTomorrowSelector
  , isDateInWeekendSelector
  , rangeOfWeekendStartDate_interval_containingDateSelector
  , nextWeekendStartDate_interval_options_afterDateSelector
  , components_fromDateComponents_toDateComponents_optionsSelector
  , dateByAddingUnit_value_toDate_optionsSelector
  , enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector
  , nextDateAfterDate_matchingComponents_optionsSelector
  , nextDateAfterDate_matchingUnit_value_optionsSelector
  , nextDateAfterDate_matchingHour_minute_second_optionsSelector
  , dateBySettingUnit_value_ofDate_optionsSelector
  , dateBySettingHour_minute_second_ofDate_optionsSelector
  , date_matchesComponentsSelector
  , currentCalendarSelector
  , autoupdatingCurrentCalendarSelector
  , calendarIdentifierSelector
  , localeSelector
  , setLocaleSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , firstWeekdaySelector
  , setFirstWeekdaySelector
  , minimumDaysInFirstWeekSelector
  , setMinimumDaysInFirstWeekSelector
  , eraSymbolsSelector
  , longEraSymbolsSelector
  , monthSymbolsSelector
  , shortMonthSymbolsSelector
  , veryShortMonthSymbolsSelector
  , standaloneMonthSymbolsSelector
  , shortStandaloneMonthSymbolsSelector
  , veryShortStandaloneMonthSymbolsSelector
  , weekdaySymbolsSelector
  , shortWeekdaySymbolsSelector
  , veryShortWeekdaySymbolsSelector
  , standaloneWeekdaySymbolsSelector
  , shortStandaloneWeekdaySymbolsSelector
  , veryShortStandaloneWeekdaySymbolsSelector
  , quarterSymbolsSelector
  , shortQuarterSymbolsSelector
  , standaloneQuarterSymbolsSelector
  , shortStandaloneQuarterSymbolsSelector
  , amSymbolSelector
  , pmSymbolSelector

  -- * Enum types
  , NSCalendarOptions(NSCalendarOptions)
  , pattern NSCalendarWrapComponents
  , pattern NSCalendarMatchStrictly
  , pattern NSCalendarSearchBackwards
  , pattern NSCalendarMatchPreviousTimePreservingSmallerUnits
  , pattern NSCalendarMatchNextTimePreservingSmallerUnits
  , pattern NSCalendarMatchNextTime
  , pattern NSCalendarMatchFirst
  , pattern NSCalendarMatchLast
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
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @+ calendarWithIdentifier:@
calendarWithIdentifier :: IsNSString calendarIdentifierConstant => calendarIdentifierConstant -> IO (Id NSCalendar)
calendarWithIdentifier calendarIdentifierConstant =
  do
    cls' <- getRequiredClass "NSCalendar"
    withObjCPtr calendarIdentifierConstant $ \raw_calendarIdentifierConstant ->
      sendClassMsg cls' (mkSelector "calendarWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_calendarIdentifierConstant :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSCalendar)
init_ nsCalendar  =
  sendMsg nsCalendar (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCalendarIdentifier:@
initWithCalendarIdentifier :: (IsNSCalendar nsCalendar, IsNSString ident) => nsCalendar -> ident -> IO RawId
initWithCalendarIdentifier nsCalendar  ident =
withObjCPtr ident $ \raw_ident ->
    fmap (RawId . castPtr) $ sendMsg nsCalendar (mkSelector "initWithCalendarIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_ident :: Ptr ())]

-- | @- minimumRangeOfUnit:@
minimumRangeOfUnit :: IsNSCalendar nsCalendar => nsCalendar -> NSCalendarUnit -> IO NSRange
minimumRangeOfUnit nsCalendar  unit =
  sendMsgStret nsCalendar (mkSelector "minimumRangeOfUnit:") retNSRange [argCULong (coerce unit)]

-- | @- maximumRangeOfUnit:@
maximumRangeOfUnit :: IsNSCalendar nsCalendar => nsCalendar -> NSCalendarUnit -> IO NSRange
maximumRangeOfUnit nsCalendar  unit =
  sendMsgStret nsCalendar (mkSelector "maximumRangeOfUnit:") retNSRange [argCULong (coerce unit)]

-- | @- rangeOfUnit:inUnit:forDate:@
rangeOfUnit_inUnit_forDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> NSCalendarUnit -> date -> IO NSRange
rangeOfUnit_inUnit_forDate nsCalendar  smaller larger date =
withObjCPtr date $ \raw_date ->
    sendMsgStret nsCalendar (mkSelector "rangeOfUnit:inUnit:forDate:") retNSRange [argCULong (coerce smaller), argCULong (coerce larger), argPtr (castPtr raw_date :: Ptr ())]

-- | @- ordinalityOfUnit:inUnit:forDate:@
ordinalityOfUnit_inUnit_forDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> NSCalendarUnit -> date -> IO CULong
ordinalityOfUnit_inUnit_forDate nsCalendar  smaller larger date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "ordinalityOfUnit:inUnit:forDate:") retCULong [argCULong (coerce smaller), argCULong (coerce larger), argPtr (castPtr raw_date :: Ptr ())]

-- | @- rangeOfUnit:startDate:interval:forDate:@
rangeOfUnit_startDate_interval_forDate :: (IsNSCalendar nsCalendar, IsNSDate datep, IsNSDate date) => nsCalendar -> NSCalendarUnit -> datep -> Ptr CDouble -> date -> IO Bool
rangeOfUnit_startDate_interval_forDate nsCalendar  unit datep tip date =
withObjCPtr datep $ \raw_datep ->
  withObjCPtr date $ \raw_date ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "rangeOfUnit:startDate:interval:forDate:") retCULong [argCULong (coerce unit), argPtr (castPtr raw_datep :: Ptr ()), argPtr tip, argPtr (castPtr raw_date :: Ptr ())]

-- | @- dateFromComponents:@
dateFromComponents :: (IsNSCalendar nsCalendar, IsNSDateComponents comps) => nsCalendar -> comps -> IO (Id NSDate)
dateFromComponents nsCalendar  comps =
withObjCPtr comps $ \raw_comps ->
    sendMsg nsCalendar (mkSelector "dateFromComponents:") (retPtr retVoid) [argPtr (castPtr raw_comps :: Ptr ())] >>= retainedObject . castPtr

-- | @- components:fromDate:@
components_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> date -> IO (Id NSDateComponents)
components_fromDate nsCalendar  unitFlags date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "components:fromDate:") (retPtr retVoid) [argCULong (coerce unitFlags), argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- dateByAddingComponents:toDate:options:@
dateByAddingComponents_toDate_options :: (IsNSCalendar nsCalendar, IsNSDateComponents comps, IsNSDate date) => nsCalendar -> comps -> date -> NSCalendarOptions -> IO (Id NSDate)
dateByAddingComponents_toDate_options nsCalendar  comps date opts =
withObjCPtr comps $ \raw_comps ->
  withObjCPtr date $ \raw_date ->
      sendMsg nsCalendar (mkSelector "dateByAddingComponents:toDate:options:") (retPtr retVoid) [argPtr (castPtr raw_comps :: Ptr ()), argPtr (castPtr raw_date :: Ptr ()), argCULong (coerce opts)] >>= retainedObject . castPtr

-- | @- components:fromDate:toDate:options:@
components_fromDate_toDate_options :: (IsNSCalendar nsCalendar, IsNSDate startingDate, IsNSDate resultDate) => nsCalendar -> NSCalendarUnit -> startingDate -> resultDate -> NSCalendarOptions -> IO (Id NSDateComponents)
components_fromDate_toDate_options nsCalendar  unitFlags startingDate resultDate opts =
withObjCPtr startingDate $ \raw_startingDate ->
  withObjCPtr resultDate $ \raw_resultDate ->
      sendMsg nsCalendar (mkSelector "components:fromDate:toDate:options:") (retPtr retVoid) [argCULong (coerce unitFlags), argPtr (castPtr raw_startingDate :: Ptr ()), argPtr (castPtr raw_resultDate :: Ptr ()), argCULong (coerce opts)] >>= retainedObject . castPtr

-- | @- getEra:year:month:day:fromDate:@
getEra_year_month_day_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
getEra_year_month_day_fromDate nsCalendar  eraValuePointer yearValuePointer monthValuePointer dayValuePointer date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "getEra:year:month:day:fromDate:") retVoid [argPtr eraValuePointer, argPtr yearValuePointer, argPtr monthValuePointer, argPtr dayValuePointer, argPtr (castPtr raw_date :: Ptr ())]

-- | @- getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:@
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDate nsCalendar  eraValuePointer yearValuePointer weekValuePointer weekdayValuePointer date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:") retVoid [argPtr eraValuePointer, argPtr yearValuePointer, argPtr weekValuePointer, argPtr weekdayValuePointer, argPtr (castPtr raw_date :: Ptr ())]

-- | @- getHour:minute:second:nanosecond:fromDate:@
getHour_minute_second_nanosecond_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
getHour_minute_second_nanosecond_fromDate nsCalendar  hourValuePointer minuteValuePointer secondValuePointer nanosecondValuePointer date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "getHour:minute:second:nanosecond:fromDate:") retVoid [argPtr hourValuePointer, argPtr minuteValuePointer, argPtr secondValuePointer, argPtr nanosecondValuePointer, argPtr (castPtr raw_date :: Ptr ())]

-- | @- component:fromDate:@
component_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> date -> IO CLong
component_fromDate nsCalendar  unit date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "component:fromDate:") retCLong [argCULong (coerce unit), argPtr (castPtr raw_date :: Ptr ())]

-- | @- dateWithEra:year:month:day:hour:minute:second:nanosecond:@
dateWithEra_year_month_day_hour_minute_second_nanosecond :: IsNSCalendar nsCalendar => nsCalendar -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id NSDate)
dateWithEra_year_month_day_hour_minute_second_nanosecond nsCalendar  eraValue yearValue monthValue dayValue hourValue minuteValue secondValue nanosecondValue =
  sendMsg nsCalendar (mkSelector "dateWithEra:year:month:day:hour:minute:second:nanosecond:") (retPtr retVoid) [argCLong (fromIntegral eraValue), argCLong (fromIntegral yearValue), argCLong (fromIntegral monthValue), argCLong (fromIntegral dayValue), argCLong (fromIntegral hourValue), argCLong (fromIntegral minuteValue), argCLong (fromIntegral secondValue), argCLong (fromIntegral nanosecondValue)] >>= retainedObject . castPtr

-- | @- dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:@
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecond :: IsNSCalendar nsCalendar => nsCalendar -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id NSDate)
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecond nsCalendar  eraValue yearValue weekValue weekdayValue hourValue minuteValue secondValue nanosecondValue =
  sendMsg nsCalendar (mkSelector "dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:") (retPtr retVoid) [argCLong (fromIntegral eraValue), argCLong (fromIntegral yearValue), argCLong (fromIntegral weekValue), argCLong (fromIntegral weekdayValue), argCLong (fromIntegral hourValue), argCLong (fromIntegral minuteValue), argCLong (fromIntegral secondValue), argCLong (fromIntegral nanosecondValue)] >>= retainedObject . castPtr

-- | @- startOfDayForDate:@
startOfDayForDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO (Id NSDate)
startOfDayForDate nsCalendar  date =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "startOfDayForDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- componentsInTimeZone:fromDate:@
componentsInTimeZone_fromDate :: (IsNSCalendar nsCalendar, IsNSTimeZone timezone, IsNSDate date) => nsCalendar -> timezone -> date -> IO (Id NSDateComponents)
componentsInTimeZone_fromDate nsCalendar  timezone date =
withObjCPtr timezone $ \raw_timezone ->
  withObjCPtr date $ \raw_date ->
      sendMsg nsCalendar (mkSelector "componentsInTimeZone:fromDate:") (retPtr retVoid) [argPtr (castPtr raw_timezone :: Ptr ()), argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- compareDate:toDate:toUnitGranularity:@
compareDate_toDate_toUnitGranularity :: (IsNSCalendar nsCalendar, IsNSDate date1, IsNSDate date2) => nsCalendar -> date1 -> date2 -> NSCalendarUnit -> IO NSComparisonResult
compareDate_toDate_toUnitGranularity nsCalendar  date1 date2 unit =
withObjCPtr date1 $ \raw_date1 ->
  withObjCPtr date2 $ \raw_date2 ->
      fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsCalendar (mkSelector "compareDate:toDate:toUnitGranularity:") retCLong [argPtr (castPtr raw_date1 :: Ptr ()), argPtr (castPtr raw_date2 :: Ptr ()), argCULong (coerce unit)]

-- | @- isDate:equalToDate:toUnitGranularity:@
isDate_equalToDate_toUnitGranularity :: (IsNSCalendar nsCalendar, IsNSDate date1, IsNSDate date2) => nsCalendar -> date1 -> date2 -> NSCalendarUnit -> IO Bool
isDate_equalToDate_toUnitGranularity nsCalendar  date1 date2 unit =
withObjCPtr date1 $ \raw_date1 ->
  withObjCPtr date2 $ \raw_date2 ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "isDate:equalToDate:toUnitGranularity:") retCULong [argPtr (castPtr raw_date1 :: Ptr ()), argPtr (castPtr raw_date2 :: Ptr ()), argCULong (coerce unit)]

-- | @- isDate:inSameDayAsDate:@
isDate_inSameDayAsDate :: (IsNSCalendar nsCalendar, IsNSDate date1, IsNSDate date2) => nsCalendar -> date1 -> date2 -> IO Bool
isDate_inSameDayAsDate nsCalendar  date1 date2 =
withObjCPtr date1 $ \raw_date1 ->
  withObjCPtr date2 $ \raw_date2 ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "isDate:inSameDayAsDate:") retCULong [argPtr (castPtr raw_date1 :: Ptr ()), argPtr (castPtr raw_date2 :: Ptr ())]

-- | @- isDateInToday:@
isDateInToday :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInToday nsCalendar  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "isDateInToday:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | @- isDateInYesterday:@
isDateInYesterday :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInYesterday nsCalendar  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "isDateInYesterday:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | @- isDateInTomorrow:@
isDateInTomorrow :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInTomorrow nsCalendar  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "isDateInTomorrow:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | @- isDateInWeekend:@
isDateInWeekend :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInWeekend nsCalendar  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "isDateInWeekend:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | @- rangeOfWeekendStartDate:interval:containingDate:@
rangeOfWeekendStartDate_interval_containingDate :: (IsNSCalendar nsCalendar, IsNSDate datep, IsNSDate date) => nsCalendar -> datep -> Ptr CDouble -> date -> IO Bool
rangeOfWeekendStartDate_interval_containingDate nsCalendar  datep tip date =
withObjCPtr datep $ \raw_datep ->
  withObjCPtr date $ \raw_date ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "rangeOfWeekendStartDate:interval:containingDate:") retCULong [argPtr (castPtr raw_datep :: Ptr ()), argPtr tip, argPtr (castPtr raw_date :: Ptr ())]

-- | @- nextWeekendStartDate:interval:options:afterDate:@
nextWeekendStartDate_interval_options_afterDate :: (IsNSCalendar nsCalendar, IsNSDate datep, IsNSDate date) => nsCalendar -> datep -> Ptr CDouble -> NSCalendarOptions -> date -> IO Bool
nextWeekendStartDate_interval_options_afterDate nsCalendar  datep tip options date =
withObjCPtr datep $ \raw_datep ->
  withObjCPtr date $ \raw_date ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "nextWeekendStartDate:interval:options:afterDate:") retCULong [argPtr (castPtr raw_datep :: Ptr ()), argPtr tip, argCULong (coerce options), argPtr (castPtr raw_date :: Ptr ())]

-- | @- components:fromDateComponents:toDateComponents:options:@
components_fromDateComponents_toDateComponents_options :: (IsNSCalendar nsCalendar, IsNSDateComponents startingDateComp, IsNSDateComponents resultDateComp) => nsCalendar -> NSCalendarUnit -> startingDateComp -> resultDateComp -> NSCalendarOptions -> IO (Id NSDateComponents)
components_fromDateComponents_toDateComponents_options nsCalendar  unitFlags startingDateComp resultDateComp options =
withObjCPtr startingDateComp $ \raw_startingDateComp ->
  withObjCPtr resultDateComp $ \raw_resultDateComp ->
      sendMsg nsCalendar (mkSelector "components:fromDateComponents:toDateComponents:options:") (retPtr retVoid) [argCULong (coerce unitFlags), argPtr (castPtr raw_startingDateComp :: Ptr ()), argPtr (castPtr raw_resultDateComp :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- dateByAddingUnit:value:toDate:options:@
dateByAddingUnit_value_toDate_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> CLong -> date -> NSCalendarOptions -> IO (Id NSDate)
dateByAddingUnit_value_toDate_options nsCalendar  unit value date options =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "dateByAddingUnit:value:toDate:options:") (retPtr retVoid) [argCULong (coerce unit), argCLong (fromIntegral value), argPtr (castPtr raw_date :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:@
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlock :: (IsNSCalendar nsCalendar, IsNSDate start, IsNSDateComponents comps) => nsCalendar -> start -> comps -> NSCalendarOptions -> Ptr () -> IO ()
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlock nsCalendar  start comps opts block =
withObjCPtr start $ \raw_start ->
  withObjCPtr comps $ \raw_comps ->
      sendMsg nsCalendar (mkSelector "enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:") retVoid [argPtr (castPtr raw_start :: Ptr ()), argPtr (castPtr raw_comps :: Ptr ()), argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- nextDateAfterDate:matchingComponents:options:@
nextDateAfterDate_matchingComponents_options :: (IsNSCalendar nsCalendar, IsNSDate date, IsNSDateComponents comps) => nsCalendar -> date -> comps -> NSCalendarOptions -> IO (Id NSDate)
nextDateAfterDate_matchingComponents_options nsCalendar  date comps options =
withObjCPtr date $ \raw_date ->
  withObjCPtr comps $ \raw_comps ->
      sendMsg nsCalendar (mkSelector "nextDateAfterDate:matchingComponents:options:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_comps :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- nextDateAfterDate:matchingUnit:value:options:@
nextDateAfterDate_matchingUnit_value_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> NSCalendarUnit -> CLong -> NSCalendarOptions -> IO (Id NSDate)
nextDateAfterDate_matchingUnit_value_options nsCalendar  date unit value options =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "nextDateAfterDate:matchingUnit:value:options:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCULong (coerce unit), argCLong (fromIntegral value), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- nextDateAfterDate:matchingHour:minute:second:options:@
nextDateAfterDate_matchingHour_minute_second_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> CLong -> CLong -> CLong -> NSCalendarOptions -> IO (Id NSDate)
nextDateAfterDate_matchingHour_minute_second_options nsCalendar  date hourValue minuteValue secondValue options =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "nextDateAfterDate:matchingHour:minute:second:options:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCLong (fromIntegral hourValue), argCLong (fromIntegral minuteValue), argCLong (fromIntegral secondValue), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- dateBySettingUnit:value:ofDate:options:@
dateBySettingUnit_value_ofDate_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> CLong -> date -> NSCalendarOptions -> IO (Id NSDate)
dateBySettingUnit_value_ofDate_options nsCalendar  unit v date opts =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "dateBySettingUnit:value:ofDate:options:") (retPtr retVoid) [argCULong (coerce unit), argCLong (fromIntegral v), argPtr (castPtr raw_date :: Ptr ()), argCULong (coerce opts)] >>= retainedObject . castPtr

-- | @- dateBySettingHour:minute:second:ofDate:options:@
dateBySettingHour_minute_second_ofDate_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> CLong -> CLong -> CLong -> date -> NSCalendarOptions -> IO (Id NSDate)
dateBySettingHour_minute_second_ofDate_options nsCalendar  h m s date opts =
withObjCPtr date $ \raw_date ->
    sendMsg nsCalendar (mkSelector "dateBySettingHour:minute:second:ofDate:options:") (retPtr retVoid) [argCLong (fromIntegral h), argCLong (fromIntegral m), argCLong (fromIntegral s), argPtr (castPtr raw_date :: Ptr ()), argCULong (coerce opts)] >>= retainedObject . castPtr

-- | @- date:matchesComponents:@
date_matchesComponents :: (IsNSCalendar nsCalendar, IsNSDate date, IsNSDateComponents components) => nsCalendar -> date -> components -> IO Bool
date_matchesComponents nsCalendar  date components =
withObjCPtr date $ \raw_date ->
  withObjCPtr components $ \raw_components ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCalendar (mkSelector "date:matchesComponents:") retCULong [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_components :: Ptr ())]

-- | @+ currentCalendar@
currentCalendar :: IO (Id NSCalendar)
currentCalendar  =
  do
    cls' <- getRequiredClass "NSCalendar"
    sendClassMsg cls' (mkSelector "currentCalendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ autoupdatingCurrentCalendar@
autoupdatingCurrentCalendar :: IO (Id NSCalendar)
autoupdatingCurrentCalendar  =
  do
    cls' <- getRequiredClass "NSCalendar"
    sendClassMsg cls' (mkSelector "autoupdatingCurrentCalendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- calendarIdentifier@
calendarIdentifier :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSString)
calendarIdentifier nsCalendar  =
  sendMsg nsCalendar (mkSelector "calendarIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- locale@
locale :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSLocale)
locale nsCalendar  =
  sendMsg nsCalendar (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSCalendar nsCalendar, IsNSLocale value) => nsCalendar -> value -> IO ()
setLocale nsCalendar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCalendar (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeZone@
timeZone :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSTimeZone)
timeZone nsCalendar  =
  sendMsg nsCalendar (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSCalendar nsCalendar, IsNSTimeZone value) => nsCalendar -> value -> IO ()
setTimeZone nsCalendar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCalendar (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- firstWeekday@
firstWeekday :: IsNSCalendar nsCalendar => nsCalendar -> IO CULong
firstWeekday nsCalendar  =
  sendMsg nsCalendar (mkSelector "firstWeekday") retCULong []

-- | @- setFirstWeekday:@
setFirstWeekday :: IsNSCalendar nsCalendar => nsCalendar -> CULong -> IO ()
setFirstWeekday nsCalendar  value =
  sendMsg nsCalendar (mkSelector "setFirstWeekday:") retVoid [argCULong (fromIntegral value)]

-- | @- minimumDaysInFirstWeek@
minimumDaysInFirstWeek :: IsNSCalendar nsCalendar => nsCalendar -> IO CULong
minimumDaysInFirstWeek nsCalendar  =
  sendMsg nsCalendar (mkSelector "minimumDaysInFirstWeek") retCULong []

-- | @- setMinimumDaysInFirstWeek:@
setMinimumDaysInFirstWeek :: IsNSCalendar nsCalendar => nsCalendar -> CULong -> IO ()
setMinimumDaysInFirstWeek nsCalendar  value =
  sendMsg nsCalendar (mkSelector "setMinimumDaysInFirstWeek:") retVoid [argCULong (fromIntegral value)]

-- | @- eraSymbols@
eraSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
eraSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "eraSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- longEraSymbols@
longEraSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
longEraSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "longEraSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- monthSymbols@
monthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
monthSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "monthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortMonthSymbols@
shortMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortMonthSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "shortMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- veryShortMonthSymbols@
veryShortMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortMonthSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "veryShortMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- standaloneMonthSymbols@
standaloneMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
standaloneMonthSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "standaloneMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortStandaloneMonthSymbols@
shortStandaloneMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortStandaloneMonthSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "shortStandaloneMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortStandaloneMonthSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "veryShortStandaloneMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- weekdaySymbols@
weekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
weekdaySymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "weekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortWeekdaySymbols@
shortWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortWeekdaySymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "shortWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- veryShortWeekdaySymbols@
veryShortWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortWeekdaySymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "veryShortWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- standaloneWeekdaySymbols@
standaloneWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
standaloneWeekdaySymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "standaloneWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortStandaloneWeekdaySymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "shortStandaloneWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortStandaloneWeekdaySymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "veryShortStandaloneWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quarterSymbols@
quarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
quarterSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "quarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortQuarterSymbols@
shortQuarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortQuarterSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "shortQuarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- standaloneQuarterSymbols@
standaloneQuarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
standaloneQuarterSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "standaloneQuarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortStandaloneQuarterSymbols nsCalendar  =
  sendMsg nsCalendar (mkSelector "shortStandaloneQuarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- AMSymbol@
amSymbol :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSString)
amSymbol nsCalendar  =
  sendMsg nsCalendar (mkSelector "AMSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- PMSymbol@
pmSymbol :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSString)
pmSymbol nsCalendar  =
  sendMsg nsCalendar (mkSelector "PMSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarWithIdentifier:@
calendarWithIdentifierSelector :: Selector
calendarWithIdentifierSelector = mkSelector "calendarWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCalendarIdentifier:@
initWithCalendarIdentifierSelector :: Selector
initWithCalendarIdentifierSelector = mkSelector "initWithCalendarIdentifier:"

-- | @Selector@ for @minimumRangeOfUnit:@
minimumRangeOfUnitSelector :: Selector
minimumRangeOfUnitSelector = mkSelector "minimumRangeOfUnit:"

-- | @Selector@ for @maximumRangeOfUnit:@
maximumRangeOfUnitSelector :: Selector
maximumRangeOfUnitSelector = mkSelector "maximumRangeOfUnit:"

-- | @Selector@ for @rangeOfUnit:inUnit:forDate:@
rangeOfUnit_inUnit_forDateSelector :: Selector
rangeOfUnit_inUnit_forDateSelector = mkSelector "rangeOfUnit:inUnit:forDate:"

-- | @Selector@ for @ordinalityOfUnit:inUnit:forDate:@
ordinalityOfUnit_inUnit_forDateSelector :: Selector
ordinalityOfUnit_inUnit_forDateSelector = mkSelector "ordinalityOfUnit:inUnit:forDate:"

-- | @Selector@ for @rangeOfUnit:startDate:interval:forDate:@
rangeOfUnit_startDate_interval_forDateSelector :: Selector
rangeOfUnit_startDate_interval_forDateSelector = mkSelector "rangeOfUnit:startDate:interval:forDate:"

-- | @Selector@ for @dateFromComponents:@
dateFromComponentsSelector :: Selector
dateFromComponentsSelector = mkSelector "dateFromComponents:"

-- | @Selector@ for @components:fromDate:@
components_fromDateSelector :: Selector
components_fromDateSelector = mkSelector "components:fromDate:"

-- | @Selector@ for @dateByAddingComponents:toDate:options:@
dateByAddingComponents_toDate_optionsSelector :: Selector
dateByAddingComponents_toDate_optionsSelector = mkSelector "dateByAddingComponents:toDate:options:"

-- | @Selector@ for @components:fromDate:toDate:options:@
components_fromDate_toDate_optionsSelector :: Selector
components_fromDate_toDate_optionsSelector = mkSelector "components:fromDate:toDate:options:"

-- | @Selector@ for @getEra:year:month:day:fromDate:@
getEra_year_month_day_fromDateSelector :: Selector
getEra_year_month_day_fromDateSelector = mkSelector "getEra:year:month:day:fromDate:"

-- | @Selector@ for @getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:@
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector :: Selector
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector = mkSelector "getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:"

-- | @Selector@ for @getHour:minute:second:nanosecond:fromDate:@
getHour_minute_second_nanosecond_fromDateSelector :: Selector
getHour_minute_second_nanosecond_fromDateSelector = mkSelector "getHour:minute:second:nanosecond:fromDate:"

-- | @Selector@ for @component:fromDate:@
component_fromDateSelector :: Selector
component_fromDateSelector = mkSelector "component:fromDate:"

-- | @Selector@ for @dateWithEra:year:month:day:hour:minute:second:nanosecond:@
dateWithEra_year_month_day_hour_minute_second_nanosecondSelector :: Selector
dateWithEra_year_month_day_hour_minute_second_nanosecondSelector = mkSelector "dateWithEra:year:month:day:hour:minute:second:nanosecond:"

-- | @Selector@ for @dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:@
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector :: Selector
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector = mkSelector "dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:"

-- | @Selector@ for @startOfDayForDate:@
startOfDayForDateSelector :: Selector
startOfDayForDateSelector = mkSelector "startOfDayForDate:"

-- | @Selector@ for @componentsInTimeZone:fromDate:@
componentsInTimeZone_fromDateSelector :: Selector
componentsInTimeZone_fromDateSelector = mkSelector "componentsInTimeZone:fromDate:"

-- | @Selector@ for @compareDate:toDate:toUnitGranularity:@
compareDate_toDate_toUnitGranularitySelector :: Selector
compareDate_toDate_toUnitGranularitySelector = mkSelector "compareDate:toDate:toUnitGranularity:"

-- | @Selector@ for @isDate:equalToDate:toUnitGranularity:@
isDate_equalToDate_toUnitGranularitySelector :: Selector
isDate_equalToDate_toUnitGranularitySelector = mkSelector "isDate:equalToDate:toUnitGranularity:"

-- | @Selector@ for @isDate:inSameDayAsDate:@
isDate_inSameDayAsDateSelector :: Selector
isDate_inSameDayAsDateSelector = mkSelector "isDate:inSameDayAsDate:"

-- | @Selector@ for @isDateInToday:@
isDateInTodaySelector :: Selector
isDateInTodaySelector = mkSelector "isDateInToday:"

-- | @Selector@ for @isDateInYesterday:@
isDateInYesterdaySelector :: Selector
isDateInYesterdaySelector = mkSelector "isDateInYesterday:"

-- | @Selector@ for @isDateInTomorrow:@
isDateInTomorrowSelector :: Selector
isDateInTomorrowSelector = mkSelector "isDateInTomorrow:"

-- | @Selector@ for @isDateInWeekend:@
isDateInWeekendSelector :: Selector
isDateInWeekendSelector = mkSelector "isDateInWeekend:"

-- | @Selector@ for @rangeOfWeekendStartDate:interval:containingDate:@
rangeOfWeekendStartDate_interval_containingDateSelector :: Selector
rangeOfWeekendStartDate_interval_containingDateSelector = mkSelector "rangeOfWeekendStartDate:interval:containingDate:"

-- | @Selector@ for @nextWeekendStartDate:interval:options:afterDate:@
nextWeekendStartDate_interval_options_afterDateSelector :: Selector
nextWeekendStartDate_interval_options_afterDateSelector = mkSelector "nextWeekendStartDate:interval:options:afterDate:"

-- | @Selector@ for @components:fromDateComponents:toDateComponents:options:@
components_fromDateComponents_toDateComponents_optionsSelector :: Selector
components_fromDateComponents_toDateComponents_optionsSelector = mkSelector "components:fromDateComponents:toDateComponents:options:"

-- | @Selector@ for @dateByAddingUnit:value:toDate:options:@
dateByAddingUnit_value_toDate_optionsSelector :: Selector
dateByAddingUnit_value_toDate_optionsSelector = mkSelector "dateByAddingUnit:value:toDate:options:"

-- | @Selector@ for @enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:@
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector :: Selector
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector = mkSelector "enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:"

-- | @Selector@ for @nextDateAfterDate:matchingComponents:options:@
nextDateAfterDate_matchingComponents_optionsSelector :: Selector
nextDateAfterDate_matchingComponents_optionsSelector = mkSelector "nextDateAfterDate:matchingComponents:options:"

-- | @Selector@ for @nextDateAfterDate:matchingUnit:value:options:@
nextDateAfterDate_matchingUnit_value_optionsSelector :: Selector
nextDateAfterDate_matchingUnit_value_optionsSelector = mkSelector "nextDateAfterDate:matchingUnit:value:options:"

-- | @Selector@ for @nextDateAfterDate:matchingHour:minute:second:options:@
nextDateAfterDate_matchingHour_minute_second_optionsSelector :: Selector
nextDateAfterDate_matchingHour_minute_second_optionsSelector = mkSelector "nextDateAfterDate:matchingHour:minute:second:options:"

-- | @Selector@ for @dateBySettingUnit:value:ofDate:options:@
dateBySettingUnit_value_ofDate_optionsSelector :: Selector
dateBySettingUnit_value_ofDate_optionsSelector = mkSelector "dateBySettingUnit:value:ofDate:options:"

-- | @Selector@ for @dateBySettingHour:minute:second:ofDate:options:@
dateBySettingHour_minute_second_ofDate_optionsSelector :: Selector
dateBySettingHour_minute_second_ofDate_optionsSelector = mkSelector "dateBySettingHour:minute:second:ofDate:options:"

-- | @Selector@ for @date:matchesComponents:@
date_matchesComponentsSelector :: Selector
date_matchesComponentsSelector = mkSelector "date:matchesComponents:"

-- | @Selector@ for @currentCalendar@
currentCalendarSelector :: Selector
currentCalendarSelector = mkSelector "currentCalendar"

-- | @Selector@ for @autoupdatingCurrentCalendar@
autoupdatingCurrentCalendarSelector :: Selector
autoupdatingCurrentCalendarSelector = mkSelector "autoupdatingCurrentCalendar"

-- | @Selector@ for @calendarIdentifier@
calendarIdentifierSelector :: Selector
calendarIdentifierSelector = mkSelector "calendarIdentifier"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @firstWeekday@
firstWeekdaySelector :: Selector
firstWeekdaySelector = mkSelector "firstWeekday"

-- | @Selector@ for @setFirstWeekday:@
setFirstWeekdaySelector :: Selector
setFirstWeekdaySelector = mkSelector "setFirstWeekday:"

-- | @Selector@ for @minimumDaysInFirstWeek@
minimumDaysInFirstWeekSelector :: Selector
minimumDaysInFirstWeekSelector = mkSelector "minimumDaysInFirstWeek"

-- | @Selector@ for @setMinimumDaysInFirstWeek:@
setMinimumDaysInFirstWeekSelector :: Selector
setMinimumDaysInFirstWeekSelector = mkSelector "setMinimumDaysInFirstWeek:"

-- | @Selector@ for @eraSymbols@
eraSymbolsSelector :: Selector
eraSymbolsSelector = mkSelector "eraSymbols"

-- | @Selector@ for @longEraSymbols@
longEraSymbolsSelector :: Selector
longEraSymbolsSelector = mkSelector "longEraSymbols"

-- | @Selector@ for @monthSymbols@
monthSymbolsSelector :: Selector
monthSymbolsSelector = mkSelector "monthSymbols"

-- | @Selector@ for @shortMonthSymbols@
shortMonthSymbolsSelector :: Selector
shortMonthSymbolsSelector = mkSelector "shortMonthSymbols"

-- | @Selector@ for @veryShortMonthSymbols@
veryShortMonthSymbolsSelector :: Selector
veryShortMonthSymbolsSelector = mkSelector "veryShortMonthSymbols"

-- | @Selector@ for @standaloneMonthSymbols@
standaloneMonthSymbolsSelector :: Selector
standaloneMonthSymbolsSelector = mkSelector "standaloneMonthSymbols"

-- | @Selector@ for @shortStandaloneMonthSymbols@
shortStandaloneMonthSymbolsSelector :: Selector
shortStandaloneMonthSymbolsSelector = mkSelector "shortStandaloneMonthSymbols"

-- | @Selector@ for @veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbolsSelector :: Selector
veryShortStandaloneMonthSymbolsSelector = mkSelector "veryShortStandaloneMonthSymbols"

-- | @Selector@ for @weekdaySymbols@
weekdaySymbolsSelector :: Selector
weekdaySymbolsSelector = mkSelector "weekdaySymbols"

-- | @Selector@ for @shortWeekdaySymbols@
shortWeekdaySymbolsSelector :: Selector
shortWeekdaySymbolsSelector = mkSelector "shortWeekdaySymbols"

-- | @Selector@ for @veryShortWeekdaySymbols@
veryShortWeekdaySymbolsSelector :: Selector
veryShortWeekdaySymbolsSelector = mkSelector "veryShortWeekdaySymbols"

-- | @Selector@ for @standaloneWeekdaySymbols@
standaloneWeekdaySymbolsSelector :: Selector
standaloneWeekdaySymbolsSelector = mkSelector "standaloneWeekdaySymbols"

-- | @Selector@ for @shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbolsSelector :: Selector
shortStandaloneWeekdaySymbolsSelector = mkSelector "shortStandaloneWeekdaySymbols"

-- | @Selector@ for @veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbolsSelector :: Selector
veryShortStandaloneWeekdaySymbolsSelector = mkSelector "veryShortStandaloneWeekdaySymbols"

-- | @Selector@ for @quarterSymbols@
quarterSymbolsSelector :: Selector
quarterSymbolsSelector = mkSelector "quarterSymbols"

-- | @Selector@ for @shortQuarterSymbols@
shortQuarterSymbolsSelector :: Selector
shortQuarterSymbolsSelector = mkSelector "shortQuarterSymbols"

-- | @Selector@ for @standaloneQuarterSymbols@
standaloneQuarterSymbolsSelector :: Selector
standaloneQuarterSymbolsSelector = mkSelector "standaloneQuarterSymbols"

-- | @Selector@ for @shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbolsSelector :: Selector
shortStandaloneQuarterSymbolsSelector = mkSelector "shortStandaloneQuarterSymbols"

-- | @Selector@ for @AMSymbol@
amSymbolSelector :: Selector
amSymbolSelector = mkSelector "AMSymbol"

-- | @Selector@ for @PMSymbol@
pmSymbolSelector :: Selector
pmSymbolSelector = mkSelector "PMSymbol"

