{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , amSymbolSelector
  , autoupdatingCurrentCalendarSelector
  , calendarIdentifierSelector
  , calendarWithIdentifierSelector
  , compareDate_toDate_toUnitGranularitySelector
  , component_fromDateSelector
  , componentsInTimeZone_fromDateSelector
  , components_fromDateComponents_toDateComponents_optionsSelector
  , components_fromDateSelector
  , components_fromDate_toDate_optionsSelector
  , currentCalendarSelector
  , dateByAddingComponents_toDate_optionsSelector
  , dateByAddingUnit_value_toDate_optionsSelector
  , dateBySettingHour_minute_second_ofDate_optionsSelector
  , dateBySettingUnit_value_ofDate_optionsSelector
  , dateFromComponentsSelector
  , dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector
  , dateWithEra_year_month_day_hour_minute_second_nanosecondSelector
  , date_matchesComponentsSelector
  , enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector
  , eraSymbolsSelector
  , firstWeekdaySelector
  , getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector
  , getEra_year_month_day_fromDateSelector
  , getHour_minute_second_nanosecond_fromDateSelector
  , initSelector
  , initWithCalendarIdentifierSelector
  , isDateInTodaySelector
  , isDateInTomorrowSelector
  , isDateInWeekendSelector
  , isDateInYesterdaySelector
  , isDate_equalToDate_toUnitGranularitySelector
  , isDate_inSameDayAsDateSelector
  , localeSelector
  , longEraSymbolsSelector
  , maximumRangeOfUnitSelector
  , minimumDaysInFirstWeekSelector
  , minimumRangeOfUnitSelector
  , monthSymbolsSelector
  , nextDateAfterDate_matchingComponents_optionsSelector
  , nextDateAfterDate_matchingHour_minute_second_optionsSelector
  , nextDateAfterDate_matchingUnit_value_optionsSelector
  , nextWeekendStartDate_interval_options_afterDateSelector
  , ordinalityOfUnit_inUnit_forDateSelector
  , pmSymbolSelector
  , quarterSymbolsSelector
  , rangeOfUnit_inUnit_forDateSelector
  , rangeOfUnit_startDate_interval_forDateSelector
  , rangeOfWeekendStartDate_interval_containingDateSelector
  , setFirstWeekdaySelector
  , setLocaleSelector
  , setMinimumDaysInFirstWeekSelector
  , setTimeZoneSelector
  , shortMonthSymbolsSelector
  , shortQuarterSymbolsSelector
  , shortStandaloneMonthSymbolsSelector
  , shortStandaloneQuarterSymbolsSelector
  , shortStandaloneWeekdaySymbolsSelector
  , shortWeekdaySymbolsSelector
  , standaloneMonthSymbolsSelector
  , standaloneQuarterSymbolsSelector
  , standaloneWeekdaySymbolsSelector
  , startOfDayForDateSelector
  , timeZoneSelector
  , veryShortMonthSymbolsSelector
  , veryShortStandaloneMonthSymbolsSelector
  , veryShortStandaloneWeekdaySymbolsSelector
  , veryShortWeekdaySymbolsSelector
  , weekdaySymbolsSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' calendarWithIdentifierSelector (toNSString calendarIdentifierConstant)

-- | @- init@
init_ :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSCalendar)
init_ nsCalendar =
  sendOwnedMessage nsCalendar initSelector

-- | @- initWithCalendarIdentifier:@
initWithCalendarIdentifier :: (IsNSCalendar nsCalendar, IsNSString ident) => nsCalendar -> ident -> IO RawId
initWithCalendarIdentifier nsCalendar ident =
  sendOwnedMessage nsCalendar initWithCalendarIdentifierSelector (toNSString ident)

-- | @- minimumRangeOfUnit:@
minimumRangeOfUnit :: IsNSCalendar nsCalendar => nsCalendar -> NSCalendarUnit -> IO NSRange
minimumRangeOfUnit nsCalendar unit =
  sendMessage nsCalendar minimumRangeOfUnitSelector unit

-- | @- maximumRangeOfUnit:@
maximumRangeOfUnit :: IsNSCalendar nsCalendar => nsCalendar -> NSCalendarUnit -> IO NSRange
maximumRangeOfUnit nsCalendar unit =
  sendMessage nsCalendar maximumRangeOfUnitSelector unit

-- | @- rangeOfUnit:inUnit:forDate:@
rangeOfUnit_inUnit_forDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> NSCalendarUnit -> date -> IO NSRange
rangeOfUnit_inUnit_forDate nsCalendar smaller larger date =
  sendMessage nsCalendar rangeOfUnit_inUnit_forDateSelector smaller larger (toNSDate date)

-- | @- ordinalityOfUnit:inUnit:forDate:@
ordinalityOfUnit_inUnit_forDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> NSCalendarUnit -> date -> IO CULong
ordinalityOfUnit_inUnit_forDate nsCalendar smaller larger date =
  sendMessage nsCalendar ordinalityOfUnit_inUnit_forDateSelector smaller larger (toNSDate date)

-- | @- rangeOfUnit:startDate:interval:forDate:@
rangeOfUnit_startDate_interval_forDate :: (IsNSCalendar nsCalendar, IsNSDate datep, IsNSDate date) => nsCalendar -> NSCalendarUnit -> datep -> Ptr CDouble -> date -> IO Bool
rangeOfUnit_startDate_interval_forDate nsCalendar unit datep tip date =
  sendMessage nsCalendar rangeOfUnit_startDate_interval_forDateSelector unit (toNSDate datep) tip (toNSDate date)

-- | @- dateFromComponents:@
dateFromComponents :: (IsNSCalendar nsCalendar, IsNSDateComponents comps) => nsCalendar -> comps -> IO (Id NSDate)
dateFromComponents nsCalendar comps =
  sendMessage nsCalendar dateFromComponentsSelector (toNSDateComponents comps)

-- | @- components:fromDate:@
components_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> date -> IO (Id NSDateComponents)
components_fromDate nsCalendar unitFlags date =
  sendMessage nsCalendar components_fromDateSelector unitFlags (toNSDate date)

-- | @- dateByAddingComponents:toDate:options:@
dateByAddingComponents_toDate_options :: (IsNSCalendar nsCalendar, IsNSDateComponents comps, IsNSDate date) => nsCalendar -> comps -> date -> NSCalendarOptions -> IO (Id NSDate)
dateByAddingComponents_toDate_options nsCalendar comps date opts =
  sendMessage nsCalendar dateByAddingComponents_toDate_optionsSelector (toNSDateComponents comps) (toNSDate date) opts

-- | @- components:fromDate:toDate:options:@
components_fromDate_toDate_options :: (IsNSCalendar nsCalendar, IsNSDate startingDate, IsNSDate resultDate) => nsCalendar -> NSCalendarUnit -> startingDate -> resultDate -> NSCalendarOptions -> IO (Id NSDateComponents)
components_fromDate_toDate_options nsCalendar unitFlags startingDate resultDate opts =
  sendMessage nsCalendar components_fromDate_toDate_optionsSelector unitFlags (toNSDate startingDate) (toNSDate resultDate) opts

-- | @- getEra:year:month:day:fromDate:@
getEra_year_month_day_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
getEra_year_month_day_fromDate nsCalendar eraValuePointer yearValuePointer monthValuePointer dayValuePointer date =
  sendMessage nsCalendar getEra_year_month_day_fromDateSelector eraValuePointer yearValuePointer monthValuePointer dayValuePointer (toNSDate date)

-- | @- getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:@
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDate nsCalendar eraValuePointer yearValuePointer weekValuePointer weekdayValuePointer date =
  sendMessage nsCalendar getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector eraValuePointer yearValuePointer weekValuePointer weekdayValuePointer (toNSDate date)

-- | @- getHour:minute:second:nanosecond:fromDate:@
getHour_minute_second_nanosecond_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> Ptr CLong -> Ptr CLong -> Ptr CLong -> Ptr CLong -> date -> IO ()
getHour_minute_second_nanosecond_fromDate nsCalendar hourValuePointer minuteValuePointer secondValuePointer nanosecondValuePointer date =
  sendMessage nsCalendar getHour_minute_second_nanosecond_fromDateSelector hourValuePointer minuteValuePointer secondValuePointer nanosecondValuePointer (toNSDate date)

-- | @- component:fromDate:@
component_fromDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> date -> IO CLong
component_fromDate nsCalendar unit date =
  sendMessage nsCalendar component_fromDateSelector unit (toNSDate date)

-- | @- dateWithEra:year:month:day:hour:minute:second:nanosecond:@
dateWithEra_year_month_day_hour_minute_second_nanosecond :: IsNSCalendar nsCalendar => nsCalendar -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id NSDate)
dateWithEra_year_month_day_hour_minute_second_nanosecond nsCalendar eraValue yearValue monthValue dayValue hourValue minuteValue secondValue nanosecondValue =
  sendMessage nsCalendar dateWithEra_year_month_day_hour_minute_second_nanosecondSelector eraValue yearValue monthValue dayValue hourValue minuteValue secondValue nanosecondValue

-- | @- dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:@
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecond :: IsNSCalendar nsCalendar => nsCalendar -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id NSDate)
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecond nsCalendar eraValue yearValue weekValue weekdayValue hourValue minuteValue secondValue nanosecondValue =
  sendMessage nsCalendar dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector eraValue yearValue weekValue weekdayValue hourValue minuteValue secondValue nanosecondValue

-- | @- startOfDayForDate:@
startOfDayForDate :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO (Id NSDate)
startOfDayForDate nsCalendar date =
  sendMessage nsCalendar startOfDayForDateSelector (toNSDate date)

-- | @- componentsInTimeZone:fromDate:@
componentsInTimeZone_fromDate :: (IsNSCalendar nsCalendar, IsNSTimeZone timezone, IsNSDate date) => nsCalendar -> timezone -> date -> IO (Id NSDateComponents)
componentsInTimeZone_fromDate nsCalendar timezone date =
  sendMessage nsCalendar componentsInTimeZone_fromDateSelector (toNSTimeZone timezone) (toNSDate date)

-- | @- compareDate:toDate:toUnitGranularity:@
compareDate_toDate_toUnitGranularity :: (IsNSCalendar nsCalendar, IsNSDate date1, IsNSDate date2) => nsCalendar -> date1 -> date2 -> NSCalendarUnit -> IO NSComparisonResult
compareDate_toDate_toUnitGranularity nsCalendar date1 date2 unit =
  sendMessage nsCalendar compareDate_toDate_toUnitGranularitySelector (toNSDate date1) (toNSDate date2) unit

-- | @- isDate:equalToDate:toUnitGranularity:@
isDate_equalToDate_toUnitGranularity :: (IsNSCalendar nsCalendar, IsNSDate date1, IsNSDate date2) => nsCalendar -> date1 -> date2 -> NSCalendarUnit -> IO Bool
isDate_equalToDate_toUnitGranularity nsCalendar date1 date2 unit =
  sendMessage nsCalendar isDate_equalToDate_toUnitGranularitySelector (toNSDate date1) (toNSDate date2) unit

-- | @- isDate:inSameDayAsDate:@
isDate_inSameDayAsDate :: (IsNSCalendar nsCalendar, IsNSDate date1, IsNSDate date2) => nsCalendar -> date1 -> date2 -> IO Bool
isDate_inSameDayAsDate nsCalendar date1 date2 =
  sendMessage nsCalendar isDate_inSameDayAsDateSelector (toNSDate date1) (toNSDate date2)

-- | @- isDateInToday:@
isDateInToday :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInToday nsCalendar date =
  sendMessage nsCalendar isDateInTodaySelector (toNSDate date)

-- | @- isDateInYesterday:@
isDateInYesterday :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInYesterday nsCalendar date =
  sendMessage nsCalendar isDateInYesterdaySelector (toNSDate date)

-- | @- isDateInTomorrow:@
isDateInTomorrow :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInTomorrow nsCalendar date =
  sendMessage nsCalendar isDateInTomorrowSelector (toNSDate date)

-- | @- isDateInWeekend:@
isDateInWeekend :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> IO Bool
isDateInWeekend nsCalendar date =
  sendMessage nsCalendar isDateInWeekendSelector (toNSDate date)

-- | @- rangeOfWeekendStartDate:interval:containingDate:@
rangeOfWeekendStartDate_interval_containingDate :: (IsNSCalendar nsCalendar, IsNSDate datep, IsNSDate date) => nsCalendar -> datep -> Ptr CDouble -> date -> IO Bool
rangeOfWeekendStartDate_interval_containingDate nsCalendar datep tip date =
  sendMessage nsCalendar rangeOfWeekendStartDate_interval_containingDateSelector (toNSDate datep) tip (toNSDate date)

-- | @- nextWeekendStartDate:interval:options:afterDate:@
nextWeekendStartDate_interval_options_afterDate :: (IsNSCalendar nsCalendar, IsNSDate datep, IsNSDate date) => nsCalendar -> datep -> Ptr CDouble -> NSCalendarOptions -> date -> IO Bool
nextWeekendStartDate_interval_options_afterDate nsCalendar datep tip options date =
  sendMessage nsCalendar nextWeekendStartDate_interval_options_afterDateSelector (toNSDate datep) tip options (toNSDate date)

-- | @- components:fromDateComponents:toDateComponents:options:@
components_fromDateComponents_toDateComponents_options :: (IsNSCalendar nsCalendar, IsNSDateComponents startingDateComp, IsNSDateComponents resultDateComp) => nsCalendar -> NSCalendarUnit -> startingDateComp -> resultDateComp -> NSCalendarOptions -> IO (Id NSDateComponents)
components_fromDateComponents_toDateComponents_options nsCalendar unitFlags startingDateComp resultDateComp options =
  sendMessage nsCalendar components_fromDateComponents_toDateComponents_optionsSelector unitFlags (toNSDateComponents startingDateComp) (toNSDateComponents resultDateComp) options

-- | @- dateByAddingUnit:value:toDate:options:@
dateByAddingUnit_value_toDate_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> CLong -> date -> NSCalendarOptions -> IO (Id NSDate)
dateByAddingUnit_value_toDate_options nsCalendar unit value date options =
  sendMessage nsCalendar dateByAddingUnit_value_toDate_optionsSelector unit value (toNSDate date) options

-- | @- enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:@
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlock :: (IsNSCalendar nsCalendar, IsNSDate start, IsNSDateComponents comps) => nsCalendar -> start -> comps -> NSCalendarOptions -> Ptr () -> IO ()
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlock nsCalendar start comps opts block =
  sendMessage nsCalendar enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector (toNSDate start) (toNSDateComponents comps) opts block

-- | @- nextDateAfterDate:matchingComponents:options:@
nextDateAfterDate_matchingComponents_options :: (IsNSCalendar nsCalendar, IsNSDate date, IsNSDateComponents comps) => nsCalendar -> date -> comps -> NSCalendarOptions -> IO (Id NSDate)
nextDateAfterDate_matchingComponents_options nsCalendar date comps options =
  sendMessage nsCalendar nextDateAfterDate_matchingComponents_optionsSelector (toNSDate date) (toNSDateComponents comps) options

-- | @- nextDateAfterDate:matchingUnit:value:options:@
nextDateAfterDate_matchingUnit_value_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> NSCalendarUnit -> CLong -> NSCalendarOptions -> IO (Id NSDate)
nextDateAfterDate_matchingUnit_value_options nsCalendar date unit value options =
  sendMessage nsCalendar nextDateAfterDate_matchingUnit_value_optionsSelector (toNSDate date) unit value options

-- | @- nextDateAfterDate:matchingHour:minute:second:options:@
nextDateAfterDate_matchingHour_minute_second_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> date -> CLong -> CLong -> CLong -> NSCalendarOptions -> IO (Id NSDate)
nextDateAfterDate_matchingHour_minute_second_options nsCalendar date hourValue minuteValue secondValue options =
  sendMessage nsCalendar nextDateAfterDate_matchingHour_minute_second_optionsSelector (toNSDate date) hourValue minuteValue secondValue options

-- | @- dateBySettingUnit:value:ofDate:options:@
dateBySettingUnit_value_ofDate_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> NSCalendarUnit -> CLong -> date -> NSCalendarOptions -> IO (Id NSDate)
dateBySettingUnit_value_ofDate_options nsCalendar unit v date opts =
  sendMessage nsCalendar dateBySettingUnit_value_ofDate_optionsSelector unit v (toNSDate date) opts

-- | @- dateBySettingHour:minute:second:ofDate:options:@
dateBySettingHour_minute_second_ofDate_options :: (IsNSCalendar nsCalendar, IsNSDate date) => nsCalendar -> CLong -> CLong -> CLong -> date -> NSCalendarOptions -> IO (Id NSDate)
dateBySettingHour_minute_second_ofDate_options nsCalendar h m s date opts =
  sendMessage nsCalendar dateBySettingHour_minute_second_ofDate_optionsSelector h m s (toNSDate date) opts

-- | @- date:matchesComponents:@
date_matchesComponents :: (IsNSCalendar nsCalendar, IsNSDate date, IsNSDateComponents components) => nsCalendar -> date -> components -> IO Bool
date_matchesComponents nsCalendar date components =
  sendMessage nsCalendar date_matchesComponentsSelector (toNSDate date) (toNSDateComponents components)

-- | @+ currentCalendar@
currentCalendar :: IO (Id NSCalendar)
currentCalendar  =
  do
    cls' <- getRequiredClass "NSCalendar"
    sendClassMessage cls' currentCalendarSelector

-- | @+ autoupdatingCurrentCalendar@
autoupdatingCurrentCalendar :: IO (Id NSCalendar)
autoupdatingCurrentCalendar  =
  do
    cls' <- getRequiredClass "NSCalendar"
    sendClassMessage cls' autoupdatingCurrentCalendarSelector

-- | @- calendarIdentifier@
calendarIdentifier :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSString)
calendarIdentifier nsCalendar =
  sendMessage nsCalendar calendarIdentifierSelector

-- | @- locale@
locale :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSLocale)
locale nsCalendar =
  sendMessage nsCalendar localeSelector

-- | @- setLocale:@
setLocale :: (IsNSCalendar nsCalendar, IsNSLocale value) => nsCalendar -> value -> IO ()
setLocale nsCalendar value =
  sendMessage nsCalendar setLocaleSelector (toNSLocale value)

-- | @- timeZone@
timeZone :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSTimeZone)
timeZone nsCalendar =
  sendMessage nsCalendar timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSCalendar nsCalendar, IsNSTimeZone value) => nsCalendar -> value -> IO ()
setTimeZone nsCalendar value =
  sendMessage nsCalendar setTimeZoneSelector (toNSTimeZone value)

-- | @- firstWeekday@
firstWeekday :: IsNSCalendar nsCalendar => nsCalendar -> IO CULong
firstWeekday nsCalendar =
  sendMessage nsCalendar firstWeekdaySelector

-- | @- setFirstWeekday:@
setFirstWeekday :: IsNSCalendar nsCalendar => nsCalendar -> CULong -> IO ()
setFirstWeekday nsCalendar value =
  sendMessage nsCalendar setFirstWeekdaySelector value

-- | @- minimumDaysInFirstWeek@
minimumDaysInFirstWeek :: IsNSCalendar nsCalendar => nsCalendar -> IO CULong
minimumDaysInFirstWeek nsCalendar =
  sendMessage nsCalendar minimumDaysInFirstWeekSelector

-- | @- setMinimumDaysInFirstWeek:@
setMinimumDaysInFirstWeek :: IsNSCalendar nsCalendar => nsCalendar -> CULong -> IO ()
setMinimumDaysInFirstWeek nsCalendar value =
  sendMessage nsCalendar setMinimumDaysInFirstWeekSelector value

-- | @- eraSymbols@
eraSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
eraSymbols nsCalendar =
  sendMessage nsCalendar eraSymbolsSelector

-- | @- longEraSymbols@
longEraSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
longEraSymbols nsCalendar =
  sendMessage nsCalendar longEraSymbolsSelector

-- | @- monthSymbols@
monthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
monthSymbols nsCalendar =
  sendMessage nsCalendar monthSymbolsSelector

-- | @- shortMonthSymbols@
shortMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortMonthSymbols nsCalendar =
  sendMessage nsCalendar shortMonthSymbolsSelector

-- | @- veryShortMonthSymbols@
veryShortMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortMonthSymbols nsCalendar =
  sendMessage nsCalendar veryShortMonthSymbolsSelector

-- | @- standaloneMonthSymbols@
standaloneMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
standaloneMonthSymbols nsCalendar =
  sendMessage nsCalendar standaloneMonthSymbolsSelector

-- | @- shortStandaloneMonthSymbols@
shortStandaloneMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortStandaloneMonthSymbols nsCalendar =
  sendMessage nsCalendar shortStandaloneMonthSymbolsSelector

-- | @- veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortStandaloneMonthSymbols nsCalendar =
  sendMessage nsCalendar veryShortStandaloneMonthSymbolsSelector

-- | @- weekdaySymbols@
weekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
weekdaySymbols nsCalendar =
  sendMessage nsCalendar weekdaySymbolsSelector

-- | @- shortWeekdaySymbols@
shortWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortWeekdaySymbols nsCalendar =
  sendMessage nsCalendar shortWeekdaySymbolsSelector

-- | @- veryShortWeekdaySymbols@
veryShortWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortWeekdaySymbols nsCalendar =
  sendMessage nsCalendar veryShortWeekdaySymbolsSelector

-- | @- standaloneWeekdaySymbols@
standaloneWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
standaloneWeekdaySymbols nsCalendar =
  sendMessage nsCalendar standaloneWeekdaySymbolsSelector

-- | @- shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortStandaloneWeekdaySymbols nsCalendar =
  sendMessage nsCalendar shortStandaloneWeekdaySymbolsSelector

-- | @- veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
veryShortStandaloneWeekdaySymbols nsCalendar =
  sendMessage nsCalendar veryShortStandaloneWeekdaySymbolsSelector

-- | @- quarterSymbols@
quarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
quarterSymbols nsCalendar =
  sendMessage nsCalendar quarterSymbolsSelector

-- | @- shortQuarterSymbols@
shortQuarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortQuarterSymbols nsCalendar =
  sendMessage nsCalendar shortQuarterSymbolsSelector

-- | @- standaloneQuarterSymbols@
standaloneQuarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
standaloneQuarterSymbols nsCalendar =
  sendMessage nsCalendar standaloneQuarterSymbolsSelector

-- | @- shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbols :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSArray)
shortStandaloneQuarterSymbols nsCalendar =
  sendMessage nsCalendar shortStandaloneQuarterSymbolsSelector

-- | @- AMSymbol@
amSymbol :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSString)
amSymbol nsCalendar =
  sendMessage nsCalendar amSymbolSelector

-- | @- PMSymbol@
pmSymbol :: IsNSCalendar nsCalendar => nsCalendar -> IO (Id NSString)
pmSymbol nsCalendar =
  sendMessage nsCalendar pmSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarWithIdentifier:@
calendarWithIdentifierSelector :: Selector '[Id NSString] (Id NSCalendar)
calendarWithIdentifierSelector = mkSelector "calendarWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCalendar)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCalendarIdentifier:@
initWithCalendarIdentifierSelector :: Selector '[Id NSString] RawId
initWithCalendarIdentifierSelector = mkSelector "initWithCalendarIdentifier:"

-- | @Selector@ for @minimumRangeOfUnit:@
minimumRangeOfUnitSelector :: Selector '[NSCalendarUnit] NSRange
minimumRangeOfUnitSelector = mkSelector "minimumRangeOfUnit:"

-- | @Selector@ for @maximumRangeOfUnit:@
maximumRangeOfUnitSelector :: Selector '[NSCalendarUnit] NSRange
maximumRangeOfUnitSelector = mkSelector "maximumRangeOfUnit:"

-- | @Selector@ for @rangeOfUnit:inUnit:forDate:@
rangeOfUnit_inUnit_forDateSelector :: Selector '[NSCalendarUnit, NSCalendarUnit, Id NSDate] NSRange
rangeOfUnit_inUnit_forDateSelector = mkSelector "rangeOfUnit:inUnit:forDate:"

-- | @Selector@ for @ordinalityOfUnit:inUnit:forDate:@
ordinalityOfUnit_inUnit_forDateSelector :: Selector '[NSCalendarUnit, NSCalendarUnit, Id NSDate] CULong
ordinalityOfUnit_inUnit_forDateSelector = mkSelector "ordinalityOfUnit:inUnit:forDate:"

-- | @Selector@ for @rangeOfUnit:startDate:interval:forDate:@
rangeOfUnit_startDate_interval_forDateSelector :: Selector '[NSCalendarUnit, Id NSDate, Ptr CDouble, Id NSDate] Bool
rangeOfUnit_startDate_interval_forDateSelector = mkSelector "rangeOfUnit:startDate:interval:forDate:"

-- | @Selector@ for @dateFromComponents:@
dateFromComponentsSelector :: Selector '[Id NSDateComponents] (Id NSDate)
dateFromComponentsSelector = mkSelector "dateFromComponents:"

-- | @Selector@ for @components:fromDate:@
components_fromDateSelector :: Selector '[NSCalendarUnit, Id NSDate] (Id NSDateComponents)
components_fromDateSelector = mkSelector "components:fromDate:"

-- | @Selector@ for @dateByAddingComponents:toDate:options:@
dateByAddingComponents_toDate_optionsSelector :: Selector '[Id NSDateComponents, Id NSDate, NSCalendarOptions] (Id NSDate)
dateByAddingComponents_toDate_optionsSelector = mkSelector "dateByAddingComponents:toDate:options:"

-- | @Selector@ for @components:fromDate:toDate:options:@
components_fromDate_toDate_optionsSelector :: Selector '[NSCalendarUnit, Id NSDate, Id NSDate, NSCalendarOptions] (Id NSDateComponents)
components_fromDate_toDate_optionsSelector = mkSelector "components:fromDate:toDate:options:"

-- | @Selector@ for @getEra:year:month:day:fromDate:@
getEra_year_month_day_fromDateSelector :: Selector '[Ptr CLong, Ptr CLong, Ptr CLong, Ptr CLong, Id NSDate] ()
getEra_year_month_day_fromDateSelector = mkSelector "getEra:year:month:day:fromDate:"

-- | @Selector@ for @getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:@
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector :: Selector '[Ptr CLong, Ptr CLong, Ptr CLong, Ptr CLong, Id NSDate] ()
getEra_yearForWeekOfYear_weekOfYear_weekday_fromDateSelector = mkSelector "getEra:yearForWeekOfYear:weekOfYear:weekday:fromDate:"

-- | @Selector@ for @getHour:minute:second:nanosecond:fromDate:@
getHour_minute_second_nanosecond_fromDateSelector :: Selector '[Ptr CLong, Ptr CLong, Ptr CLong, Ptr CLong, Id NSDate] ()
getHour_minute_second_nanosecond_fromDateSelector = mkSelector "getHour:minute:second:nanosecond:fromDate:"

-- | @Selector@ for @component:fromDate:@
component_fromDateSelector :: Selector '[NSCalendarUnit, Id NSDate] CLong
component_fromDateSelector = mkSelector "component:fromDate:"

-- | @Selector@ for @dateWithEra:year:month:day:hour:minute:second:nanosecond:@
dateWithEra_year_month_day_hour_minute_second_nanosecondSelector :: Selector '[CLong, CLong, CLong, CLong, CLong, CLong, CLong, CLong] (Id NSDate)
dateWithEra_year_month_day_hour_minute_second_nanosecondSelector = mkSelector "dateWithEra:year:month:day:hour:minute:second:nanosecond:"

-- | @Selector@ for @dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:@
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector :: Selector '[CLong, CLong, CLong, CLong, CLong, CLong, CLong, CLong] (Id NSDate)
dateWithEra_yearForWeekOfYear_weekOfYear_weekday_hour_minute_second_nanosecondSelector = mkSelector "dateWithEra:yearForWeekOfYear:weekOfYear:weekday:hour:minute:second:nanosecond:"

-- | @Selector@ for @startOfDayForDate:@
startOfDayForDateSelector :: Selector '[Id NSDate] (Id NSDate)
startOfDayForDateSelector = mkSelector "startOfDayForDate:"

-- | @Selector@ for @componentsInTimeZone:fromDate:@
componentsInTimeZone_fromDateSelector :: Selector '[Id NSTimeZone, Id NSDate] (Id NSDateComponents)
componentsInTimeZone_fromDateSelector = mkSelector "componentsInTimeZone:fromDate:"

-- | @Selector@ for @compareDate:toDate:toUnitGranularity:@
compareDate_toDate_toUnitGranularitySelector :: Selector '[Id NSDate, Id NSDate, NSCalendarUnit] NSComparisonResult
compareDate_toDate_toUnitGranularitySelector = mkSelector "compareDate:toDate:toUnitGranularity:"

-- | @Selector@ for @isDate:equalToDate:toUnitGranularity:@
isDate_equalToDate_toUnitGranularitySelector :: Selector '[Id NSDate, Id NSDate, NSCalendarUnit] Bool
isDate_equalToDate_toUnitGranularitySelector = mkSelector "isDate:equalToDate:toUnitGranularity:"

-- | @Selector@ for @isDate:inSameDayAsDate:@
isDate_inSameDayAsDateSelector :: Selector '[Id NSDate, Id NSDate] Bool
isDate_inSameDayAsDateSelector = mkSelector "isDate:inSameDayAsDate:"

-- | @Selector@ for @isDateInToday:@
isDateInTodaySelector :: Selector '[Id NSDate] Bool
isDateInTodaySelector = mkSelector "isDateInToday:"

-- | @Selector@ for @isDateInYesterday:@
isDateInYesterdaySelector :: Selector '[Id NSDate] Bool
isDateInYesterdaySelector = mkSelector "isDateInYesterday:"

-- | @Selector@ for @isDateInTomorrow:@
isDateInTomorrowSelector :: Selector '[Id NSDate] Bool
isDateInTomorrowSelector = mkSelector "isDateInTomorrow:"

-- | @Selector@ for @isDateInWeekend:@
isDateInWeekendSelector :: Selector '[Id NSDate] Bool
isDateInWeekendSelector = mkSelector "isDateInWeekend:"

-- | @Selector@ for @rangeOfWeekendStartDate:interval:containingDate:@
rangeOfWeekendStartDate_interval_containingDateSelector :: Selector '[Id NSDate, Ptr CDouble, Id NSDate] Bool
rangeOfWeekendStartDate_interval_containingDateSelector = mkSelector "rangeOfWeekendStartDate:interval:containingDate:"

-- | @Selector@ for @nextWeekendStartDate:interval:options:afterDate:@
nextWeekendStartDate_interval_options_afterDateSelector :: Selector '[Id NSDate, Ptr CDouble, NSCalendarOptions, Id NSDate] Bool
nextWeekendStartDate_interval_options_afterDateSelector = mkSelector "nextWeekendStartDate:interval:options:afterDate:"

-- | @Selector@ for @components:fromDateComponents:toDateComponents:options:@
components_fromDateComponents_toDateComponents_optionsSelector :: Selector '[NSCalendarUnit, Id NSDateComponents, Id NSDateComponents, NSCalendarOptions] (Id NSDateComponents)
components_fromDateComponents_toDateComponents_optionsSelector = mkSelector "components:fromDateComponents:toDateComponents:options:"

-- | @Selector@ for @dateByAddingUnit:value:toDate:options:@
dateByAddingUnit_value_toDate_optionsSelector :: Selector '[NSCalendarUnit, CLong, Id NSDate, NSCalendarOptions] (Id NSDate)
dateByAddingUnit_value_toDate_optionsSelector = mkSelector "dateByAddingUnit:value:toDate:options:"

-- | @Selector@ for @enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:@
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector :: Selector '[Id NSDate, Id NSDateComponents, NSCalendarOptions, Ptr ()] ()
enumerateDatesStartingAfterDate_matchingComponents_options_usingBlockSelector = mkSelector "enumerateDatesStartingAfterDate:matchingComponents:options:usingBlock:"

-- | @Selector@ for @nextDateAfterDate:matchingComponents:options:@
nextDateAfterDate_matchingComponents_optionsSelector :: Selector '[Id NSDate, Id NSDateComponents, NSCalendarOptions] (Id NSDate)
nextDateAfterDate_matchingComponents_optionsSelector = mkSelector "nextDateAfterDate:matchingComponents:options:"

-- | @Selector@ for @nextDateAfterDate:matchingUnit:value:options:@
nextDateAfterDate_matchingUnit_value_optionsSelector :: Selector '[Id NSDate, NSCalendarUnit, CLong, NSCalendarOptions] (Id NSDate)
nextDateAfterDate_matchingUnit_value_optionsSelector = mkSelector "nextDateAfterDate:matchingUnit:value:options:"

-- | @Selector@ for @nextDateAfterDate:matchingHour:minute:second:options:@
nextDateAfterDate_matchingHour_minute_second_optionsSelector :: Selector '[Id NSDate, CLong, CLong, CLong, NSCalendarOptions] (Id NSDate)
nextDateAfterDate_matchingHour_minute_second_optionsSelector = mkSelector "nextDateAfterDate:matchingHour:minute:second:options:"

-- | @Selector@ for @dateBySettingUnit:value:ofDate:options:@
dateBySettingUnit_value_ofDate_optionsSelector :: Selector '[NSCalendarUnit, CLong, Id NSDate, NSCalendarOptions] (Id NSDate)
dateBySettingUnit_value_ofDate_optionsSelector = mkSelector "dateBySettingUnit:value:ofDate:options:"

-- | @Selector@ for @dateBySettingHour:minute:second:ofDate:options:@
dateBySettingHour_minute_second_ofDate_optionsSelector :: Selector '[CLong, CLong, CLong, Id NSDate, NSCalendarOptions] (Id NSDate)
dateBySettingHour_minute_second_ofDate_optionsSelector = mkSelector "dateBySettingHour:minute:second:ofDate:options:"

-- | @Selector@ for @date:matchesComponents:@
date_matchesComponentsSelector :: Selector '[Id NSDate, Id NSDateComponents] Bool
date_matchesComponentsSelector = mkSelector "date:matchesComponents:"

-- | @Selector@ for @currentCalendar@
currentCalendarSelector :: Selector '[] (Id NSCalendar)
currentCalendarSelector = mkSelector "currentCalendar"

-- | @Selector@ for @autoupdatingCurrentCalendar@
autoupdatingCurrentCalendarSelector :: Selector '[] (Id NSCalendar)
autoupdatingCurrentCalendarSelector = mkSelector "autoupdatingCurrentCalendar"

-- | @Selector@ for @calendarIdentifier@
calendarIdentifierSelector :: Selector '[] (Id NSString)
calendarIdentifierSelector = mkSelector "calendarIdentifier"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @firstWeekday@
firstWeekdaySelector :: Selector '[] CULong
firstWeekdaySelector = mkSelector "firstWeekday"

-- | @Selector@ for @setFirstWeekday:@
setFirstWeekdaySelector :: Selector '[CULong] ()
setFirstWeekdaySelector = mkSelector "setFirstWeekday:"

-- | @Selector@ for @minimumDaysInFirstWeek@
minimumDaysInFirstWeekSelector :: Selector '[] CULong
minimumDaysInFirstWeekSelector = mkSelector "minimumDaysInFirstWeek"

-- | @Selector@ for @setMinimumDaysInFirstWeek:@
setMinimumDaysInFirstWeekSelector :: Selector '[CULong] ()
setMinimumDaysInFirstWeekSelector = mkSelector "setMinimumDaysInFirstWeek:"

-- | @Selector@ for @eraSymbols@
eraSymbolsSelector :: Selector '[] (Id NSArray)
eraSymbolsSelector = mkSelector "eraSymbols"

-- | @Selector@ for @longEraSymbols@
longEraSymbolsSelector :: Selector '[] (Id NSArray)
longEraSymbolsSelector = mkSelector "longEraSymbols"

-- | @Selector@ for @monthSymbols@
monthSymbolsSelector :: Selector '[] (Id NSArray)
monthSymbolsSelector = mkSelector "monthSymbols"

-- | @Selector@ for @shortMonthSymbols@
shortMonthSymbolsSelector :: Selector '[] (Id NSArray)
shortMonthSymbolsSelector = mkSelector "shortMonthSymbols"

-- | @Selector@ for @veryShortMonthSymbols@
veryShortMonthSymbolsSelector :: Selector '[] (Id NSArray)
veryShortMonthSymbolsSelector = mkSelector "veryShortMonthSymbols"

-- | @Selector@ for @standaloneMonthSymbols@
standaloneMonthSymbolsSelector :: Selector '[] (Id NSArray)
standaloneMonthSymbolsSelector = mkSelector "standaloneMonthSymbols"

-- | @Selector@ for @shortStandaloneMonthSymbols@
shortStandaloneMonthSymbolsSelector :: Selector '[] (Id NSArray)
shortStandaloneMonthSymbolsSelector = mkSelector "shortStandaloneMonthSymbols"

-- | @Selector@ for @veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbolsSelector :: Selector '[] (Id NSArray)
veryShortStandaloneMonthSymbolsSelector = mkSelector "veryShortStandaloneMonthSymbols"

-- | @Selector@ for @weekdaySymbols@
weekdaySymbolsSelector :: Selector '[] (Id NSArray)
weekdaySymbolsSelector = mkSelector "weekdaySymbols"

-- | @Selector@ for @shortWeekdaySymbols@
shortWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
shortWeekdaySymbolsSelector = mkSelector "shortWeekdaySymbols"

-- | @Selector@ for @veryShortWeekdaySymbols@
veryShortWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
veryShortWeekdaySymbolsSelector = mkSelector "veryShortWeekdaySymbols"

-- | @Selector@ for @standaloneWeekdaySymbols@
standaloneWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
standaloneWeekdaySymbolsSelector = mkSelector "standaloneWeekdaySymbols"

-- | @Selector@ for @shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
shortStandaloneWeekdaySymbolsSelector = mkSelector "shortStandaloneWeekdaySymbols"

-- | @Selector@ for @veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
veryShortStandaloneWeekdaySymbolsSelector = mkSelector "veryShortStandaloneWeekdaySymbols"

-- | @Selector@ for @quarterSymbols@
quarterSymbolsSelector :: Selector '[] (Id NSArray)
quarterSymbolsSelector = mkSelector "quarterSymbols"

-- | @Selector@ for @shortQuarterSymbols@
shortQuarterSymbolsSelector :: Selector '[] (Id NSArray)
shortQuarterSymbolsSelector = mkSelector "shortQuarterSymbols"

-- | @Selector@ for @standaloneQuarterSymbols@
standaloneQuarterSymbolsSelector :: Selector '[] (Id NSArray)
standaloneQuarterSymbolsSelector = mkSelector "standaloneQuarterSymbols"

-- | @Selector@ for @shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbolsSelector :: Selector '[] (Id NSArray)
shortStandaloneQuarterSymbolsSelector = mkSelector "shortStandaloneQuarterSymbols"

-- | @Selector@ for @AMSymbol@
amSymbolSelector :: Selector '[] (Id NSString)
amSymbolSelector = mkSelector "AMSymbol"

-- | @Selector@ for @PMSymbol@
pmSymbolSelector :: Selector '[] (Id NSString)
pmSymbolSelector = mkSelector "PMSymbol"

