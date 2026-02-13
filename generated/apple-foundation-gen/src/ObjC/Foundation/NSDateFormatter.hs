{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDateFormatter@.
module ObjC.Foundation.NSDateFormatter
  ( NSDateFormatter
  , IsNSDateFormatter(..)
  , getObjectValue_forString_range_error
  , stringFromDate
  , dateFromString
  , localizedStringFromDate_dateStyle_timeStyle
  , dateFormatFromTemplate_options_locale
  , setLocalizedDateFormatFromTemplate
  , initWithDateFormat_allowNaturalLanguage
  , allowsNaturalLanguage
  , formattingContext
  , setFormattingContext
  , defaultFormatterBehavior
  , setDefaultFormatterBehavior
  , dateFormat
  , setDateFormat
  , dateStyle
  , setDateStyle
  , timeStyle
  , setTimeStyle
  , locale
  , setLocale
  , generatesCalendarDates
  , setGeneratesCalendarDates
  , formatterBehavior
  , setFormatterBehavior
  , timeZone
  , setTimeZone
  , calendar
  , setCalendar
  , lenient
  , setLenient
  , twoDigitStartDate
  , setTwoDigitStartDate
  , defaultDate
  , setDefaultDate
  , eraSymbols
  , setEraSymbols
  , monthSymbols
  , setMonthSymbols
  , shortMonthSymbols
  , setShortMonthSymbols
  , weekdaySymbols
  , setWeekdaySymbols
  , shortWeekdaySymbols
  , setShortWeekdaySymbols
  , amSymbol
  , setAMSymbol
  , pmSymbol
  , setPMSymbol
  , longEraSymbols
  , setLongEraSymbols
  , veryShortMonthSymbols
  , setVeryShortMonthSymbols
  , standaloneMonthSymbols
  , setStandaloneMonthSymbols
  , shortStandaloneMonthSymbols
  , setShortStandaloneMonthSymbols
  , veryShortStandaloneMonthSymbols
  , setVeryShortStandaloneMonthSymbols
  , veryShortWeekdaySymbols
  , setVeryShortWeekdaySymbols
  , standaloneWeekdaySymbols
  , setStandaloneWeekdaySymbols
  , shortStandaloneWeekdaySymbols
  , setShortStandaloneWeekdaySymbols
  , veryShortStandaloneWeekdaySymbols
  , setVeryShortStandaloneWeekdaySymbols
  , quarterSymbols
  , setQuarterSymbols
  , shortQuarterSymbols
  , setShortQuarterSymbols
  , standaloneQuarterSymbols
  , setStandaloneQuarterSymbols
  , shortStandaloneQuarterSymbols
  , setShortStandaloneQuarterSymbols
  , gregorianStartDate
  , setGregorianStartDate
  , doesRelativeDateFormatting
  , setDoesRelativeDateFormatting
  , allowsNaturalLanguageSelector
  , amSymbolSelector
  , calendarSelector
  , dateFormatFromTemplate_options_localeSelector
  , dateFormatSelector
  , dateFromStringSelector
  , dateStyleSelector
  , defaultDateSelector
  , defaultFormatterBehaviorSelector
  , doesRelativeDateFormattingSelector
  , eraSymbolsSelector
  , formatterBehaviorSelector
  , formattingContextSelector
  , generatesCalendarDatesSelector
  , getObjectValue_forString_range_errorSelector
  , gregorianStartDateSelector
  , initWithDateFormat_allowNaturalLanguageSelector
  , lenientSelector
  , localeSelector
  , localizedStringFromDate_dateStyle_timeStyleSelector
  , longEraSymbolsSelector
  , monthSymbolsSelector
  , pmSymbolSelector
  , quarterSymbolsSelector
  , setAMSymbolSelector
  , setCalendarSelector
  , setDateFormatSelector
  , setDateStyleSelector
  , setDefaultDateSelector
  , setDefaultFormatterBehaviorSelector
  , setDoesRelativeDateFormattingSelector
  , setEraSymbolsSelector
  , setFormatterBehaviorSelector
  , setFormattingContextSelector
  , setGeneratesCalendarDatesSelector
  , setGregorianStartDateSelector
  , setLenientSelector
  , setLocaleSelector
  , setLocalizedDateFormatFromTemplateSelector
  , setLongEraSymbolsSelector
  , setMonthSymbolsSelector
  , setPMSymbolSelector
  , setQuarterSymbolsSelector
  , setShortMonthSymbolsSelector
  , setShortQuarterSymbolsSelector
  , setShortStandaloneMonthSymbolsSelector
  , setShortStandaloneQuarterSymbolsSelector
  , setShortStandaloneWeekdaySymbolsSelector
  , setShortWeekdaySymbolsSelector
  , setStandaloneMonthSymbolsSelector
  , setStandaloneQuarterSymbolsSelector
  , setStandaloneWeekdaySymbolsSelector
  , setTimeStyleSelector
  , setTimeZoneSelector
  , setTwoDigitStartDateSelector
  , setVeryShortMonthSymbolsSelector
  , setVeryShortStandaloneMonthSymbolsSelector
  , setVeryShortStandaloneWeekdaySymbolsSelector
  , setVeryShortWeekdaySymbolsSelector
  , setWeekdaySymbolsSelector
  , shortMonthSymbolsSelector
  , shortQuarterSymbolsSelector
  , shortStandaloneMonthSymbolsSelector
  , shortStandaloneQuarterSymbolsSelector
  , shortStandaloneWeekdaySymbolsSelector
  , shortWeekdaySymbolsSelector
  , standaloneMonthSymbolsSelector
  , standaloneQuarterSymbolsSelector
  , standaloneWeekdaySymbolsSelector
  , stringFromDateSelector
  , timeStyleSelector
  , timeZoneSelector
  , twoDigitStartDateSelector
  , veryShortMonthSymbolsSelector
  , veryShortStandaloneMonthSymbolsSelector
  , veryShortStandaloneWeekdaySymbolsSelector
  , veryShortWeekdaySymbolsSelector
  , weekdaySymbolsSelector

  -- * Enum types
  , NSDateFormatterBehavior(NSDateFormatterBehavior)
  , pattern NSDateFormatterBehaviorDefault
  , pattern NSDateFormatterBehavior10_0
  , pattern NSDateFormatterBehavior10_4
  , NSDateFormatterStyle(NSDateFormatterStyle)
  , pattern NSDateFormatterNoStyle
  , pattern NSDateFormatterShortStyle
  , pattern NSDateFormatterMediumStyle
  , pattern NSDateFormatterLongStyle
  , pattern NSDateFormatterFullStyle
  , NSFormattingContext(NSFormattingContext)
  , pattern NSFormattingContextUnknown
  , pattern NSFormattingContextDynamic
  , pattern NSFormattingContextStandalone
  , pattern NSFormattingContextListItem
  , pattern NSFormattingContextBeginningOfSentence
  , pattern NSFormattingContextMiddleOfSentence

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

-- | @- getObjectValue:forString:range:error:@
getObjectValue_forString_range_error :: (IsNSDateFormatter nsDateFormatter, IsNSString string, IsNSError error_) => nsDateFormatter -> Ptr RawId -> string -> Ptr NSRange -> error_ -> IO Bool
getObjectValue_forString_range_error nsDateFormatter obj_ string rangep error_ =
  sendMessage nsDateFormatter getObjectValue_forString_range_errorSelector obj_ (toNSString string) rangep (toNSError error_)

-- | @- stringFromDate:@
stringFromDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate date) => nsDateFormatter -> date -> IO (Id NSString)
stringFromDate nsDateFormatter date =
  sendMessage nsDateFormatter stringFromDateSelector (toNSDate date)

-- | @- dateFromString:@
dateFromString :: (IsNSDateFormatter nsDateFormatter, IsNSString string) => nsDateFormatter -> string -> IO (Id NSDate)
dateFromString nsDateFormatter string =
  sendMessage nsDateFormatter dateFromStringSelector (toNSString string)

-- | @+ localizedStringFromDate:dateStyle:timeStyle:@
localizedStringFromDate_dateStyle_timeStyle :: IsNSDate date => date -> NSDateFormatterStyle -> NSDateFormatterStyle -> IO (Id NSString)
localizedStringFromDate_dateStyle_timeStyle date dstyle tstyle =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    sendClassMessage cls' localizedStringFromDate_dateStyle_timeStyleSelector (toNSDate date) dstyle tstyle

-- | @+ dateFormatFromTemplate:options:locale:@
dateFormatFromTemplate_options_locale :: (IsNSString tmplate, IsNSLocale locale) => tmplate -> CULong -> locale -> IO (Id NSString)
dateFormatFromTemplate_options_locale tmplate opts locale =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    sendClassMessage cls' dateFormatFromTemplate_options_localeSelector (toNSString tmplate) opts (toNSLocale locale)

-- | @- setLocalizedDateFormatFromTemplate:@
setLocalizedDateFormatFromTemplate :: (IsNSDateFormatter nsDateFormatter, IsNSString dateFormatTemplate) => nsDateFormatter -> dateFormatTemplate -> IO ()
setLocalizedDateFormatFromTemplate nsDateFormatter dateFormatTemplate =
  sendMessage nsDateFormatter setLocalizedDateFormatFromTemplateSelector (toNSString dateFormatTemplate)

-- | @- initWithDateFormat:allowNaturalLanguage:@
initWithDateFormat_allowNaturalLanguage :: (IsNSDateFormatter nsDateFormatter, IsNSString format) => nsDateFormatter -> format -> Bool -> IO RawId
initWithDateFormat_allowNaturalLanguage nsDateFormatter format flag =
  sendOwnedMessage nsDateFormatter initWithDateFormat_allowNaturalLanguageSelector (toNSString format) flag

-- | @- allowsNaturalLanguage@
allowsNaturalLanguage :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
allowsNaturalLanguage nsDateFormatter =
  sendMessage nsDateFormatter allowsNaturalLanguageSelector

-- | @- formattingContext@
formattingContext :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSFormattingContext
formattingContext nsDateFormatter =
  sendMessage nsDateFormatter formattingContextSelector

-- | @- setFormattingContext:@
setFormattingContext :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsDateFormatter value =
  sendMessage nsDateFormatter setFormattingContextSelector value

-- | @+ defaultFormatterBehavior@
defaultFormatterBehavior :: IO NSDateFormatterBehavior
defaultFormatterBehavior  =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    sendClassMessage cls' defaultFormatterBehaviorSelector

-- | @+ setDefaultFormatterBehavior:@
setDefaultFormatterBehavior :: NSDateFormatterBehavior -> IO ()
setDefaultFormatterBehavior value =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    sendClassMessage cls' setDefaultFormatterBehaviorSelector value

-- | @- dateFormat@
dateFormat :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSString)
dateFormat nsDateFormatter =
  sendMessage nsDateFormatter dateFormatSelector

-- | @- setDateFormat:@
setDateFormat :: (IsNSDateFormatter nsDateFormatter, IsNSString value) => nsDateFormatter -> value -> IO ()
setDateFormat nsDateFormatter value =
  sendMessage nsDateFormatter setDateFormatSelector (toNSString value)

-- | @- dateStyle@
dateStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSDateFormatterStyle
dateStyle nsDateFormatter =
  sendMessage nsDateFormatter dateStyleSelector

-- | @- setDateStyle:@
setDateStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSDateFormatterStyle -> IO ()
setDateStyle nsDateFormatter value =
  sendMessage nsDateFormatter setDateStyleSelector value

-- | @- timeStyle@
timeStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSDateFormatterStyle
timeStyle nsDateFormatter =
  sendMessage nsDateFormatter timeStyleSelector

-- | @- setTimeStyle:@
setTimeStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSDateFormatterStyle -> IO ()
setTimeStyle nsDateFormatter value =
  sendMessage nsDateFormatter setTimeStyleSelector value

-- | @- locale@
locale :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSLocale)
locale nsDateFormatter =
  sendMessage nsDateFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSDateFormatter nsDateFormatter, IsNSLocale value) => nsDateFormatter -> value -> IO ()
setLocale nsDateFormatter value =
  sendMessage nsDateFormatter setLocaleSelector (toNSLocale value)

-- | @- generatesCalendarDates@
generatesCalendarDates :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
generatesCalendarDates nsDateFormatter =
  sendMessage nsDateFormatter generatesCalendarDatesSelector

-- | @- setGeneratesCalendarDates:@
setGeneratesCalendarDates :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> Bool -> IO ()
setGeneratesCalendarDates nsDateFormatter value =
  sendMessage nsDateFormatter setGeneratesCalendarDatesSelector value

-- | @- formatterBehavior@
formatterBehavior :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSDateFormatterBehavior
formatterBehavior nsDateFormatter =
  sendMessage nsDateFormatter formatterBehaviorSelector

-- | @- setFormatterBehavior:@
setFormatterBehavior :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSDateFormatterBehavior -> IO ()
setFormatterBehavior nsDateFormatter value =
  sendMessage nsDateFormatter setFormatterBehaviorSelector value

-- | @- timeZone@
timeZone :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSTimeZone)
timeZone nsDateFormatter =
  sendMessage nsDateFormatter timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSDateFormatter nsDateFormatter, IsNSTimeZone value) => nsDateFormatter -> value -> IO ()
setTimeZone nsDateFormatter value =
  sendMessage nsDateFormatter setTimeZoneSelector (toNSTimeZone value)

-- | @- calendar@
calendar :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSCalendar)
calendar nsDateFormatter =
  sendMessage nsDateFormatter calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSDateFormatter nsDateFormatter, IsNSCalendar value) => nsDateFormatter -> value -> IO ()
setCalendar nsDateFormatter value =
  sendMessage nsDateFormatter setCalendarSelector (toNSCalendar value)

-- | @- lenient@
lenient :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
lenient nsDateFormatter =
  sendMessage nsDateFormatter lenientSelector

-- | @- setLenient:@
setLenient :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> Bool -> IO ()
setLenient nsDateFormatter value =
  sendMessage nsDateFormatter setLenientSelector value

-- | @- twoDigitStartDate@
twoDigitStartDate :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSDate)
twoDigitStartDate nsDateFormatter =
  sendMessage nsDateFormatter twoDigitStartDateSelector

-- | @- setTwoDigitStartDate:@
setTwoDigitStartDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate value) => nsDateFormatter -> value -> IO ()
setTwoDigitStartDate nsDateFormatter value =
  sendMessage nsDateFormatter setTwoDigitStartDateSelector (toNSDate value)

-- | @- defaultDate@
defaultDate :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSDate)
defaultDate nsDateFormatter =
  sendMessage nsDateFormatter defaultDateSelector

-- | @- setDefaultDate:@
setDefaultDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate value) => nsDateFormatter -> value -> IO ()
setDefaultDate nsDateFormatter value =
  sendMessage nsDateFormatter setDefaultDateSelector (toNSDate value)

-- | @- eraSymbols@
eraSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
eraSymbols nsDateFormatter =
  sendMessage nsDateFormatter eraSymbolsSelector

-- | @- setEraSymbols:@
setEraSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setEraSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setEraSymbolsSelector (toNSArray value)

-- | @- monthSymbols@
monthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
monthSymbols nsDateFormatter =
  sendMessage nsDateFormatter monthSymbolsSelector

-- | @- setMonthSymbols:@
setMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setMonthSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setMonthSymbolsSelector (toNSArray value)

-- | @- shortMonthSymbols@
shortMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortMonthSymbols nsDateFormatter =
  sendMessage nsDateFormatter shortMonthSymbolsSelector

-- | @- setShortMonthSymbols:@
setShortMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortMonthSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setShortMonthSymbolsSelector (toNSArray value)

-- | @- weekdaySymbols@
weekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
weekdaySymbols nsDateFormatter =
  sendMessage nsDateFormatter weekdaySymbolsSelector

-- | @- setWeekdaySymbols:@
setWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setWeekdaySymbols nsDateFormatter value =
  sendMessage nsDateFormatter setWeekdaySymbolsSelector (toNSArray value)

-- | @- shortWeekdaySymbols@
shortWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortWeekdaySymbols nsDateFormatter =
  sendMessage nsDateFormatter shortWeekdaySymbolsSelector

-- | @- setShortWeekdaySymbols:@
setShortWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortWeekdaySymbols nsDateFormatter value =
  sendMessage nsDateFormatter setShortWeekdaySymbolsSelector (toNSArray value)

-- | @- AMSymbol@
amSymbol :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSString)
amSymbol nsDateFormatter =
  sendMessage nsDateFormatter amSymbolSelector

-- | @- setAMSymbol:@
setAMSymbol :: (IsNSDateFormatter nsDateFormatter, IsNSString value) => nsDateFormatter -> value -> IO ()
setAMSymbol nsDateFormatter value =
  sendMessage nsDateFormatter setAMSymbolSelector (toNSString value)

-- | @- PMSymbol@
pmSymbol :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSString)
pmSymbol nsDateFormatter =
  sendMessage nsDateFormatter pmSymbolSelector

-- | @- setPMSymbol:@
setPMSymbol :: (IsNSDateFormatter nsDateFormatter, IsNSString value) => nsDateFormatter -> value -> IO ()
setPMSymbol nsDateFormatter value =
  sendMessage nsDateFormatter setPMSymbolSelector (toNSString value)

-- | @- longEraSymbols@
longEraSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
longEraSymbols nsDateFormatter =
  sendMessage nsDateFormatter longEraSymbolsSelector

-- | @- setLongEraSymbols:@
setLongEraSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setLongEraSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setLongEraSymbolsSelector (toNSArray value)

-- | @- veryShortMonthSymbols@
veryShortMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortMonthSymbols nsDateFormatter =
  sendMessage nsDateFormatter veryShortMonthSymbolsSelector

-- | @- setVeryShortMonthSymbols:@
setVeryShortMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortMonthSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setVeryShortMonthSymbolsSelector (toNSArray value)

-- | @- standaloneMonthSymbols@
standaloneMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
standaloneMonthSymbols nsDateFormatter =
  sendMessage nsDateFormatter standaloneMonthSymbolsSelector

-- | @- setStandaloneMonthSymbols:@
setStandaloneMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setStandaloneMonthSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setStandaloneMonthSymbolsSelector (toNSArray value)

-- | @- shortStandaloneMonthSymbols@
shortStandaloneMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortStandaloneMonthSymbols nsDateFormatter =
  sendMessage nsDateFormatter shortStandaloneMonthSymbolsSelector

-- | @- setShortStandaloneMonthSymbols:@
setShortStandaloneMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortStandaloneMonthSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setShortStandaloneMonthSymbolsSelector (toNSArray value)

-- | @- veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortStandaloneMonthSymbols nsDateFormatter =
  sendMessage nsDateFormatter veryShortStandaloneMonthSymbolsSelector

-- | @- setVeryShortStandaloneMonthSymbols:@
setVeryShortStandaloneMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortStandaloneMonthSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setVeryShortStandaloneMonthSymbolsSelector (toNSArray value)

-- | @- veryShortWeekdaySymbols@
veryShortWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortWeekdaySymbols nsDateFormatter =
  sendMessage nsDateFormatter veryShortWeekdaySymbolsSelector

-- | @- setVeryShortWeekdaySymbols:@
setVeryShortWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortWeekdaySymbols nsDateFormatter value =
  sendMessage nsDateFormatter setVeryShortWeekdaySymbolsSelector (toNSArray value)

-- | @- standaloneWeekdaySymbols@
standaloneWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
standaloneWeekdaySymbols nsDateFormatter =
  sendMessage nsDateFormatter standaloneWeekdaySymbolsSelector

-- | @- setStandaloneWeekdaySymbols:@
setStandaloneWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setStandaloneWeekdaySymbols nsDateFormatter value =
  sendMessage nsDateFormatter setStandaloneWeekdaySymbolsSelector (toNSArray value)

-- | @- shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortStandaloneWeekdaySymbols nsDateFormatter =
  sendMessage nsDateFormatter shortStandaloneWeekdaySymbolsSelector

-- | @- setShortStandaloneWeekdaySymbols:@
setShortStandaloneWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortStandaloneWeekdaySymbols nsDateFormatter value =
  sendMessage nsDateFormatter setShortStandaloneWeekdaySymbolsSelector (toNSArray value)

-- | @- veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortStandaloneWeekdaySymbols nsDateFormatter =
  sendMessage nsDateFormatter veryShortStandaloneWeekdaySymbolsSelector

-- | @- setVeryShortStandaloneWeekdaySymbols:@
setVeryShortStandaloneWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortStandaloneWeekdaySymbols nsDateFormatter value =
  sendMessage nsDateFormatter setVeryShortStandaloneWeekdaySymbolsSelector (toNSArray value)

-- | @- quarterSymbols@
quarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
quarterSymbols nsDateFormatter =
  sendMessage nsDateFormatter quarterSymbolsSelector

-- | @- setQuarterSymbols:@
setQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setQuarterSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setQuarterSymbolsSelector (toNSArray value)

-- | @- shortQuarterSymbols@
shortQuarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortQuarterSymbols nsDateFormatter =
  sendMessage nsDateFormatter shortQuarterSymbolsSelector

-- | @- setShortQuarterSymbols:@
setShortQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortQuarterSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setShortQuarterSymbolsSelector (toNSArray value)

-- | @- standaloneQuarterSymbols@
standaloneQuarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
standaloneQuarterSymbols nsDateFormatter =
  sendMessage nsDateFormatter standaloneQuarterSymbolsSelector

-- | @- setStandaloneQuarterSymbols:@
setStandaloneQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setStandaloneQuarterSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setStandaloneQuarterSymbolsSelector (toNSArray value)

-- | @- shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortStandaloneQuarterSymbols nsDateFormatter =
  sendMessage nsDateFormatter shortStandaloneQuarterSymbolsSelector

-- | @- setShortStandaloneQuarterSymbols:@
setShortStandaloneQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortStandaloneQuarterSymbols nsDateFormatter value =
  sendMessage nsDateFormatter setShortStandaloneQuarterSymbolsSelector (toNSArray value)

-- | @- gregorianStartDate@
gregorianStartDate :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSDate)
gregorianStartDate nsDateFormatter =
  sendMessage nsDateFormatter gregorianStartDateSelector

-- | @- setGregorianStartDate:@
setGregorianStartDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate value) => nsDateFormatter -> value -> IO ()
setGregorianStartDate nsDateFormatter value =
  sendMessage nsDateFormatter setGregorianStartDateSelector (toNSDate value)

-- | @- doesRelativeDateFormatting@
doesRelativeDateFormatting :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
doesRelativeDateFormatting nsDateFormatter =
  sendMessage nsDateFormatter doesRelativeDateFormattingSelector

-- | @- setDoesRelativeDateFormatting:@
setDoesRelativeDateFormatting :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> Bool -> IO ()
setDoesRelativeDateFormatting nsDateFormatter value =
  sendMessage nsDateFormatter setDoesRelativeDateFormattingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getObjectValue:forString:range:error:@
getObjectValue_forString_range_errorSelector :: Selector '[Ptr RawId, Id NSString, Ptr NSRange, Id NSError] Bool
getObjectValue_forString_range_errorSelector = mkSelector "getObjectValue:forString:range:error:"

-- | @Selector@ for @stringFromDate:@
stringFromDateSelector :: Selector '[Id NSDate] (Id NSString)
stringFromDateSelector = mkSelector "stringFromDate:"

-- | @Selector@ for @dateFromString:@
dateFromStringSelector :: Selector '[Id NSString] (Id NSDate)
dateFromStringSelector = mkSelector "dateFromString:"

-- | @Selector@ for @localizedStringFromDate:dateStyle:timeStyle:@
localizedStringFromDate_dateStyle_timeStyleSelector :: Selector '[Id NSDate, NSDateFormatterStyle, NSDateFormatterStyle] (Id NSString)
localizedStringFromDate_dateStyle_timeStyleSelector = mkSelector "localizedStringFromDate:dateStyle:timeStyle:"

-- | @Selector@ for @dateFormatFromTemplate:options:locale:@
dateFormatFromTemplate_options_localeSelector :: Selector '[Id NSString, CULong, Id NSLocale] (Id NSString)
dateFormatFromTemplate_options_localeSelector = mkSelector "dateFormatFromTemplate:options:locale:"

-- | @Selector@ for @setLocalizedDateFormatFromTemplate:@
setLocalizedDateFormatFromTemplateSelector :: Selector '[Id NSString] ()
setLocalizedDateFormatFromTemplateSelector = mkSelector "setLocalizedDateFormatFromTemplate:"

-- | @Selector@ for @initWithDateFormat:allowNaturalLanguage:@
initWithDateFormat_allowNaturalLanguageSelector :: Selector '[Id NSString, Bool] RawId
initWithDateFormat_allowNaturalLanguageSelector = mkSelector "initWithDateFormat:allowNaturalLanguage:"

-- | @Selector@ for @allowsNaturalLanguage@
allowsNaturalLanguageSelector :: Selector '[] Bool
allowsNaturalLanguageSelector = mkSelector "allowsNaturalLanguage"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector '[] NSFormattingContext
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector '[NSFormattingContext] ()
setFormattingContextSelector = mkSelector "setFormattingContext:"

-- | @Selector@ for @defaultFormatterBehavior@
defaultFormatterBehaviorSelector :: Selector '[] NSDateFormatterBehavior
defaultFormatterBehaviorSelector = mkSelector "defaultFormatterBehavior"

-- | @Selector@ for @setDefaultFormatterBehavior:@
setDefaultFormatterBehaviorSelector :: Selector '[NSDateFormatterBehavior] ()
setDefaultFormatterBehaviorSelector = mkSelector "setDefaultFormatterBehavior:"

-- | @Selector@ for @dateFormat@
dateFormatSelector :: Selector '[] (Id NSString)
dateFormatSelector = mkSelector "dateFormat"

-- | @Selector@ for @setDateFormat:@
setDateFormatSelector :: Selector '[Id NSString] ()
setDateFormatSelector = mkSelector "setDateFormat:"

-- | @Selector@ for @dateStyle@
dateStyleSelector :: Selector '[] NSDateFormatterStyle
dateStyleSelector = mkSelector "dateStyle"

-- | @Selector@ for @setDateStyle:@
setDateStyleSelector :: Selector '[NSDateFormatterStyle] ()
setDateStyleSelector = mkSelector "setDateStyle:"

-- | @Selector@ for @timeStyle@
timeStyleSelector :: Selector '[] NSDateFormatterStyle
timeStyleSelector = mkSelector "timeStyle"

-- | @Selector@ for @setTimeStyle:@
setTimeStyleSelector :: Selector '[NSDateFormatterStyle] ()
setTimeStyleSelector = mkSelector "setTimeStyle:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @generatesCalendarDates@
generatesCalendarDatesSelector :: Selector '[] Bool
generatesCalendarDatesSelector = mkSelector "generatesCalendarDates"

-- | @Selector@ for @setGeneratesCalendarDates:@
setGeneratesCalendarDatesSelector :: Selector '[Bool] ()
setGeneratesCalendarDatesSelector = mkSelector "setGeneratesCalendarDates:"

-- | @Selector@ for @formatterBehavior@
formatterBehaviorSelector :: Selector '[] NSDateFormatterBehavior
formatterBehaviorSelector = mkSelector "formatterBehavior"

-- | @Selector@ for @setFormatterBehavior:@
setFormatterBehaviorSelector :: Selector '[NSDateFormatterBehavior] ()
setFormatterBehaviorSelector = mkSelector "setFormatterBehavior:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id NSCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id NSCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @lenient@
lenientSelector :: Selector '[] Bool
lenientSelector = mkSelector "lenient"

-- | @Selector@ for @setLenient:@
setLenientSelector :: Selector '[Bool] ()
setLenientSelector = mkSelector "setLenient:"

-- | @Selector@ for @twoDigitStartDate@
twoDigitStartDateSelector :: Selector '[] (Id NSDate)
twoDigitStartDateSelector = mkSelector "twoDigitStartDate"

-- | @Selector@ for @setTwoDigitStartDate:@
setTwoDigitStartDateSelector :: Selector '[Id NSDate] ()
setTwoDigitStartDateSelector = mkSelector "setTwoDigitStartDate:"

-- | @Selector@ for @defaultDate@
defaultDateSelector :: Selector '[] (Id NSDate)
defaultDateSelector = mkSelector "defaultDate"

-- | @Selector@ for @setDefaultDate:@
setDefaultDateSelector :: Selector '[Id NSDate] ()
setDefaultDateSelector = mkSelector "setDefaultDate:"

-- | @Selector@ for @eraSymbols@
eraSymbolsSelector :: Selector '[] (Id NSArray)
eraSymbolsSelector = mkSelector "eraSymbols"

-- | @Selector@ for @setEraSymbols:@
setEraSymbolsSelector :: Selector '[Id NSArray] ()
setEraSymbolsSelector = mkSelector "setEraSymbols:"

-- | @Selector@ for @monthSymbols@
monthSymbolsSelector :: Selector '[] (Id NSArray)
monthSymbolsSelector = mkSelector "monthSymbols"

-- | @Selector@ for @setMonthSymbols:@
setMonthSymbolsSelector :: Selector '[Id NSArray] ()
setMonthSymbolsSelector = mkSelector "setMonthSymbols:"

-- | @Selector@ for @shortMonthSymbols@
shortMonthSymbolsSelector :: Selector '[] (Id NSArray)
shortMonthSymbolsSelector = mkSelector "shortMonthSymbols"

-- | @Selector@ for @setShortMonthSymbols:@
setShortMonthSymbolsSelector :: Selector '[Id NSArray] ()
setShortMonthSymbolsSelector = mkSelector "setShortMonthSymbols:"

-- | @Selector@ for @weekdaySymbols@
weekdaySymbolsSelector :: Selector '[] (Id NSArray)
weekdaySymbolsSelector = mkSelector "weekdaySymbols"

-- | @Selector@ for @setWeekdaySymbols:@
setWeekdaySymbolsSelector :: Selector '[Id NSArray] ()
setWeekdaySymbolsSelector = mkSelector "setWeekdaySymbols:"

-- | @Selector@ for @shortWeekdaySymbols@
shortWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
shortWeekdaySymbolsSelector = mkSelector "shortWeekdaySymbols"

-- | @Selector@ for @setShortWeekdaySymbols:@
setShortWeekdaySymbolsSelector :: Selector '[Id NSArray] ()
setShortWeekdaySymbolsSelector = mkSelector "setShortWeekdaySymbols:"

-- | @Selector@ for @AMSymbol@
amSymbolSelector :: Selector '[] (Id NSString)
amSymbolSelector = mkSelector "AMSymbol"

-- | @Selector@ for @setAMSymbol:@
setAMSymbolSelector :: Selector '[Id NSString] ()
setAMSymbolSelector = mkSelector "setAMSymbol:"

-- | @Selector@ for @PMSymbol@
pmSymbolSelector :: Selector '[] (Id NSString)
pmSymbolSelector = mkSelector "PMSymbol"

-- | @Selector@ for @setPMSymbol:@
setPMSymbolSelector :: Selector '[Id NSString] ()
setPMSymbolSelector = mkSelector "setPMSymbol:"

-- | @Selector@ for @longEraSymbols@
longEraSymbolsSelector :: Selector '[] (Id NSArray)
longEraSymbolsSelector = mkSelector "longEraSymbols"

-- | @Selector@ for @setLongEraSymbols:@
setLongEraSymbolsSelector :: Selector '[Id NSArray] ()
setLongEraSymbolsSelector = mkSelector "setLongEraSymbols:"

-- | @Selector@ for @veryShortMonthSymbols@
veryShortMonthSymbolsSelector :: Selector '[] (Id NSArray)
veryShortMonthSymbolsSelector = mkSelector "veryShortMonthSymbols"

-- | @Selector@ for @setVeryShortMonthSymbols:@
setVeryShortMonthSymbolsSelector :: Selector '[Id NSArray] ()
setVeryShortMonthSymbolsSelector = mkSelector "setVeryShortMonthSymbols:"

-- | @Selector@ for @standaloneMonthSymbols@
standaloneMonthSymbolsSelector :: Selector '[] (Id NSArray)
standaloneMonthSymbolsSelector = mkSelector "standaloneMonthSymbols"

-- | @Selector@ for @setStandaloneMonthSymbols:@
setStandaloneMonthSymbolsSelector :: Selector '[Id NSArray] ()
setStandaloneMonthSymbolsSelector = mkSelector "setStandaloneMonthSymbols:"

-- | @Selector@ for @shortStandaloneMonthSymbols@
shortStandaloneMonthSymbolsSelector :: Selector '[] (Id NSArray)
shortStandaloneMonthSymbolsSelector = mkSelector "shortStandaloneMonthSymbols"

-- | @Selector@ for @setShortStandaloneMonthSymbols:@
setShortStandaloneMonthSymbolsSelector :: Selector '[Id NSArray] ()
setShortStandaloneMonthSymbolsSelector = mkSelector "setShortStandaloneMonthSymbols:"

-- | @Selector@ for @veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbolsSelector :: Selector '[] (Id NSArray)
veryShortStandaloneMonthSymbolsSelector = mkSelector "veryShortStandaloneMonthSymbols"

-- | @Selector@ for @setVeryShortStandaloneMonthSymbols:@
setVeryShortStandaloneMonthSymbolsSelector :: Selector '[Id NSArray] ()
setVeryShortStandaloneMonthSymbolsSelector = mkSelector "setVeryShortStandaloneMonthSymbols:"

-- | @Selector@ for @veryShortWeekdaySymbols@
veryShortWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
veryShortWeekdaySymbolsSelector = mkSelector "veryShortWeekdaySymbols"

-- | @Selector@ for @setVeryShortWeekdaySymbols:@
setVeryShortWeekdaySymbolsSelector :: Selector '[Id NSArray] ()
setVeryShortWeekdaySymbolsSelector = mkSelector "setVeryShortWeekdaySymbols:"

-- | @Selector@ for @standaloneWeekdaySymbols@
standaloneWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
standaloneWeekdaySymbolsSelector = mkSelector "standaloneWeekdaySymbols"

-- | @Selector@ for @setStandaloneWeekdaySymbols:@
setStandaloneWeekdaySymbolsSelector :: Selector '[Id NSArray] ()
setStandaloneWeekdaySymbolsSelector = mkSelector "setStandaloneWeekdaySymbols:"

-- | @Selector@ for @shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
shortStandaloneWeekdaySymbolsSelector = mkSelector "shortStandaloneWeekdaySymbols"

-- | @Selector@ for @setShortStandaloneWeekdaySymbols:@
setShortStandaloneWeekdaySymbolsSelector :: Selector '[Id NSArray] ()
setShortStandaloneWeekdaySymbolsSelector = mkSelector "setShortStandaloneWeekdaySymbols:"

-- | @Selector@ for @veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbolsSelector :: Selector '[] (Id NSArray)
veryShortStandaloneWeekdaySymbolsSelector = mkSelector "veryShortStandaloneWeekdaySymbols"

-- | @Selector@ for @setVeryShortStandaloneWeekdaySymbols:@
setVeryShortStandaloneWeekdaySymbolsSelector :: Selector '[Id NSArray] ()
setVeryShortStandaloneWeekdaySymbolsSelector = mkSelector "setVeryShortStandaloneWeekdaySymbols:"

-- | @Selector@ for @quarterSymbols@
quarterSymbolsSelector :: Selector '[] (Id NSArray)
quarterSymbolsSelector = mkSelector "quarterSymbols"

-- | @Selector@ for @setQuarterSymbols:@
setQuarterSymbolsSelector :: Selector '[Id NSArray] ()
setQuarterSymbolsSelector = mkSelector "setQuarterSymbols:"

-- | @Selector@ for @shortQuarterSymbols@
shortQuarterSymbolsSelector :: Selector '[] (Id NSArray)
shortQuarterSymbolsSelector = mkSelector "shortQuarterSymbols"

-- | @Selector@ for @setShortQuarterSymbols:@
setShortQuarterSymbolsSelector :: Selector '[Id NSArray] ()
setShortQuarterSymbolsSelector = mkSelector "setShortQuarterSymbols:"

-- | @Selector@ for @standaloneQuarterSymbols@
standaloneQuarterSymbolsSelector :: Selector '[] (Id NSArray)
standaloneQuarterSymbolsSelector = mkSelector "standaloneQuarterSymbols"

-- | @Selector@ for @setStandaloneQuarterSymbols:@
setStandaloneQuarterSymbolsSelector :: Selector '[Id NSArray] ()
setStandaloneQuarterSymbolsSelector = mkSelector "setStandaloneQuarterSymbols:"

-- | @Selector@ for @shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbolsSelector :: Selector '[] (Id NSArray)
shortStandaloneQuarterSymbolsSelector = mkSelector "shortStandaloneQuarterSymbols"

-- | @Selector@ for @setShortStandaloneQuarterSymbols:@
setShortStandaloneQuarterSymbolsSelector :: Selector '[Id NSArray] ()
setShortStandaloneQuarterSymbolsSelector = mkSelector "setShortStandaloneQuarterSymbols:"

-- | @Selector@ for @gregorianStartDate@
gregorianStartDateSelector :: Selector '[] (Id NSDate)
gregorianStartDateSelector = mkSelector "gregorianStartDate"

-- | @Selector@ for @setGregorianStartDate:@
setGregorianStartDateSelector :: Selector '[Id NSDate] ()
setGregorianStartDateSelector = mkSelector "setGregorianStartDate:"

-- | @Selector@ for @doesRelativeDateFormatting@
doesRelativeDateFormattingSelector :: Selector '[] Bool
doesRelativeDateFormattingSelector = mkSelector "doesRelativeDateFormatting"

-- | @Selector@ for @setDoesRelativeDateFormatting:@
setDoesRelativeDateFormattingSelector :: Selector '[Bool] ()
setDoesRelativeDateFormattingSelector = mkSelector "setDoesRelativeDateFormatting:"

