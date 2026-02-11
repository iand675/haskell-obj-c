{-# LANGUAGE PatternSynonyms #-}
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
  , getObjectValue_forString_range_errorSelector
  , stringFromDateSelector
  , dateFromStringSelector
  , localizedStringFromDate_dateStyle_timeStyleSelector
  , dateFormatFromTemplate_options_localeSelector
  , setLocalizedDateFormatFromTemplateSelector
  , initWithDateFormat_allowNaturalLanguageSelector
  , allowsNaturalLanguageSelector
  , formattingContextSelector
  , setFormattingContextSelector
  , defaultFormatterBehaviorSelector
  , setDefaultFormatterBehaviorSelector
  , dateFormatSelector
  , setDateFormatSelector
  , dateStyleSelector
  , setDateStyleSelector
  , timeStyleSelector
  , setTimeStyleSelector
  , localeSelector
  , setLocaleSelector
  , generatesCalendarDatesSelector
  , setGeneratesCalendarDatesSelector
  , formatterBehaviorSelector
  , setFormatterBehaviorSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , calendarSelector
  , setCalendarSelector
  , lenientSelector
  , setLenientSelector
  , twoDigitStartDateSelector
  , setTwoDigitStartDateSelector
  , defaultDateSelector
  , setDefaultDateSelector
  , eraSymbolsSelector
  , setEraSymbolsSelector
  , monthSymbolsSelector
  , setMonthSymbolsSelector
  , shortMonthSymbolsSelector
  , setShortMonthSymbolsSelector
  , weekdaySymbolsSelector
  , setWeekdaySymbolsSelector
  , shortWeekdaySymbolsSelector
  , setShortWeekdaySymbolsSelector
  , amSymbolSelector
  , setAMSymbolSelector
  , pmSymbolSelector
  , setPMSymbolSelector
  , longEraSymbolsSelector
  , setLongEraSymbolsSelector
  , veryShortMonthSymbolsSelector
  , setVeryShortMonthSymbolsSelector
  , standaloneMonthSymbolsSelector
  , setStandaloneMonthSymbolsSelector
  , shortStandaloneMonthSymbolsSelector
  , setShortStandaloneMonthSymbolsSelector
  , veryShortStandaloneMonthSymbolsSelector
  , setVeryShortStandaloneMonthSymbolsSelector
  , veryShortWeekdaySymbolsSelector
  , setVeryShortWeekdaySymbolsSelector
  , standaloneWeekdaySymbolsSelector
  , setStandaloneWeekdaySymbolsSelector
  , shortStandaloneWeekdaySymbolsSelector
  , setShortStandaloneWeekdaySymbolsSelector
  , veryShortStandaloneWeekdaySymbolsSelector
  , setVeryShortStandaloneWeekdaySymbolsSelector
  , quarterSymbolsSelector
  , setQuarterSymbolsSelector
  , shortQuarterSymbolsSelector
  , setShortQuarterSymbolsSelector
  , standaloneQuarterSymbolsSelector
  , setStandaloneQuarterSymbolsSelector
  , shortStandaloneQuarterSymbolsSelector
  , setShortStandaloneQuarterSymbolsSelector
  , gregorianStartDateSelector
  , setGregorianStartDateSelector
  , doesRelativeDateFormattingSelector
  , setDoesRelativeDateFormattingSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- getObjectValue:forString:range:error:@
getObjectValue_forString_range_error :: (IsNSDateFormatter nsDateFormatter, IsNSString string, IsNSError error_) => nsDateFormatter -> Ptr RawId -> string -> Ptr NSRange -> error_ -> IO Bool
getObjectValue_forString_range_error nsDateFormatter  obj_ string rangep error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateFormatter (mkSelector "getObjectValue:forString:range:error:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr rangep, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- stringFromDate:@
stringFromDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate date) => nsDateFormatter -> date -> IO (Id NSString)
stringFromDate nsDateFormatter  date =
withObjCPtr date $ \raw_date ->
    sendMsg nsDateFormatter (mkSelector "stringFromDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- dateFromString:@
dateFromString :: (IsNSDateFormatter nsDateFormatter, IsNSString string) => nsDateFormatter -> string -> IO (Id NSDate)
dateFromString nsDateFormatter  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsDateFormatter (mkSelector "dateFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localizedStringFromDate:dateStyle:timeStyle:@
localizedStringFromDate_dateStyle_timeStyle :: IsNSDate date => date -> NSDateFormatterStyle -> NSDateFormatterStyle -> IO (Id NSString)
localizedStringFromDate_dateStyle_timeStyle date dstyle tstyle =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "localizedStringFromDate:dateStyle:timeStyle:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argCULong (coerce dstyle), argCULong (coerce tstyle)] >>= retainedObject . castPtr

-- | @+ dateFormatFromTemplate:options:locale:@
dateFormatFromTemplate_options_locale :: (IsNSString tmplate, IsNSLocale locale) => tmplate -> CULong -> locale -> IO (Id NSString)
dateFormatFromTemplate_options_locale tmplate opts locale =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    withObjCPtr tmplate $ \raw_tmplate ->
      withObjCPtr locale $ \raw_locale ->
        sendClassMsg cls' (mkSelector "dateFormatFromTemplate:options:locale:") (retPtr retVoid) [argPtr (castPtr raw_tmplate :: Ptr ()), argCULong (fromIntegral opts), argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @- setLocalizedDateFormatFromTemplate:@
setLocalizedDateFormatFromTemplate :: (IsNSDateFormatter nsDateFormatter, IsNSString dateFormatTemplate) => nsDateFormatter -> dateFormatTemplate -> IO ()
setLocalizedDateFormatFromTemplate nsDateFormatter  dateFormatTemplate =
withObjCPtr dateFormatTemplate $ \raw_dateFormatTemplate ->
    sendMsg nsDateFormatter (mkSelector "setLocalizedDateFormatFromTemplate:") retVoid [argPtr (castPtr raw_dateFormatTemplate :: Ptr ())]

-- | @- initWithDateFormat:allowNaturalLanguage:@
initWithDateFormat_allowNaturalLanguage :: (IsNSDateFormatter nsDateFormatter, IsNSString format) => nsDateFormatter -> format -> Bool -> IO RawId
initWithDateFormat_allowNaturalLanguage nsDateFormatter  format flag =
withObjCPtr format $ \raw_format ->
    fmap (RawId . castPtr) $ sendMsg nsDateFormatter (mkSelector "initWithDateFormat:allowNaturalLanguage:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- allowsNaturalLanguage@
allowsNaturalLanguage :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
allowsNaturalLanguage nsDateFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateFormatter (mkSelector "allowsNaturalLanguage") retCULong []

-- | @- formattingContext@
formattingContext :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSFormattingContext
formattingContext nsDateFormatter  =
  fmap (coerce :: CLong -> NSFormattingContext) $ sendMsg nsDateFormatter (mkSelector "formattingContext") retCLong []

-- | @- setFormattingContext:@
setFormattingContext :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setFormattingContext:") retVoid [argCLong (coerce value)]

-- | @+ defaultFormatterBehavior@
defaultFormatterBehavior :: IO NSDateFormatterBehavior
defaultFormatterBehavior  =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    fmap (coerce :: CULong -> NSDateFormatterBehavior) $ sendClassMsg cls' (mkSelector "defaultFormatterBehavior") retCULong []

-- | @+ setDefaultFormatterBehavior:@
setDefaultFormatterBehavior :: NSDateFormatterBehavior -> IO ()
setDefaultFormatterBehavior value =
  do
    cls' <- getRequiredClass "NSDateFormatter"
    sendClassMsg cls' (mkSelector "setDefaultFormatterBehavior:") retVoid [argCULong (coerce value)]

-- | @- dateFormat@
dateFormat :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSString)
dateFormat nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "dateFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateFormat:@
setDateFormat :: (IsNSDateFormatter nsDateFormatter, IsNSString value) => nsDateFormatter -> value -> IO ()
setDateFormat nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setDateFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateStyle@
dateStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSDateFormatterStyle
dateStyle nsDateFormatter  =
  fmap (coerce :: CULong -> NSDateFormatterStyle) $ sendMsg nsDateFormatter (mkSelector "dateStyle") retCULong []

-- | @- setDateStyle:@
setDateStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSDateFormatterStyle -> IO ()
setDateStyle nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setDateStyle:") retVoid [argCULong (coerce value)]

-- | @- timeStyle@
timeStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSDateFormatterStyle
timeStyle nsDateFormatter  =
  fmap (coerce :: CULong -> NSDateFormatterStyle) $ sendMsg nsDateFormatter (mkSelector "timeStyle") retCULong []

-- | @- setTimeStyle:@
setTimeStyle :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSDateFormatterStyle -> IO ()
setTimeStyle nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setTimeStyle:") retVoid [argCULong (coerce value)]

-- | @- locale@
locale :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSLocale)
locale nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSDateFormatter nsDateFormatter, IsNSLocale value) => nsDateFormatter -> value -> IO ()
setLocale nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- generatesCalendarDates@
generatesCalendarDates :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
generatesCalendarDates nsDateFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateFormatter (mkSelector "generatesCalendarDates") retCULong []

-- | @- setGeneratesCalendarDates:@
setGeneratesCalendarDates :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> Bool -> IO ()
setGeneratesCalendarDates nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setGeneratesCalendarDates:") retVoid [argCULong (if value then 1 else 0)]

-- | @- formatterBehavior@
formatterBehavior :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO NSDateFormatterBehavior
formatterBehavior nsDateFormatter  =
  fmap (coerce :: CULong -> NSDateFormatterBehavior) $ sendMsg nsDateFormatter (mkSelector "formatterBehavior") retCULong []

-- | @- setFormatterBehavior:@
setFormatterBehavior :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> NSDateFormatterBehavior -> IO ()
setFormatterBehavior nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setFormatterBehavior:") retVoid [argCULong (coerce value)]

-- | @- timeZone@
timeZone :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSTimeZone)
timeZone nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSDateFormatter nsDateFormatter, IsNSTimeZone value) => nsDateFormatter -> value -> IO ()
setTimeZone nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- calendar@
calendar :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSCalendar)
calendar nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSDateFormatter nsDateFormatter, IsNSCalendar value) => nsDateFormatter -> value -> IO ()
setCalendar nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lenient@
lenient :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
lenient nsDateFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateFormatter (mkSelector "lenient") retCULong []

-- | @- setLenient:@
setLenient :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> Bool -> IO ()
setLenient nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setLenient:") retVoid [argCULong (if value then 1 else 0)]

-- | @- twoDigitStartDate@
twoDigitStartDate :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSDate)
twoDigitStartDate nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "twoDigitStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTwoDigitStartDate:@
setTwoDigitStartDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate value) => nsDateFormatter -> value -> IO ()
setTwoDigitStartDate nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setTwoDigitStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultDate@
defaultDate :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSDate)
defaultDate nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "defaultDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultDate:@
setDefaultDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate value) => nsDateFormatter -> value -> IO ()
setDefaultDate nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setDefaultDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- eraSymbols@
eraSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
eraSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "eraSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEraSymbols:@
setEraSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setEraSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setEraSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- monthSymbols@
monthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
monthSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "monthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMonthSymbols:@
setMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setMonthSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setMonthSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortMonthSymbols@
shortMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortMonthSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "shortMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortMonthSymbols:@
setShortMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortMonthSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setShortMonthSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- weekdaySymbols@
weekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
weekdaySymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "weekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWeekdaySymbols:@
setWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setWeekdaySymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setWeekdaySymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortWeekdaySymbols@
shortWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortWeekdaySymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "shortWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortWeekdaySymbols:@
setShortWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortWeekdaySymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setShortWeekdaySymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- AMSymbol@
amSymbol :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSString)
amSymbol nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "AMSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAMSymbol:@
setAMSymbol :: (IsNSDateFormatter nsDateFormatter, IsNSString value) => nsDateFormatter -> value -> IO ()
setAMSymbol nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setAMSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- PMSymbol@
pmSymbol :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSString)
pmSymbol nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "PMSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPMSymbol:@
setPMSymbol :: (IsNSDateFormatter nsDateFormatter, IsNSString value) => nsDateFormatter -> value -> IO ()
setPMSymbol nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setPMSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- longEraSymbols@
longEraSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
longEraSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "longEraSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLongEraSymbols:@
setLongEraSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setLongEraSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setLongEraSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- veryShortMonthSymbols@
veryShortMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortMonthSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "veryShortMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVeryShortMonthSymbols:@
setVeryShortMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortMonthSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setVeryShortMonthSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standaloneMonthSymbols@
standaloneMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
standaloneMonthSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "standaloneMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandaloneMonthSymbols:@
setStandaloneMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setStandaloneMonthSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setStandaloneMonthSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortStandaloneMonthSymbols@
shortStandaloneMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortStandaloneMonthSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "shortStandaloneMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortStandaloneMonthSymbols:@
setShortStandaloneMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortStandaloneMonthSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setShortStandaloneMonthSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortStandaloneMonthSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "veryShortStandaloneMonthSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVeryShortStandaloneMonthSymbols:@
setVeryShortStandaloneMonthSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortStandaloneMonthSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setVeryShortStandaloneMonthSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- veryShortWeekdaySymbols@
veryShortWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortWeekdaySymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "veryShortWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVeryShortWeekdaySymbols:@
setVeryShortWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortWeekdaySymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setVeryShortWeekdaySymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standaloneWeekdaySymbols@
standaloneWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
standaloneWeekdaySymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "standaloneWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandaloneWeekdaySymbols:@
setStandaloneWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setStandaloneWeekdaySymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setStandaloneWeekdaySymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortStandaloneWeekdaySymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "shortStandaloneWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortStandaloneWeekdaySymbols:@
setShortStandaloneWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortStandaloneWeekdaySymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setShortStandaloneWeekdaySymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
veryShortStandaloneWeekdaySymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "veryShortStandaloneWeekdaySymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVeryShortStandaloneWeekdaySymbols:@
setVeryShortStandaloneWeekdaySymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setVeryShortStandaloneWeekdaySymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setVeryShortStandaloneWeekdaySymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- quarterSymbols@
quarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
quarterSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "quarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuarterSymbols:@
setQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setQuarterSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setQuarterSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortQuarterSymbols@
shortQuarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortQuarterSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "shortQuarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortQuarterSymbols:@
setShortQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortQuarterSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setShortQuarterSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standaloneQuarterSymbols@
standaloneQuarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
standaloneQuarterSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "standaloneQuarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandaloneQuarterSymbols:@
setStandaloneQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setStandaloneQuarterSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setStandaloneQuarterSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbols :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSArray)
shortStandaloneQuarterSymbols nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "shortStandaloneQuarterSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortStandaloneQuarterSymbols:@
setShortStandaloneQuarterSymbols :: (IsNSDateFormatter nsDateFormatter, IsNSArray value) => nsDateFormatter -> value -> IO ()
setShortStandaloneQuarterSymbols nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setShortStandaloneQuarterSymbols:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gregorianStartDate@
gregorianStartDate :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO (Id NSDate)
gregorianStartDate nsDateFormatter  =
  sendMsg nsDateFormatter (mkSelector "gregorianStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGregorianStartDate:@
setGregorianStartDate :: (IsNSDateFormatter nsDateFormatter, IsNSDate value) => nsDateFormatter -> value -> IO ()
setGregorianStartDate nsDateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateFormatter (mkSelector "setGregorianStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- doesRelativeDateFormatting@
doesRelativeDateFormatting :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> IO Bool
doesRelativeDateFormatting nsDateFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateFormatter (mkSelector "doesRelativeDateFormatting") retCULong []

-- | @- setDoesRelativeDateFormatting:@
setDoesRelativeDateFormatting :: IsNSDateFormatter nsDateFormatter => nsDateFormatter -> Bool -> IO ()
setDoesRelativeDateFormatting nsDateFormatter  value =
  sendMsg nsDateFormatter (mkSelector "setDoesRelativeDateFormatting:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getObjectValue:forString:range:error:@
getObjectValue_forString_range_errorSelector :: Selector
getObjectValue_forString_range_errorSelector = mkSelector "getObjectValue:forString:range:error:"

-- | @Selector@ for @stringFromDate:@
stringFromDateSelector :: Selector
stringFromDateSelector = mkSelector "stringFromDate:"

-- | @Selector@ for @dateFromString:@
dateFromStringSelector :: Selector
dateFromStringSelector = mkSelector "dateFromString:"

-- | @Selector@ for @localizedStringFromDate:dateStyle:timeStyle:@
localizedStringFromDate_dateStyle_timeStyleSelector :: Selector
localizedStringFromDate_dateStyle_timeStyleSelector = mkSelector "localizedStringFromDate:dateStyle:timeStyle:"

-- | @Selector@ for @dateFormatFromTemplate:options:locale:@
dateFormatFromTemplate_options_localeSelector :: Selector
dateFormatFromTemplate_options_localeSelector = mkSelector "dateFormatFromTemplate:options:locale:"

-- | @Selector@ for @setLocalizedDateFormatFromTemplate:@
setLocalizedDateFormatFromTemplateSelector :: Selector
setLocalizedDateFormatFromTemplateSelector = mkSelector "setLocalizedDateFormatFromTemplate:"

-- | @Selector@ for @initWithDateFormat:allowNaturalLanguage:@
initWithDateFormat_allowNaturalLanguageSelector :: Selector
initWithDateFormat_allowNaturalLanguageSelector = mkSelector "initWithDateFormat:allowNaturalLanguage:"

-- | @Selector@ for @allowsNaturalLanguage@
allowsNaturalLanguageSelector :: Selector
allowsNaturalLanguageSelector = mkSelector "allowsNaturalLanguage"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector
setFormattingContextSelector = mkSelector "setFormattingContext:"

-- | @Selector@ for @defaultFormatterBehavior@
defaultFormatterBehaviorSelector :: Selector
defaultFormatterBehaviorSelector = mkSelector "defaultFormatterBehavior"

-- | @Selector@ for @setDefaultFormatterBehavior:@
setDefaultFormatterBehaviorSelector :: Selector
setDefaultFormatterBehaviorSelector = mkSelector "setDefaultFormatterBehavior:"

-- | @Selector@ for @dateFormat@
dateFormatSelector :: Selector
dateFormatSelector = mkSelector "dateFormat"

-- | @Selector@ for @setDateFormat:@
setDateFormatSelector :: Selector
setDateFormatSelector = mkSelector "setDateFormat:"

-- | @Selector@ for @dateStyle@
dateStyleSelector :: Selector
dateStyleSelector = mkSelector "dateStyle"

-- | @Selector@ for @setDateStyle:@
setDateStyleSelector :: Selector
setDateStyleSelector = mkSelector "setDateStyle:"

-- | @Selector@ for @timeStyle@
timeStyleSelector :: Selector
timeStyleSelector = mkSelector "timeStyle"

-- | @Selector@ for @setTimeStyle:@
setTimeStyleSelector :: Selector
setTimeStyleSelector = mkSelector "setTimeStyle:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @generatesCalendarDates@
generatesCalendarDatesSelector :: Selector
generatesCalendarDatesSelector = mkSelector "generatesCalendarDates"

-- | @Selector@ for @setGeneratesCalendarDates:@
setGeneratesCalendarDatesSelector :: Selector
setGeneratesCalendarDatesSelector = mkSelector "setGeneratesCalendarDates:"

-- | @Selector@ for @formatterBehavior@
formatterBehaviorSelector :: Selector
formatterBehaviorSelector = mkSelector "formatterBehavior"

-- | @Selector@ for @setFormatterBehavior:@
setFormatterBehaviorSelector :: Selector
setFormatterBehaviorSelector = mkSelector "setFormatterBehavior:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @lenient@
lenientSelector :: Selector
lenientSelector = mkSelector "lenient"

-- | @Selector@ for @setLenient:@
setLenientSelector :: Selector
setLenientSelector = mkSelector "setLenient:"

-- | @Selector@ for @twoDigitStartDate@
twoDigitStartDateSelector :: Selector
twoDigitStartDateSelector = mkSelector "twoDigitStartDate"

-- | @Selector@ for @setTwoDigitStartDate:@
setTwoDigitStartDateSelector :: Selector
setTwoDigitStartDateSelector = mkSelector "setTwoDigitStartDate:"

-- | @Selector@ for @defaultDate@
defaultDateSelector :: Selector
defaultDateSelector = mkSelector "defaultDate"

-- | @Selector@ for @setDefaultDate:@
setDefaultDateSelector :: Selector
setDefaultDateSelector = mkSelector "setDefaultDate:"

-- | @Selector@ for @eraSymbols@
eraSymbolsSelector :: Selector
eraSymbolsSelector = mkSelector "eraSymbols"

-- | @Selector@ for @setEraSymbols:@
setEraSymbolsSelector :: Selector
setEraSymbolsSelector = mkSelector "setEraSymbols:"

-- | @Selector@ for @monthSymbols@
monthSymbolsSelector :: Selector
monthSymbolsSelector = mkSelector "monthSymbols"

-- | @Selector@ for @setMonthSymbols:@
setMonthSymbolsSelector :: Selector
setMonthSymbolsSelector = mkSelector "setMonthSymbols:"

-- | @Selector@ for @shortMonthSymbols@
shortMonthSymbolsSelector :: Selector
shortMonthSymbolsSelector = mkSelector "shortMonthSymbols"

-- | @Selector@ for @setShortMonthSymbols:@
setShortMonthSymbolsSelector :: Selector
setShortMonthSymbolsSelector = mkSelector "setShortMonthSymbols:"

-- | @Selector@ for @weekdaySymbols@
weekdaySymbolsSelector :: Selector
weekdaySymbolsSelector = mkSelector "weekdaySymbols"

-- | @Selector@ for @setWeekdaySymbols:@
setWeekdaySymbolsSelector :: Selector
setWeekdaySymbolsSelector = mkSelector "setWeekdaySymbols:"

-- | @Selector@ for @shortWeekdaySymbols@
shortWeekdaySymbolsSelector :: Selector
shortWeekdaySymbolsSelector = mkSelector "shortWeekdaySymbols"

-- | @Selector@ for @setShortWeekdaySymbols:@
setShortWeekdaySymbolsSelector :: Selector
setShortWeekdaySymbolsSelector = mkSelector "setShortWeekdaySymbols:"

-- | @Selector@ for @AMSymbol@
amSymbolSelector :: Selector
amSymbolSelector = mkSelector "AMSymbol"

-- | @Selector@ for @setAMSymbol:@
setAMSymbolSelector :: Selector
setAMSymbolSelector = mkSelector "setAMSymbol:"

-- | @Selector@ for @PMSymbol@
pmSymbolSelector :: Selector
pmSymbolSelector = mkSelector "PMSymbol"

-- | @Selector@ for @setPMSymbol:@
setPMSymbolSelector :: Selector
setPMSymbolSelector = mkSelector "setPMSymbol:"

-- | @Selector@ for @longEraSymbols@
longEraSymbolsSelector :: Selector
longEraSymbolsSelector = mkSelector "longEraSymbols"

-- | @Selector@ for @setLongEraSymbols:@
setLongEraSymbolsSelector :: Selector
setLongEraSymbolsSelector = mkSelector "setLongEraSymbols:"

-- | @Selector@ for @veryShortMonthSymbols@
veryShortMonthSymbolsSelector :: Selector
veryShortMonthSymbolsSelector = mkSelector "veryShortMonthSymbols"

-- | @Selector@ for @setVeryShortMonthSymbols:@
setVeryShortMonthSymbolsSelector :: Selector
setVeryShortMonthSymbolsSelector = mkSelector "setVeryShortMonthSymbols:"

-- | @Selector@ for @standaloneMonthSymbols@
standaloneMonthSymbolsSelector :: Selector
standaloneMonthSymbolsSelector = mkSelector "standaloneMonthSymbols"

-- | @Selector@ for @setStandaloneMonthSymbols:@
setStandaloneMonthSymbolsSelector :: Selector
setStandaloneMonthSymbolsSelector = mkSelector "setStandaloneMonthSymbols:"

-- | @Selector@ for @shortStandaloneMonthSymbols@
shortStandaloneMonthSymbolsSelector :: Selector
shortStandaloneMonthSymbolsSelector = mkSelector "shortStandaloneMonthSymbols"

-- | @Selector@ for @setShortStandaloneMonthSymbols:@
setShortStandaloneMonthSymbolsSelector :: Selector
setShortStandaloneMonthSymbolsSelector = mkSelector "setShortStandaloneMonthSymbols:"

-- | @Selector@ for @veryShortStandaloneMonthSymbols@
veryShortStandaloneMonthSymbolsSelector :: Selector
veryShortStandaloneMonthSymbolsSelector = mkSelector "veryShortStandaloneMonthSymbols"

-- | @Selector@ for @setVeryShortStandaloneMonthSymbols:@
setVeryShortStandaloneMonthSymbolsSelector :: Selector
setVeryShortStandaloneMonthSymbolsSelector = mkSelector "setVeryShortStandaloneMonthSymbols:"

-- | @Selector@ for @veryShortWeekdaySymbols@
veryShortWeekdaySymbolsSelector :: Selector
veryShortWeekdaySymbolsSelector = mkSelector "veryShortWeekdaySymbols"

-- | @Selector@ for @setVeryShortWeekdaySymbols:@
setVeryShortWeekdaySymbolsSelector :: Selector
setVeryShortWeekdaySymbolsSelector = mkSelector "setVeryShortWeekdaySymbols:"

-- | @Selector@ for @standaloneWeekdaySymbols@
standaloneWeekdaySymbolsSelector :: Selector
standaloneWeekdaySymbolsSelector = mkSelector "standaloneWeekdaySymbols"

-- | @Selector@ for @setStandaloneWeekdaySymbols:@
setStandaloneWeekdaySymbolsSelector :: Selector
setStandaloneWeekdaySymbolsSelector = mkSelector "setStandaloneWeekdaySymbols:"

-- | @Selector@ for @shortStandaloneWeekdaySymbols@
shortStandaloneWeekdaySymbolsSelector :: Selector
shortStandaloneWeekdaySymbolsSelector = mkSelector "shortStandaloneWeekdaySymbols"

-- | @Selector@ for @setShortStandaloneWeekdaySymbols:@
setShortStandaloneWeekdaySymbolsSelector :: Selector
setShortStandaloneWeekdaySymbolsSelector = mkSelector "setShortStandaloneWeekdaySymbols:"

-- | @Selector@ for @veryShortStandaloneWeekdaySymbols@
veryShortStandaloneWeekdaySymbolsSelector :: Selector
veryShortStandaloneWeekdaySymbolsSelector = mkSelector "veryShortStandaloneWeekdaySymbols"

-- | @Selector@ for @setVeryShortStandaloneWeekdaySymbols:@
setVeryShortStandaloneWeekdaySymbolsSelector :: Selector
setVeryShortStandaloneWeekdaySymbolsSelector = mkSelector "setVeryShortStandaloneWeekdaySymbols:"

-- | @Selector@ for @quarterSymbols@
quarterSymbolsSelector :: Selector
quarterSymbolsSelector = mkSelector "quarterSymbols"

-- | @Selector@ for @setQuarterSymbols:@
setQuarterSymbolsSelector :: Selector
setQuarterSymbolsSelector = mkSelector "setQuarterSymbols:"

-- | @Selector@ for @shortQuarterSymbols@
shortQuarterSymbolsSelector :: Selector
shortQuarterSymbolsSelector = mkSelector "shortQuarterSymbols"

-- | @Selector@ for @setShortQuarterSymbols:@
setShortQuarterSymbolsSelector :: Selector
setShortQuarterSymbolsSelector = mkSelector "setShortQuarterSymbols:"

-- | @Selector@ for @standaloneQuarterSymbols@
standaloneQuarterSymbolsSelector :: Selector
standaloneQuarterSymbolsSelector = mkSelector "standaloneQuarterSymbols"

-- | @Selector@ for @setStandaloneQuarterSymbols:@
setStandaloneQuarterSymbolsSelector :: Selector
setStandaloneQuarterSymbolsSelector = mkSelector "setStandaloneQuarterSymbols:"

-- | @Selector@ for @shortStandaloneQuarterSymbols@
shortStandaloneQuarterSymbolsSelector :: Selector
shortStandaloneQuarterSymbolsSelector = mkSelector "shortStandaloneQuarterSymbols"

-- | @Selector@ for @setShortStandaloneQuarterSymbols:@
setShortStandaloneQuarterSymbolsSelector :: Selector
setShortStandaloneQuarterSymbolsSelector = mkSelector "setShortStandaloneQuarterSymbols:"

-- | @Selector@ for @gregorianStartDate@
gregorianStartDateSelector :: Selector
gregorianStartDateSelector = mkSelector "gregorianStartDate"

-- | @Selector@ for @setGregorianStartDate:@
setGregorianStartDateSelector :: Selector
setGregorianStartDateSelector = mkSelector "setGregorianStartDate:"

-- | @Selector@ for @doesRelativeDateFormatting@
doesRelativeDateFormattingSelector :: Selector
doesRelativeDateFormattingSelector = mkSelector "doesRelativeDateFormatting"

-- | @Selector@ for @setDoesRelativeDateFormatting:@
setDoesRelativeDateFormattingSelector :: Selector
setDoesRelativeDateFormattingSelector = mkSelector "setDoesRelativeDateFormatting:"

