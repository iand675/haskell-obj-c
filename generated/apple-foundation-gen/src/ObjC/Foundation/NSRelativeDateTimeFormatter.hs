{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRelativeDateTimeFormatter@.
module ObjC.Foundation.NSRelativeDateTimeFormatter
  ( NSRelativeDateTimeFormatter
  , IsNSRelativeDateTimeFormatter(..)
  , localizedStringFromDateComponents
  , localizedStringFromTimeInterval
  , localizedStringForDate_relativeToDate
  , stringForObjectValue
  , dateTimeStyle
  , setDateTimeStyle
  , unitsStyle
  , setUnitsStyle
  , formattingContext
  , setFormattingContext
  , calendar
  , setCalendar
  , locale
  , setLocale
  , calendarSelector
  , dateTimeStyleSelector
  , formattingContextSelector
  , localeSelector
  , localizedStringForDate_relativeToDateSelector
  , localizedStringFromDateComponentsSelector
  , localizedStringFromTimeIntervalSelector
  , setCalendarSelector
  , setDateTimeStyleSelector
  , setFormattingContextSelector
  , setLocaleSelector
  , setUnitsStyleSelector
  , stringForObjectValueSelector
  , unitsStyleSelector

  -- * Enum types
  , NSFormattingContext(NSFormattingContext)
  , pattern NSFormattingContextUnknown
  , pattern NSFormattingContextDynamic
  , pattern NSFormattingContextStandalone
  , pattern NSFormattingContextListItem
  , pattern NSFormattingContextBeginningOfSentence
  , pattern NSFormattingContextMiddleOfSentence
  , NSRelativeDateTimeFormatterStyle(NSRelativeDateTimeFormatterStyle)
  , pattern NSRelativeDateTimeFormatterStyleNumeric
  , pattern NSRelativeDateTimeFormatterStyleNamed
  , NSRelativeDateTimeFormatterUnitsStyle(NSRelativeDateTimeFormatterUnitsStyle)
  , pattern NSRelativeDateTimeFormatterUnitsStyleFull
  , pattern NSRelativeDateTimeFormatterUnitsStyleSpellOut
  , pattern NSRelativeDateTimeFormatterUnitsStyleShort
  , pattern NSRelativeDateTimeFormatterUnitsStyleAbbreviated

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- localizedStringFromDateComponents:@
localizedStringFromDateComponents :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSDateComponents dateComponents) => nsRelativeDateTimeFormatter -> dateComponents -> IO (Id NSString)
localizedStringFromDateComponents nsRelativeDateTimeFormatter dateComponents =
  sendMessage nsRelativeDateTimeFormatter localizedStringFromDateComponentsSelector (toNSDateComponents dateComponents)

-- | @- localizedStringFromTimeInterval:@
localizedStringFromTimeInterval :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> CDouble -> IO (Id NSString)
localizedStringFromTimeInterval nsRelativeDateTimeFormatter timeInterval =
  sendMessage nsRelativeDateTimeFormatter localizedStringFromTimeIntervalSelector timeInterval

-- | @- localizedStringForDate:relativeToDate:@
localizedStringForDate_relativeToDate :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSDate date, IsNSDate referenceDate) => nsRelativeDateTimeFormatter -> date -> referenceDate -> IO (Id NSString)
localizedStringForDate_relativeToDate nsRelativeDateTimeFormatter date referenceDate =
  sendMessage nsRelativeDateTimeFormatter localizedStringForDate_relativeToDateSelector (toNSDate date) (toNSDate referenceDate)

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsRelativeDateTimeFormatter obj_ =
  sendMessage nsRelativeDateTimeFormatter stringForObjectValueSelector obj_

-- | @- dateTimeStyle@
dateTimeStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO NSRelativeDateTimeFormatterStyle
dateTimeStyle nsRelativeDateTimeFormatter =
  sendMessage nsRelativeDateTimeFormatter dateTimeStyleSelector

-- | @- setDateTimeStyle:@
setDateTimeStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> NSRelativeDateTimeFormatterStyle -> IO ()
setDateTimeStyle nsRelativeDateTimeFormatter value =
  sendMessage nsRelativeDateTimeFormatter setDateTimeStyleSelector value

-- | @- unitsStyle@
unitsStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO NSRelativeDateTimeFormatterUnitsStyle
unitsStyle nsRelativeDateTimeFormatter =
  sendMessage nsRelativeDateTimeFormatter unitsStyleSelector

-- | @- setUnitsStyle:@
setUnitsStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> NSRelativeDateTimeFormatterUnitsStyle -> IO ()
setUnitsStyle nsRelativeDateTimeFormatter value =
  sendMessage nsRelativeDateTimeFormatter setUnitsStyleSelector value

-- | @- formattingContext@
formattingContext :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO NSFormattingContext
formattingContext nsRelativeDateTimeFormatter =
  sendMessage nsRelativeDateTimeFormatter formattingContextSelector

-- | @- setFormattingContext:@
setFormattingContext :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsRelativeDateTimeFormatter value =
  sendMessage nsRelativeDateTimeFormatter setFormattingContextSelector value

-- | @- calendar@
calendar :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO (Id NSCalendar)
calendar nsRelativeDateTimeFormatter =
  sendMessage nsRelativeDateTimeFormatter calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSCalendar value) => nsRelativeDateTimeFormatter -> value -> IO ()
setCalendar nsRelativeDateTimeFormatter value =
  sendMessage nsRelativeDateTimeFormatter setCalendarSelector (toNSCalendar value)

-- | @- locale@
locale :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO (Id NSLocale)
locale nsRelativeDateTimeFormatter =
  sendMessage nsRelativeDateTimeFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSLocale value) => nsRelativeDateTimeFormatter -> value -> IO ()
setLocale nsRelativeDateTimeFormatter value =
  sendMessage nsRelativeDateTimeFormatter setLocaleSelector (toNSLocale value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringFromDateComponents:@
localizedStringFromDateComponentsSelector :: Selector '[Id NSDateComponents] (Id NSString)
localizedStringFromDateComponentsSelector = mkSelector "localizedStringFromDateComponents:"

-- | @Selector@ for @localizedStringFromTimeInterval:@
localizedStringFromTimeIntervalSelector :: Selector '[CDouble] (Id NSString)
localizedStringFromTimeIntervalSelector = mkSelector "localizedStringFromTimeInterval:"

-- | @Selector@ for @localizedStringForDate:relativeToDate:@
localizedStringForDate_relativeToDateSelector :: Selector '[Id NSDate, Id NSDate] (Id NSString)
localizedStringForDate_relativeToDateSelector = mkSelector "localizedStringForDate:relativeToDate:"

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector '[RawId] (Id NSString)
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @dateTimeStyle@
dateTimeStyleSelector :: Selector '[] NSRelativeDateTimeFormatterStyle
dateTimeStyleSelector = mkSelector "dateTimeStyle"

-- | @Selector@ for @setDateTimeStyle:@
setDateTimeStyleSelector :: Selector '[NSRelativeDateTimeFormatterStyle] ()
setDateTimeStyleSelector = mkSelector "setDateTimeStyle:"

-- | @Selector@ for @unitsStyle@
unitsStyleSelector :: Selector '[] NSRelativeDateTimeFormatterUnitsStyle
unitsStyleSelector = mkSelector "unitsStyle"

-- | @Selector@ for @setUnitsStyle:@
setUnitsStyleSelector :: Selector '[NSRelativeDateTimeFormatterUnitsStyle] ()
setUnitsStyleSelector = mkSelector "setUnitsStyle:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector '[] NSFormattingContext
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector '[NSFormattingContext] ()
setFormattingContextSelector = mkSelector "setFormattingContext:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id NSCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id NSCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

