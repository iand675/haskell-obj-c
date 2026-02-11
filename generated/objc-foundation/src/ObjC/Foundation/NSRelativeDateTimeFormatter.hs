{-# LANGUAGE PatternSynonyms #-}
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
  , localizedStringFromDateComponentsSelector
  , localizedStringFromTimeIntervalSelector
  , localizedStringForDate_relativeToDateSelector
  , stringForObjectValueSelector
  , dateTimeStyleSelector
  , setDateTimeStyleSelector
  , unitsStyleSelector
  , setUnitsStyleSelector
  , formattingContextSelector
  , setFormattingContextSelector
  , calendarSelector
  , setCalendarSelector
  , localeSelector
  , setLocaleSelector

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

-- | @- localizedStringFromDateComponents:@
localizedStringFromDateComponents :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSDateComponents dateComponents) => nsRelativeDateTimeFormatter -> dateComponents -> IO (Id NSString)
localizedStringFromDateComponents nsRelativeDateTimeFormatter  dateComponents =
withObjCPtr dateComponents $ \raw_dateComponents ->
    sendMsg nsRelativeDateTimeFormatter (mkSelector "localizedStringFromDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_dateComponents :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringFromTimeInterval:@
localizedStringFromTimeInterval :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> CDouble -> IO (Id NSString)
localizedStringFromTimeInterval nsRelativeDateTimeFormatter  timeInterval =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "localizedStringFromTimeInterval:") (retPtr retVoid) [argCDouble (fromIntegral timeInterval)] >>= retainedObject . castPtr

-- | @- localizedStringForDate:relativeToDate:@
localizedStringForDate_relativeToDate :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSDate date, IsNSDate referenceDate) => nsRelativeDateTimeFormatter -> date -> referenceDate -> IO (Id NSString)
localizedStringForDate_relativeToDate nsRelativeDateTimeFormatter  date referenceDate =
withObjCPtr date $ \raw_date ->
  withObjCPtr referenceDate $ \raw_referenceDate ->
      sendMsg nsRelativeDateTimeFormatter (mkSelector "localizedStringForDate:relativeToDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_referenceDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsRelativeDateTimeFormatter  obj_ =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "stringForObjectValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- dateTimeStyle@
dateTimeStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO NSRelativeDateTimeFormatterStyle
dateTimeStyle nsRelativeDateTimeFormatter  =
  fmap (coerce :: CLong -> NSRelativeDateTimeFormatterStyle) $ sendMsg nsRelativeDateTimeFormatter (mkSelector "dateTimeStyle") retCLong []

-- | @- setDateTimeStyle:@
setDateTimeStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> NSRelativeDateTimeFormatterStyle -> IO ()
setDateTimeStyle nsRelativeDateTimeFormatter  value =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "setDateTimeStyle:") retVoid [argCLong (coerce value)]

-- | @- unitsStyle@
unitsStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO NSRelativeDateTimeFormatterUnitsStyle
unitsStyle nsRelativeDateTimeFormatter  =
  fmap (coerce :: CLong -> NSRelativeDateTimeFormatterUnitsStyle) $ sendMsg nsRelativeDateTimeFormatter (mkSelector "unitsStyle") retCLong []

-- | @- setUnitsStyle:@
setUnitsStyle :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> NSRelativeDateTimeFormatterUnitsStyle -> IO ()
setUnitsStyle nsRelativeDateTimeFormatter  value =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "setUnitsStyle:") retVoid [argCLong (coerce value)]

-- | @- formattingContext@
formattingContext :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO NSFormattingContext
formattingContext nsRelativeDateTimeFormatter  =
  fmap (coerce :: CLong -> NSFormattingContext) $ sendMsg nsRelativeDateTimeFormatter (mkSelector "formattingContext") retCLong []

-- | @- setFormattingContext:@
setFormattingContext :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsRelativeDateTimeFormatter  value =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "setFormattingContext:") retVoid [argCLong (coerce value)]

-- | @- calendar@
calendar :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO (Id NSCalendar)
calendar nsRelativeDateTimeFormatter  =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSCalendar value) => nsRelativeDateTimeFormatter -> value -> IO ()
setCalendar nsRelativeDateTimeFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRelativeDateTimeFormatter (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locale@
locale :: IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter => nsRelativeDateTimeFormatter -> IO (Id NSLocale)
locale nsRelativeDateTimeFormatter  =
  sendMsg nsRelativeDateTimeFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSRelativeDateTimeFormatter nsRelativeDateTimeFormatter, IsNSLocale value) => nsRelativeDateTimeFormatter -> value -> IO ()
setLocale nsRelativeDateTimeFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRelativeDateTimeFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringFromDateComponents:@
localizedStringFromDateComponentsSelector :: Selector
localizedStringFromDateComponentsSelector = mkSelector "localizedStringFromDateComponents:"

-- | @Selector@ for @localizedStringFromTimeInterval:@
localizedStringFromTimeIntervalSelector :: Selector
localizedStringFromTimeIntervalSelector = mkSelector "localizedStringFromTimeInterval:"

-- | @Selector@ for @localizedStringForDate:relativeToDate:@
localizedStringForDate_relativeToDateSelector :: Selector
localizedStringForDate_relativeToDateSelector = mkSelector "localizedStringForDate:relativeToDate:"

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @dateTimeStyle@
dateTimeStyleSelector :: Selector
dateTimeStyleSelector = mkSelector "dateTimeStyle"

-- | @Selector@ for @setDateTimeStyle:@
setDateTimeStyleSelector :: Selector
setDateTimeStyleSelector = mkSelector "setDateTimeStyle:"

-- | @Selector@ for @unitsStyle@
unitsStyleSelector :: Selector
unitsStyleSelector = mkSelector "unitsStyle"

-- | @Selector@ for @setUnitsStyle:@
setUnitsStyleSelector :: Selector
setUnitsStyleSelector = mkSelector "setUnitsStyle:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector
setFormattingContextSelector = mkSelector "setFormattingContext:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

