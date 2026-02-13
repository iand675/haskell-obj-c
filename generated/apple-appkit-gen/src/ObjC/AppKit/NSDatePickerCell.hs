{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDatePickerCell@.
module ObjC.AppKit.NSDatePickerCell
  ( NSDatePickerCell
  , IsNSDatePickerCell(..)
  , initTextCell
  , initWithCoder
  , initImageCell
  , datePickerStyle
  , setDatePickerStyle
  , drawsBackground
  , setDrawsBackground
  , backgroundColor
  , setBackgroundColor
  , textColor
  , setTextColor
  , datePickerMode
  , setDatePickerMode
  , datePickerElements
  , setDatePickerElements
  , calendar
  , setCalendar
  , locale
  , setLocale
  , timeZone
  , setTimeZone
  , dateValue
  , setDateValue
  , timeInterval
  , setTimeInterval
  , minDate
  , setMinDate
  , maxDate
  , setMaxDate
  , delegate
  , setDelegate
  , backgroundColorSelector
  , calendarSelector
  , datePickerElementsSelector
  , datePickerModeSelector
  , datePickerStyleSelector
  , dateValueSelector
  , delegateSelector
  , drawsBackgroundSelector
  , initImageCellSelector
  , initTextCellSelector
  , initWithCoderSelector
  , localeSelector
  , maxDateSelector
  , minDateSelector
  , setBackgroundColorSelector
  , setCalendarSelector
  , setDatePickerElementsSelector
  , setDatePickerModeSelector
  , setDatePickerStyleSelector
  , setDateValueSelector
  , setDelegateSelector
  , setDrawsBackgroundSelector
  , setLocaleSelector
  , setMaxDateSelector
  , setMinDateSelector
  , setTextColorSelector
  , setTimeIntervalSelector
  , setTimeZoneSelector
  , textColorSelector
  , timeIntervalSelector
  , timeZoneSelector

  -- * Enum types
  , NSDatePickerElementFlags(NSDatePickerElementFlags)
  , pattern NSDatePickerElementFlagHourMinute
  , pattern NSDatePickerElementFlagHourMinuteSecond
  , pattern NSDatePickerElementFlagTimeZone
  , pattern NSDatePickerElementFlagYearMonth
  , pattern NSDatePickerElementFlagYearMonthDay
  , pattern NSDatePickerElementFlagEra
  , NSDatePickerMode(NSDatePickerMode)
  , pattern NSDatePickerModeSingle
  , pattern NSDatePickerModeRange
  , NSDatePickerStyle(NSDatePickerStyle)
  , pattern NSDatePickerStyleTextFieldAndStepper
  , pattern NSDatePickerStyleClockAndCalendar
  , pattern NSDatePickerStyleTextField

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSDatePickerCell nsDatePickerCell, IsNSString string) => nsDatePickerCell -> string -> IO (Id NSDatePickerCell)
initTextCell nsDatePickerCell string =
  sendOwnedMessage nsDatePickerCell initTextCellSelector (toNSString string)

-- | @- initWithCoder:@
initWithCoder :: (IsNSDatePickerCell nsDatePickerCell, IsNSCoder coder) => nsDatePickerCell -> coder -> IO (Id NSDatePickerCell)
initWithCoder nsDatePickerCell coder =
  sendOwnedMessage nsDatePickerCell initWithCoderSelector (toNSCoder coder)

-- | @- initImageCell:@
initImageCell :: (IsNSDatePickerCell nsDatePickerCell, IsNSImage image) => nsDatePickerCell -> image -> IO (Id NSDatePickerCell)
initImageCell nsDatePickerCell image =
  sendOwnedMessage nsDatePickerCell initImageCellSelector (toNSImage image)

-- | @- datePickerStyle@
datePickerStyle :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO NSDatePickerStyle
datePickerStyle nsDatePickerCell =
  sendMessage nsDatePickerCell datePickerStyleSelector

-- | @- setDatePickerStyle:@
setDatePickerStyle :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> NSDatePickerStyle -> IO ()
setDatePickerStyle nsDatePickerCell value =
  sendMessage nsDatePickerCell setDatePickerStyleSelector value

-- | @- drawsBackground@
drawsBackground :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO Bool
drawsBackground nsDatePickerCell =
  sendMessage nsDatePickerCell drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> Bool -> IO ()
setDrawsBackground nsDatePickerCell value =
  sendMessage nsDatePickerCell setDrawsBackgroundSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSColor)
backgroundColor nsDatePickerCell =
  sendMessage nsDatePickerCell backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSDatePickerCell nsDatePickerCell, IsNSColor value) => nsDatePickerCell -> value -> IO ()
setBackgroundColor nsDatePickerCell value =
  sendMessage nsDatePickerCell setBackgroundColorSelector (toNSColor value)

-- | @- textColor@
textColor :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSColor)
textColor nsDatePickerCell =
  sendMessage nsDatePickerCell textColorSelector

-- | @- setTextColor:@
setTextColor :: (IsNSDatePickerCell nsDatePickerCell, IsNSColor value) => nsDatePickerCell -> value -> IO ()
setTextColor nsDatePickerCell value =
  sendMessage nsDatePickerCell setTextColorSelector (toNSColor value)

-- | @- datePickerMode@
datePickerMode :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO NSDatePickerMode
datePickerMode nsDatePickerCell =
  sendMessage nsDatePickerCell datePickerModeSelector

-- | @- setDatePickerMode:@
setDatePickerMode :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> NSDatePickerMode -> IO ()
setDatePickerMode nsDatePickerCell value =
  sendMessage nsDatePickerCell setDatePickerModeSelector value

-- | @- datePickerElements@
datePickerElements :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO NSDatePickerElementFlags
datePickerElements nsDatePickerCell =
  sendMessage nsDatePickerCell datePickerElementsSelector

-- | @- setDatePickerElements:@
setDatePickerElements :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> NSDatePickerElementFlags -> IO ()
setDatePickerElements nsDatePickerCell value =
  sendMessage nsDatePickerCell setDatePickerElementsSelector value

-- | @- calendar@
calendar :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSCalendar)
calendar nsDatePickerCell =
  sendMessage nsDatePickerCell calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSDatePickerCell nsDatePickerCell, IsNSCalendar value) => nsDatePickerCell -> value -> IO ()
setCalendar nsDatePickerCell value =
  sendMessage nsDatePickerCell setCalendarSelector (toNSCalendar value)

-- | @- locale@
locale :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSLocale)
locale nsDatePickerCell =
  sendMessage nsDatePickerCell localeSelector

-- | @- setLocale:@
setLocale :: (IsNSDatePickerCell nsDatePickerCell, IsNSLocale value) => nsDatePickerCell -> value -> IO ()
setLocale nsDatePickerCell value =
  sendMessage nsDatePickerCell setLocaleSelector (toNSLocale value)

-- | @- timeZone@
timeZone :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSTimeZone)
timeZone nsDatePickerCell =
  sendMessage nsDatePickerCell timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSDatePickerCell nsDatePickerCell, IsNSTimeZone value) => nsDatePickerCell -> value -> IO ()
setTimeZone nsDatePickerCell value =
  sendMessage nsDatePickerCell setTimeZoneSelector (toNSTimeZone value)

-- | @- dateValue@
dateValue :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSDate)
dateValue nsDatePickerCell =
  sendMessage nsDatePickerCell dateValueSelector

-- | @- setDateValue:@
setDateValue :: (IsNSDatePickerCell nsDatePickerCell, IsNSDate value) => nsDatePickerCell -> value -> IO ()
setDateValue nsDatePickerCell value =
  sendMessage nsDatePickerCell setDateValueSelector (toNSDate value)

-- | @- timeInterval@
timeInterval :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO CDouble
timeInterval nsDatePickerCell =
  sendMessage nsDatePickerCell timeIntervalSelector

-- | @- setTimeInterval:@
setTimeInterval :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> CDouble -> IO ()
setTimeInterval nsDatePickerCell value =
  sendMessage nsDatePickerCell setTimeIntervalSelector value

-- | @- minDate@
minDate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSDate)
minDate nsDatePickerCell =
  sendMessage nsDatePickerCell minDateSelector

-- | @- setMinDate:@
setMinDate :: (IsNSDatePickerCell nsDatePickerCell, IsNSDate value) => nsDatePickerCell -> value -> IO ()
setMinDate nsDatePickerCell value =
  sendMessage nsDatePickerCell setMinDateSelector (toNSDate value)

-- | @- maxDate@
maxDate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSDate)
maxDate nsDatePickerCell =
  sendMessage nsDatePickerCell maxDateSelector

-- | @- setMaxDate:@
setMaxDate :: (IsNSDatePickerCell nsDatePickerCell, IsNSDate value) => nsDatePickerCell -> value -> IO ()
setMaxDate nsDatePickerCell value =
  sendMessage nsDatePickerCell setMaxDateSelector (toNSDate value)

-- | @- delegate@
delegate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO RawId
delegate nsDatePickerCell =
  sendMessage nsDatePickerCell delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> RawId -> IO ()
setDelegate nsDatePickerCell value =
  sendMessage nsDatePickerCell setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSDatePickerCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSDatePickerCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSDatePickerCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @datePickerStyle@
datePickerStyleSelector :: Selector '[] NSDatePickerStyle
datePickerStyleSelector = mkSelector "datePickerStyle"

-- | @Selector@ for @setDatePickerStyle:@
setDatePickerStyleSelector :: Selector '[NSDatePickerStyle] ()
setDatePickerStyleSelector = mkSelector "setDatePickerStyle:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @textColor@
textColorSelector :: Selector '[] (Id NSColor)
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @setTextColor:@
setTextColorSelector :: Selector '[Id NSColor] ()
setTextColorSelector = mkSelector "setTextColor:"

-- | @Selector@ for @datePickerMode@
datePickerModeSelector :: Selector '[] NSDatePickerMode
datePickerModeSelector = mkSelector "datePickerMode"

-- | @Selector@ for @setDatePickerMode:@
setDatePickerModeSelector :: Selector '[NSDatePickerMode] ()
setDatePickerModeSelector = mkSelector "setDatePickerMode:"

-- | @Selector@ for @datePickerElements@
datePickerElementsSelector :: Selector '[] NSDatePickerElementFlags
datePickerElementsSelector = mkSelector "datePickerElements"

-- | @Selector@ for @setDatePickerElements:@
setDatePickerElementsSelector :: Selector '[NSDatePickerElementFlags] ()
setDatePickerElementsSelector = mkSelector "setDatePickerElements:"

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

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @dateValue@
dateValueSelector :: Selector '[] (Id NSDate)
dateValueSelector = mkSelector "dateValue"

-- | @Selector@ for @setDateValue:@
setDateValueSelector :: Selector '[Id NSDate] ()
setDateValueSelector = mkSelector "setDateValue:"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector '[] CDouble
timeIntervalSelector = mkSelector "timeInterval"

-- | @Selector@ for @setTimeInterval:@
setTimeIntervalSelector :: Selector '[CDouble] ()
setTimeIntervalSelector = mkSelector "setTimeInterval:"

-- | @Selector@ for @minDate@
minDateSelector :: Selector '[] (Id NSDate)
minDateSelector = mkSelector "minDate"

-- | @Selector@ for @setMinDate:@
setMinDateSelector :: Selector '[Id NSDate] ()
setMinDateSelector = mkSelector "setMinDate:"

-- | @Selector@ for @maxDate@
maxDateSelector :: Selector '[] (Id NSDate)
maxDateSelector = mkSelector "maxDate"

-- | @Selector@ for @setMaxDate:@
setMaxDateSelector :: Selector '[Id NSDate] ()
setMaxDateSelector = mkSelector "setMaxDate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

