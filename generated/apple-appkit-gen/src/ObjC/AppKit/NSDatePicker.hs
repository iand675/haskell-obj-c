{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDatePicker@.
module ObjC.AppKit.NSDatePicker
  ( NSDatePicker
  , IsNSDatePicker(..)
  , datePickerStyle
  , setDatePickerStyle
  , bezeled
  , setBezeled
  , bordered
  , setBordered
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
  , presentsCalendarOverlay
  , setPresentsCalendarOverlay
  , delegate
  , setDelegate
  , backgroundColorSelector
  , bezeledSelector
  , borderedSelector
  , calendarSelector
  , datePickerElementsSelector
  , datePickerModeSelector
  , datePickerStyleSelector
  , dateValueSelector
  , delegateSelector
  , drawsBackgroundSelector
  , localeSelector
  , maxDateSelector
  , minDateSelector
  , presentsCalendarOverlaySelector
  , setBackgroundColorSelector
  , setBezeledSelector
  , setBorderedSelector
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
  , setPresentsCalendarOverlaySelector
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

-- | @- datePickerStyle@
datePickerStyle :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO NSDatePickerStyle
datePickerStyle nsDatePicker =
  sendMessage nsDatePicker datePickerStyleSelector

-- | @- setDatePickerStyle:@
setDatePickerStyle :: IsNSDatePicker nsDatePicker => nsDatePicker -> NSDatePickerStyle -> IO ()
setDatePickerStyle nsDatePicker value =
  sendMessage nsDatePicker setDatePickerStyleSelector value

-- | @- bezeled@
bezeled :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
bezeled nsDatePicker =
  sendMessage nsDatePicker bezeledSelector

-- | @- setBezeled:@
setBezeled :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setBezeled nsDatePicker value =
  sendMessage nsDatePicker setBezeledSelector value

-- | @- bordered@
bordered :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
bordered nsDatePicker =
  sendMessage nsDatePicker borderedSelector

-- | @- setBordered:@
setBordered :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setBordered nsDatePicker value =
  sendMessage nsDatePicker setBorderedSelector value

-- | @- drawsBackground@
drawsBackground :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
drawsBackground nsDatePicker =
  sendMessage nsDatePicker drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setDrawsBackground nsDatePicker value =
  sendMessage nsDatePicker setDrawsBackgroundSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSColor)
backgroundColor nsDatePicker =
  sendMessage nsDatePicker backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSDatePicker nsDatePicker, IsNSColor value) => nsDatePicker -> value -> IO ()
setBackgroundColor nsDatePicker value =
  sendMessage nsDatePicker setBackgroundColorSelector (toNSColor value)

-- | @- textColor@
textColor :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSColor)
textColor nsDatePicker =
  sendMessage nsDatePicker textColorSelector

-- | @- setTextColor:@
setTextColor :: (IsNSDatePicker nsDatePicker, IsNSColor value) => nsDatePicker -> value -> IO ()
setTextColor nsDatePicker value =
  sendMessage nsDatePicker setTextColorSelector (toNSColor value)

-- | @- datePickerMode@
datePickerMode :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO NSDatePickerMode
datePickerMode nsDatePicker =
  sendMessage nsDatePicker datePickerModeSelector

-- | @- setDatePickerMode:@
setDatePickerMode :: IsNSDatePicker nsDatePicker => nsDatePicker -> NSDatePickerMode -> IO ()
setDatePickerMode nsDatePicker value =
  sendMessage nsDatePicker setDatePickerModeSelector value

-- | @- datePickerElements@
datePickerElements :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO NSDatePickerElementFlags
datePickerElements nsDatePicker =
  sendMessage nsDatePicker datePickerElementsSelector

-- | @- setDatePickerElements:@
setDatePickerElements :: IsNSDatePicker nsDatePicker => nsDatePicker -> NSDatePickerElementFlags -> IO ()
setDatePickerElements nsDatePicker value =
  sendMessage nsDatePicker setDatePickerElementsSelector value

-- | @- calendar@
calendar :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSCalendar)
calendar nsDatePicker =
  sendMessage nsDatePicker calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSDatePicker nsDatePicker, IsNSCalendar value) => nsDatePicker -> value -> IO ()
setCalendar nsDatePicker value =
  sendMessage nsDatePicker setCalendarSelector (toNSCalendar value)

-- | @- locale@
locale :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSLocale)
locale nsDatePicker =
  sendMessage nsDatePicker localeSelector

-- | @- setLocale:@
setLocale :: (IsNSDatePicker nsDatePicker, IsNSLocale value) => nsDatePicker -> value -> IO ()
setLocale nsDatePicker value =
  sendMessage nsDatePicker setLocaleSelector (toNSLocale value)

-- | @- timeZone@
timeZone :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSTimeZone)
timeZone nsDatePicker =
  sendMessage nsDatePicker timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSDatePicker nsDatePicker, IsNSTimeZone value) => nsDatePicker -> value -> IO ()
setTimeZone nsDatePicker value =
  sendMessage nsDatePicker setTimeZoneSelector (toNSTimeZone value)

-- | @- dateValue@
dateValue :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSDate)
dateValue nsDatePicker =
  sendMessage nsDatePicker dateValueSelector

-- | @- setDateValue:@
setDateValue :: (IsNSDatePicker nsDatePicker, IsNSDate value) => nsDatePicker -> value -> IO ()
setDateValue nsDatePicker value =
  sendMessage nsDatePicker setDateValueSelector (toNSDate value)

-- | @- timeInterval@
timeInterval :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO CDouble
timeInterval nsDatePicker =
  sendMessage nsDatePicker timeIntervalSelector

-- | @- setTimeInterval:@
setTimeInterval :: IsNSDatePicker nsDatePicker => nsDatePicker -> CDouble -> IO ()
setTimeInterval nsDatePicker value =
  sendMessage nsDatePicker setTimeIntervalSelector value

-- | @- minDate@
minDate :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSDate)
minDate nsDatePicker =
  sendMessage nsDatePicker minDateSelector

-- | @- setMinDate:@
setMinDate :: (IsNSDatePicker nsDatePicker, IsNSDate value) => nsDatePicker -> value -> IO ()
setMinDate nsDatePicker value =
  sendMessage nsDatePicker setMinDateSelector (toNSDate value)

-- | @- maxDate@
maxDate :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSDate)
maxDate nsDatePicker =
  sendMessage nsDatePicker maxDateSelector

-- | @- setMaxDate:@
setMaxDate :: (IsNSDatePicker nsDatePicker, IsNSDate value) => nsDatePicker -> value -> IO ()
setMaxDate nsDatePicker value =
  sendMessage nsDatePicker setMaxDateSelector (toNSDate value)

-- | @- presentsCalendarOverlay@
presentsCalendarOverlay :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
presentsCalendarOverlay nsDatePicker =
  sendMessage nsDatePicker presentsCalendarOverlaySelector

-- | @- setPresentsCalendarOverlay:@
setPresentsCalendarOverlay :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setPresentsCalendarOverlay nsDatePicker value =
  sendMessage nsDatePicker setPresentsCalendarOverlaySelector value

-- | @- delegate@
delegate :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO RawId
delegate nsDatePicker =
  sendMessage nsDatePicker delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSDatePicker nsDatePicker => nsDatePicker -> RawId -> IO ()
setDelegate nsDatePicker value =
  sendMessage nsDatePicker setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @datePickerStyle@
datePickerStyleSelector :: Selector '[] NSDatePickerStyle
datePickerStyleSelector = mkSelector "datePickerStyle"

-- | @Selector@ for @setDatePickerStyle:@
setDatePickerStyleSelector :: Selector '[NSDatePickerStyle] ()
setDatePickerStyleSelector = mkSelector "setDatePickerStyle:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector '[] Bool
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector '[Bool] ()
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector '[] Bool
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector '[Bool] ()
setBorderedSelector = mkSelector "setBordered:"

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

-- | @Selector@ for @presentsCalendarOverlay@
presentsCalendarOverlaySelector :: Selector '[] Bool
presentsCalendarOverlaySelector = mkSelector "presentsCalendarOverlay"

-- | @Selector@ for @setPresentsCalendarOverlay:@
setPresentsCalendarOverlaySelector :: Selector '[Bool] ()
setPresentsCalendarOverlaySelector = mkSelector "setPresentsCalendarOverlay:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

