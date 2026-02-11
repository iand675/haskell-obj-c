{-# LANGUAGE PatternSynonyms #-}
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
  , datePickerStyleSelector
  , setDatePickerStyleSelector
  , bezeledSelector
  , setBezeledSelector
  , borderedSelector
  , setBorderedSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , textColorSelector
  , setTextColorSelector
  , datePickerModeSelector
  , setDatePickerModeSelector
  , datePickerElementsSelector
  , setDatePickerElementsSelector
  , calendarSelector
  , setCalendarSelector
  , localeSelector
  , setLocaleSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , dateValueSelector
  , setDateValueSelector
  , timeIntervalSelector
  , setTimeIntervalSelector
  , minDateSelector
  , setMinDateSelector
  , maxDateSelector
  , setMaxDateSelector
  , presentsCalendarOverlaySelector
  , setPresentsCalendarOverlaySelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- datePickerStyle@
datePickerStyle :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO NSDatePickerStyle
datePickerStyle nsDatePicker  =
  fmap (coerce :: CULong -> NSDatePickerStyle) $ sendMsg nsDatePicker (mkSelector "datePickerStyle") retCULong []

-- | @- setDatePickerStyle:@
setDatePickerStyle :: IsNSDatePicker nsDatePicker => nsDatePicker -> NSDatePickerStyle -> IO ()
setDatePickerStyle nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setDatePickerStyle:") retVoid [argCULong (coerce value)]

-- | @- bezeled@
bezeled :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
bezeled nsDatePicker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDatePicker (mkSelector "bezeled") retCULong []

-- | @- setBezeled:@
setBezeled :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setBezeled nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setBezeled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- bordered@
bordered :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
bordered nsDatePicker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDatePicker (mkSelector "bordered") retCULong []

-- | @- setBordered:@
setBordered :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setBordered nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- drawsBackground@
drawsBackground :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
drawsBackground nsDatePicker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDatePicker (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setDrawsBackground nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSColor)
backgroundColor nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSDatePicker nsDatePicker, IsNSColor value) => nsDatePicker -> value -> IO ()
setBackgroundColor nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textColor@
textColor :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSColor)
textColor nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "textColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextColor:@
setTextColor :: (IsNSDatePicker nsDatePicker, IsNSColor value) => nsDatePicker -> value -> IO ()
setTextColor nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setTextColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- datePickerMode@
datePickerMode :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO NSDatePickerMode
datePickerMode nsDatePicker  =
  fmap (coerce :: CULong -> NSDatePickerMode) $ sendMsg nsDatePicker (mkSelector "datePickerMode") retCULong []

-- | @- setDatePickerMode:@
setDatePickerMode :: IsNSDatePicker nsDatePicker => nsDatePicker -> NSDatePickerMode -> IO ()
setDatePickerMode nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setDatePickerMode:") retVoid [argCULong (coerce value)]

-- | @- datePickerElements@
datePickerElements :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO NSDatePickerElementFlags
datePickerElements nsDatePicker  =
  fmap (coerce :: CULong -> NSDatePickerElementFlags) $ sendMsg nsDatePicker (mkSelector "datePickerElements") retCULong []

-- | @- setDatePickerElements:@
setDatePickerElements :: IsNSDatePicker nsDatePicker => nsDatePicker -> NSDatePickerElementFlags -> IO ()
setDatePickerElements nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setDatePickerElements:") retVoid [argCULong (coerce value)]

-- | @- calendar@
calendar :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSCalendar)
calendar nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSDatePicker nsDatePicker, IsNSCalendar value) => nsDatePicker -> value -> IO ()
setCalendar nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locale@
locale :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSLocale)
locale nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSDatePicker nsDatePicker, IsNSLocale value) => nsDatePicker -> value -> IO ()
setLocale nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeZone@
timeZone :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSTimeZone)
timeZone nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSDatePicker nsDatePicker, IsNSTimeZone value) => nsDatePicker -> value -> IO ()
setTimeZone nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateValue@
dateValue :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSDate)
dateValue nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "dateValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateValue:@
setDateValue :: (IsNSDatePicker nsDatePicker, IsNSDate value) => nsDatePicker -> value -> IO ()
setDateValue nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setDateValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeInterval@
timeInterval :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO CDouble
timeInterval nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "timeInterval") retCDouble []

-- | @- setTimeInterval:@
setTimeInterval :: IsNSDatePicker nsDatePicker => nsDatePicker -> CDouble -> IO ()
setTimeInterval nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setTimeInterval:") retVoid [argCDouble (fromIntegral value)]

-- | @- minDate@
minDate :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSDate)
minDate nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "minDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinDate:@
setMinDate :: (IsNSDatePicker nsDatePicker, IsNSDate value) => nsDatePicker -> value -> IO ()
setMinDate nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setMinDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDate@
maxDate :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO (Id NSDate)
maxDate nsDatePicker  =
  sendMsg nsDatePicker (mkSelector "maxDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDate:@
setMaxDate :: (IsNSDatePicker nsDatePicker, IsNSDate value) => nsDatePicker -> value -> IO ()
setMaxDate nsDatePicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDatePicker (mkSelector "setMaxDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- presentsCalendarOverlay@
presentsCalendarOverlay :: IsNSDatePicker nsDatePicker => nsDatePicker -> IO Bool
presentsCalendarOverlay nsDatePicker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDatePicker (mkSelector "presentsCalendarOverlay") retCULong []

-- | @- setPresentsCalendarOverlay:@
setPresentsCalendarOverlay :: IsNSDatePicker nsDatePicker => nsDatePicker -> Bool -> IO ()
setPresentsCalendarOverlay nsDatePicker  value =
  sendMsg nsDatePicker (mkSelector "setPresentsCalendarOverlay:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @datePickerStyle@
datePickerStyleSelector :: Selector
datePickerStyleSelector = mkSelector "datePickerStyle"

-- | @Selector@ for @setDatePickerStyle:@
setDatePickerStyleSelector :: Selector
setDatePickerStyleSelector = mkSelector "setDatePickerStyle:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @textColor@
textColorSelector :: Selector
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @setTextColor:@
setTextColorSelector :: Selector
setTextColorSelector = mkSelector "setTextColor:"

-- | @Selector@ for @datePickerMode@
datePickerModeSelector :: Selector
datePickerModeSelector = mkSelector "datePickerMode"

-- | @Selector@ for @setDatePickerMode:@
setDatePickerModeSelector :: Selector
setDatePickerModeSelector = mkSelector "setDatePickerMode:"

-- | @Selector@ for @datePickerElements@
datePickerElementsSelector :: Selector
datePickerElementsSelector = mkSelector "datePickerElements"

-- | @Selector@ for @setDatePickerElements:@
setDatePickerElementsSelector :: Selector
setDatePickerElementsSelector = mkSelector "setDatePickerElements:"

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

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @dateValue@
dateValueSelector :: Selector
dateValueSelector = mkSelector "dateValue"

-- | @Selector@ for @setDateValue:@
setDateValueSelector :: Selector
setDateValueSelector = mkSelector "setDateValue:"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector
timeIntervalSelector = mkSelector "timeInterval"

-- | @Selector@ for @setTimeInterval:@
setTimeIntervalSelector :: Selector
setTimeIntervalSelector = mkSelector "setTimeInterval:"

-- | @Selector@ for @minDate@
minDateSelector :: Selector
minDateSelector = mkSelector "minDate"

-- | @Selector@ for @setMinDate:@
setMinDateSelector :: Selector
setMinDateSelector = mkSelector "setMinDate:"

-- | @Selector@ for @maxDate@
maxDateSelector :: Selector
maxDateSelector = mkSelector "maxDate"

-- | @Selector@ for @setMaxDate:@
setMaxDateSelector :: Selector
setMaxDateSelector = mkSelector "setMaxDate:"

-- | @Selector@ for @presentsCalendarOverlay@
presentsCalendarOverlaySelector :: Selector
presentsCalendarOverlaySelector = mkSelector "presentsCalendarOverlay"

-- | @Selector@ for @setPresentsCalendarOverlay:@
setPresentsCalendarOverlaySelector :: Selector
setPresentsCalendarOverlaySelector = mkSelector "setPresentsCalendarOverlay:"

