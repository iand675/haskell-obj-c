{-# LANGUAGE PatternSynonyms #-}
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
  , initTextCellSelector
  , initWithCoderSelector
  , initImageCellSelector
  , datePickerStyleSelector
  , setDatePickerStyleSelector
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
  , delegateSelector
  , setDelegateSelector

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

-- | @- initTextCell:@
initTextCell :: (IsNSDatePickerCell nsDatePickerCell, IsNSString string) => nsDatePickerCell -> string -> IO (Id NSDatePickerCell)
initTextCell nsDatePickerCell  string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsDatePickerCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSDatePickerCell nsDatePickerCell, IsNSCoder coder) => nsDatePickerCell -> coder -> IO (Id NSDatePickerCell)
initWithCoder nsDatePickerCell  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsDatePickerCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSDatePickerCell nsDatePickerCell, IsNSImage image) => nsDatePickerCell -> image -> IO (Id NSDatePickerCell)
initImageCell nsDatePickerCell  image =
  withObjCPtr image $ \raw_image ->
      sendMsg nsDatePickerCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- datePickerStyle@
datePickerStyle :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO NSDatePickerStyle
datePickerStyle nsDatePickerCell  =
    fmap (coerce :: CULong -> NSDatePickerStyle) $ sendMsg nsDatePickerCell (mkSelector "datePickerStyle") retCULong []

-- | @- setDatePickerStyle:@
setDatePickerStyle :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> NSDatePickerStyle -> IO ()
setDatePickerStyle nsDatePickerCell  value =
    sendMsg nsDatePickerCell (mkSelector "setDatePickerStyle:") retVoid [argCULong (coerce value)]

-- | @- drawsBackground@
drawsBackground :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO Bool
drawsBackground nsDatePickerCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDatePickerCell (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> Bool -> IO ()
setDrawsBackground nsDatePickerCell  value =
    sendMsg nsDatePickerCell (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSColor)
backgroundColor nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSDatePickerCell nsDatePickerCell, IsNSColor value) => nsDatePickerCell -> value -> IO ()
setBackgroundColor nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textColor@
textColor :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSColor)
textColor nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "textColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextColor:@
setTextColor :: (IsNSDatePickerCell nsDatePickerCell, IsNSColor value) => nsDatePickerCell -> value -> IO ()
setTextColor nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setTextColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- datePickerMode@
datePickerMode :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO NSDatePickerMode
datePickerMode nsDatePickerCell  =
    fmap (coerce :: CULong -> NSDatePickerMode) $ sendMsg nsDatePickerCell (mkSelector "datePickerMode") retCULong []

-- | @- setDatePickerMode:@
setDatePickerMode :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> NSDatePickerMode -> IO ()
setDatePickerMode nsDatePickerCell  value =
    sendMsg nsDatePickerCell (mkSelector "setDatePickerMode:") retVoid [argCULong (coerce value)]

-- | @- datePickerElements@
datePickerElements :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO NSDatePickerElementFlags
datePickerElements nsDatePickerCell  =
    fmap (coerce :: CULong -> NSDatePickerElementFlags) $ sendMsg nsDatePickerCell (mkSelector "datePickerElements") retCULong []

-- | @- setDatePickerElements:@
setDatePickerElements :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> NSDatePickerElementFlags -> IO ()
setDatePickerElements nsDatePickerCell  value =
    sendMsg nsDatePickerCell (mkSelector "setDatePickerElements:") retVoid [argCULong (coerce value)]

-- | @- calendar@
calendar :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSCalendar)
calendar nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSDatePickerCell nsDatePickerCell, IsNSCalendar value) => nsDatePickerCell -> value -> IO ()
setCalendar nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locale@
locale :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSLocale)
locale nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSDatePickerCell nsDatePickerCell, IsNSLocale value) => nsDatePickerCell -> value -> IO ()
setLocale nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeZone@
timeZone :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSTimeZone)
timeZone nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSDatePickerCell nsDatePickerCell, IsNSTimeZone value) => nsDatePickerCell -> value -> IO ()
setTimeZone nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateValue@
dateValue :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSDate)
dateValue nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "dateValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateValue:@
setDateValue :: (IsNSDatePickerCell nsDatePickerCell, IsNSDate value) => nsDatePickerCell -> value -> IO ()
setDateValue nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setDateValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeInterval@
timeInterval :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO CDouble
timeInterval nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "timeInterval") retCDouble []

-- | @- setTimeInterval:@
setTimeInterval :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> CDouble -> IO ()
setTimeInterval nsDatePickerCell  value =
    sendMsg nsDatePickerCell (mkSelector "setTimeInterval:") retVoid [argCDouble value]

-- | @- minDate@
minDate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSDate)
minDate nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "minDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinDate:@
setMinDate :: (IsNSDatePickerCell nsDatePickerCell, IsNSDate value) => nsDatePickerCell -> value -> IO ()
setMinDate nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setMinDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDate@
maxDate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO (Id NSDate)
maxDate nsDatePickerCell  =
    sendMsg nsDatePickerCell (mkSelector "maxDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDate:@
setMaxDate :: (IsNSDatePickerCell nsDatePickerCell, IsNSDate value) => nsDatePickerCell -> value -> IO ()
setMaxDate nsDatePickerCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsDatePickerCell (mkSelector "setMaxDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> IO RawId
delegate nsDatePickerCell  =
    fmap (RawId . castPtr) $ sendMsg nsDatePickerCell (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSDatePickerCell nsDatePickerCell => nsDatePickerCell -> RawId -> IO ()
setDelegate nsDatePickerCell  value =
    sendMsg nsDatePickerCell (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @datePickerStyle@
datePickerStyleSelector :: Selector
datePickerStyleSelector = mkSelector "datePickerStyle"

-- | @Selector@ for @setDatePickerStyle:@
setDatePickerStyleSelector :: Selector
setDatePickerStyleSelector = mkSelector "setDatePickerStyle:"

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

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

