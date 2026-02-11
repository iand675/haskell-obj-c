{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDateIntervalFormatter@.
module ObjC.Foundation.NSDateIntervalFormatter
  ( NSDateIntervalFormatter
  , IsNSDateIntervalFormatter(..)
  , stringFromDate_toDate
  , stringFromDateInterval
  , locale
  , setLocale
  , calendar
  , setCalendar
  , timeZone
  , setTimeZone
  , dateTemplate
  , setDateTemplate
  , dateStyle
  , setDateStyle
  , timeStyle
  , setTimeStyle
  , stringFromDate_toDateSelector
  , stringFromDateIntervalSelector
  , localeSelector
  , setLocaleSelector
  , calendarSelector
  , setCalendarSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , dateTemplateSelector
  , setDateTemplateSelector
  , dateStyleSelector
  , setDateStyleSelector
  , timeStyleSelector
  , setTimeStyleSelector

  -- * Enum types
  , NSDateIntervalFormatterStyle(NSDateIntervalFormatterStyle)
  , pattern NSDateIntervalFormatterNoStyle
  , pattern NSDateIntervalFormatterShortStyle
  , pattern NSDateIntervalFormatterMediumStyle
  , pattern NSDateIntervalFormatterLongStyle
  , pattern NSDateIntervalFormatterFullStyle

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

-- | @- stringFromDate:toDate:@
stringFromDate_toDate :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSDate fromDate, IsNSDate toDate) => nsDateIntervalFormatter -> fromDate -> toDate -> IO (Id NSString)
stringFromDate_toDate nsDateIntervalFormatter  fromDate toDate =
withObjCPtr fromDate $ \raw_fromDate ->
  withObjCPtr toDate $ \raw_toDate ->
      sendMsg nsDateIntervalFormatter (mkSelector "stringFromDate:toDate:") (retPtr retVoid) [argPtr (castPtr raw_fromDate :: Ptr ()), argPtr (castPtr raw_toDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringFromDateInterval:@
stringFromDateInterval :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSDateInterval dateInterval) => nsDateIntervalFormatter -> dateInterval -> IO (Id NSString)
stringFromDateInterval nsDateIntervalFormatter  dateInterval =
withObjCPtr dateInterval $ \raw_dateInterval ->
    sendMsg nsDateIntervalFormatter (mkSelector "stringFromDateInterval:") (retPtr retVoid) [argPtr (castPtr raw_dateInterval :: Ptr ())] >>= retainedObject . castPtr

-- | @- locale@
locale :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSLocale)
locale nsDateIntervalFormatter  =
  sendMsg nsDateIntervalFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSLocale value) => nsDateIntervalFormatter -> value -> IO ()
setLocale nsDateIntervalFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateIntervalFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- calendar@
calendar :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSCalendar)
calendar nsDateIntervalFormatter  =
  sendMsg nsDateIntervalFormatter (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSCalendar value) => nsDateIntervalFormatter -> value -> IO ()
setCalendar nsDateIntervalFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateIntervalFormatter (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeZone@
timeZone :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSTimeZone)
timeZone nsDateIntervalFormatter  =
  sendMsg nsDateIntervalFormatter (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSTimeZone value) => nsDateIntervalFormatter -> value -> IO ()
setTimeZone nsDateIntervalFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateIntervalFormatter (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateTemplate@
dateTemplate :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSString)
dateTemplate nsDateIntervalFormatter  =
  sendMsg nsDateIntervalFormatter (mkSelector "dateTemplate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateTemplate:@
setDateTemplate :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSString value) => nsDateIntervalFormatter -> value -> IO ()
setDateTemplate nsDateIntervalFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateIntervalFormatter (mkSelector "setDateTemplate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateStyle@
dateStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO NSDateIntervalFormatterStyle
dateStyle nsDateIntervalFormatter  =
  fmap (coerce :: CULong -> NSDateIntervalFormatterStyle) $ sendMsg nsDateIntervalFormatter (mkSelector "dateStyle") retCULong []

-- | @- setDateStyle:@
setDateStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> NSDateIntervalFormatterStyle -> IO ()
setDateStyle nsDateIntervalFormatter  value =
  sendMsg nsDateIntervalFormatter (mkSelector "setDateStyle:") retVoid [argCULong (coerce value)]

-- | @- timeStyle@
timeStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO NSDateIntervalFormatterStyle
timeStyle nsDateIntervalFormatter  =
  fmap (coerce :: CULong -> NSDateIntervalFormatterStyle) $ sendMsg nsDateIntervalFormatter (mkSelector "timeStyle") retCULong []

-- | @- setTimeStyle:@
setTimeStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> NSDateIntervalFormatterStyle -> IO ()
setTimeStyle nsDateIntervalFormatter  value =
  sendMsg nsDateIntervalFormatter (mkSelector "setTimeStyle:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromDate:toDate:@
stringFromDate_toDateSelector :: Selector
stringFromDate_toDateSelector = mkSelector "stringFromDate:toDate:"

-- | @Selector@ for @stringFromDateInterval:@
stringFromDateIntervalSelector :: Selector
stringFromDateIntervalSelector = mkSelector "stringFromDateInterval:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @dateTemplate@
dateTemplateSelector :: Selector
dateTemplateSelector = mkSelector "dateTemplate"

-- | @Selector@ for @setDateTemplate:@
setDateTemplateSelector :: Selector
setDateTemplateSelector = mkSelector "setDateTemplate:"

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

