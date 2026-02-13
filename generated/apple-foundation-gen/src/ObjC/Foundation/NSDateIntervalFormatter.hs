{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , calendarSelector
  , dateStyleSelector
  , dateTemplateSelector
  , localeSelector
  , setCalendarSelector
  , setDateStyleSelector
  , setDateTemplateSelector
  , setLocaleSelector
  , setTimeStyleSelector
  , setTimeZoneSelector
  , stringFromDateIntervalSelector
  , stringFromDate_toDateSelector
  , timeStyleSelector
  , timeZoneSelector

  -- * Enum types
  , NSDateIntervalFormatterStyle(NSDateIntervalFormatterStyle)
  , pattern NSDateIntervalFormatterNoStyle
  , pattern NSDateIntervalFormatterShortStyle
  , pattern NSDateIntervalFormatterMediumStyle
  , pattern NSDateIntervalFormatterLongStyle
  , pattern NSDateIntervalFormatterFullStyle

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringFromDate:toDate:@
stringFromDate_toDate :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSDate fromDate, IsNSDate toDate) => nsDateIntervalFormatter -> fromDate -> toDate -> IO (Id NSString)
stringFromDate_toDate nsDateIntervalFormatter fromDate toDate =
  sendMessage nsDateIntervalFormatter stringFromDate_toDateSelector (toNSDate fromDate) (toNSDate toDate)

-- | @- stringFromDateInterval:@
stringFromDateInterval :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSDateInterval dateInterval) => nsDateIntervalFormatter -> dateInterval -> IO (Id NSString)
stringFromDateInterval nsDateIntervalFormatter dateInterval =
  sendMessage nsDateIntervalFormatter stringFromDateIntervalSelector (toNSDateInterval dateInterval)

-- | @- locale@
locale :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSLocale)
locale nsDateIntervalFormatter =
  sendMessage nsDateIntervalFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSLocale value) => nsDateIntervalFormatter -> value -> IO ()
setLocale nsDateIntervalFormatter value =
  sendMessage nsDateIntervalFormatter setLocaleSelector (toNSLocale value)

-- | @- calendar@
calendar :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSCalendar)
calendar nsDateIntervalFormatter =
  sendMessage nsDateIntervalFormatter calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSCalendar value) => nsDateIntervalFormatter -> value -> IO ()
setCalendar nsDateIntervalFormatter value =
  sendMessage nsDateIntervalFormatter setCalendarSelector (toNSCalendar value)

-- | @- timeZone@
timeZone :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSTimeZone)
timeZone nsDateIntervalFormatter =
  sendMessage nsDateIntervalFormatter timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSTimeZone value) => nsDateIntervalFormatter -> value -> IO ()
setTimeZone nsDateIntervalFormatter value =
  sendMessage nsDateIntervalFormatter setTimeZoneSelector (toNSTimeZone value)

-- | @- dateTemplate@
dateTemplate :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO (Id NSString)
dateTemplate nsDateIntervalFormatter =
  sendMessage nsDateIntervalFormatter dateTemplateSelector

-- | @- setDateTemplate:@
setDateTemplate :: (IsNSDateIntervalFormatter nsDateIntervalFormatter, IsNSString value) => nsDateIntervalFormatter -> value -> IO ()
setDateTemplate nsDateIntervalFormatter value =
  sendMessage nsDateIntervalFormatter setDateTemplateSelector (toNSString value)

-- | @- dateStyle@
dateStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO NSDateIntervalFormatterStyle
dateStyle nsDateIntervalFormatter =
  sendMessage nsDateIntervalFormatter dateStyleSelector

-- | @- setDateStyle:@
setDateStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> NSDateIntervalFormatterStyle -> IO ()
setDateStyle nsDateIntervalFormatter value =
  sendMessage nsDateIntervalFormatter setDateStyleSelector value

-- | @- timeStyle@
timeStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> IO NSDateIntervalFormatterStyle
timeStyle nsDateIntervalFormatter =
  sendMessage nsDateIntervalFormatter timeStyleSelector

-- | @- setTimeStyle:@
setTimeStyle :: IsNSDateIntervalFormatter nsDateIntervalFormatter => nsDateIntervalFormatter -> NSDateIntervalFormatterStyle -> IO ()
setTimeStyle nsDateIntervalFormatter value =
  sendMessage nsDateIntervalFormatter setTimeStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromDate:toDate:@
stringFromDate_toDateSelector :: Selector '[Id NSDate, Id NSDate] (Id NSString)
stringFromDate_toDateSelector = mkSelector "stringFromDate:toDate:"

-- | @Selector@ for @stringFromDateInterval:@
stringFromDateIntervalSelector :: Selector '[Id NSDateInterval] (Id NSString)
stringFromDateIntervalSelector = mkSelector "stringFromDateInterval:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id NSCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id NSCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @dateTemplate@
dateTemplateSelector :: Selector '[] (Id NSString)
dateTemplateSelector = mkSelector "dateTemplate"

-- | @Selector@ for @setDateTemplate:@
setDateTemplateSelector :: Selector '[Id NSString] ()
setDateTemplateSelector = mkSelector "setDateTemplate:"

-- | @Selector@ for @dateStyle@
dateStyleSelector :: Selector '[] NSDateIntervalFormatterStyle
dateStyleSelector = mkSelector "dateStyle"

-- | @Selector@ for @setDateStyle:@
setDateStyleSelector :: Selector '[NSDateIntervalFormatterStyle] ()
setDateStyleSelector = mkSelector "setDateStyle:"

-- | @Selector@ for @timeStyle@
timeStyleSelector :: Selector '[] NSDateIntervalFormatterStyle
timeStyleSelector = mkSelector "timeStyle"

-- | @Selector@ for @setTimeStyle:@
setTimeStyleSelector :: Selector '[NSDateIntervalFormatterStyle] ()
setTimeStyleSelector = mkSelector "setTimeStyle:"

