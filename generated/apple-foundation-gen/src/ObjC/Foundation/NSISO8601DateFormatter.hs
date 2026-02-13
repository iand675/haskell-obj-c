{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSISO8601DateFormatter@.
module ObjC.Foundation.NSISO8601DateFormatter
  ( NSISO8601DateFormatter
  , IsNSISO8601DateFormatter(..)
  , init_
  , stringFromDate
  , dateFromString
  , stringFromDate_timeZone_formatOptions
  , timeZone
  , setTimeZone
  , formatOptions
  , setFormatOptions
  , dateFromStringSelector
  , formatOptionsSelector
  , initSelector
  , setFormatOptionsSelector
  , setTimeZoneSelector
  , stringFromDateSelector
  , stringFromDate_timeZone_formatOptionsSelector
  , timeZoneSelector

  -- * Enum types
  , NSISO8601DateFormatOptions(NSISO8601DateFormatOptions)
  , pattern NSISO8601DateFormatWithYear
  , pattern NSISO8601DateFormatWithMonth
  , pattern NSISO8601DateFormatWithWeekOfYear
  , pattern NSISO8601DateFormatWithDay
  , pattern NSISO8601DateFormatWithTime
  , pattern NSISO8601DateFormatWithTimeZone
  , pattern NSISO8601DateFormatWithSpaceBetweenDateAndTime
  , pattern NSISO8601DateFormatWithDashSeparatorInDate
  , pattern NSISO8601DateFormatWithColonSeparatorInTime
  , pattern NSISO8601DateFormatWithColonSeparatorInTimeZone
  , pattern NSISO8601DateFormatWithFractionalSeconds
  , pattern NSISO8601DateFormatWithFullDate
  , pattern NSISO8601DateFormatWithFullTime
  , pattern NSISO8601DateFormatWithInternetDateTime

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> IO (Id NSISO8601DateFormatter)
init_ nsisO8601DateFormatter =
  sendOwnedMessage nsisO8601DateFormatter initSelector

-- | @- stringFromDate:@
stringFromDate :: (IsNSISO8601DateFormatter nsisO8601DateFormatter, IsNSDate date) => nsisO8601DateFormatter -> date -> IO (Id NSString)
stringFromDate nsisO8601DateFormatter date =
  sendMessage nsisO8601DateFormatter stringFromDateSelector (toNSDate date)

-- | @- dateFromString:@
dateFromString :: (IsNSISO8601DateFormatter nsisO8601DateFormatter, IsNSString string) => nsisO8601DateFormatter -> string -> IO (Id NSDate)
dateFromString nsisO8601DateFormatter string =
  sendMessage nsisO8601DateFormatter dateFromStringSelector (toNSString string)

-- | @+ stringFromDate:timeZone:formatOptions:@
stringFromDate_timeZone_formatOptions :: (IsNSDate date, IsNSTimeZone timeZone) => date -> timeZone -> NSISO8601DateFormatOptions -> IO (Id NSString)
stringFromDate_timeZone_formatOptions date timeZone formatOptions =
  do
    cls' <- getRequiredClass "NSISO8601DateFormatter"
    sendClassMessage cls' stringFromDate_timeZone_formatOptionsSelector (toNSDate date) (toNSTimeZone timeZone) formatOptions

-- | @- timeZone@
timeZone :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> IO (Id NSTimeZone)
timeZone nsisO8601DateFormatter =
  sendMessage nsisO8601DateFormatter timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsNSISO8601DateFormatter nsisO8601DateFormatter, IsNSTimeZone value) => nsisO8601DateFormatter -> value -> IO ()
setTimeZone nsisO8601DateFormatter value =
  sendMessage nsisO8601DateFormatter setTimeZoneSelector (toNSTimeZone value)

-- | @- formatOptions@
formatOptions :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> IO NSISO8601DateFormatOptions
formatOptions nsisO8601DateFormatter =
  sendMessage nsisO8601DateFormatter formatOptionsSelector

-- | @- setFormatOptions:@
setFormatOptions :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> NSISO8601DateFormatOptions -> IO ()
setFormatOptions nsisO8601DateFormatter value =
  sendMessage nsisO8601DateFormatter setFormatOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSISO8601DateFormatter)
initSelector = mkSelector "init"

-- | @Selector@ for @stringFromDate:@
stringFromDateSelector :: Selector '[Id NSDate] (Id NSString)
stringFromDateSelector = mkSelector "stringFromDate:"

-- | @Selector@ for @dateFromString:@
dateFromStringSelector :: Selector '[Id NSString] (Id NSDate)
dateFromStringSelector = mkSelector "dateFromString:"

-- | @Selector@ for @stringFromDate:timeZone:formatOptions:@
stringFromDate_timeZone_formatOptionsSelector :: Selector '[Id NSDate, Id NSTimeZone, NSISO8601DateFormatOptions] (Id NSString)
stringFromDate_timeZone_formatOptionsSelector = mkSelector "stringFromDate:timeZone:formatOptions:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @formatOptions@
formatOptionsSelector :: Selector '[] NSISO8601DateFormatOptions
formatOptionsSelector = mkSelector "formatOptions"

-- | @Selector@ for @setFormatOptions:@
setFormatOptionsSelector :: Selector '[NSISO8601DateFormatOptions] ()
setFormatOptionsSelector = mkSelector "setFormatOptions:"

