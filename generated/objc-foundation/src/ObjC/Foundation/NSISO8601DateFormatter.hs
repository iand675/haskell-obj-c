{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , stringFromDateSelector
  , dateFromStringSelector
  , stringFromDate_timeZone_formatOptionsSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , formatOptionsSelector
  , setFormatOptionsSelector

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

-- | @- init@
init_ :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> IO (Id NSISO8601DateFormatter)
init_ nsisO8601DateFormatter  =
  sendMsg nsisO8601DateFormatter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- stringFromDate:@
stringFromDate :: (IsNSISO8601DateFormatter nsisO8601DateFormatter, IsNSDate date) => nsisO8601DateFormatter -> date -> IO (Id NSString)
stringFromDate nsisO8601DateFormatter  date =
withObjCPtr date $ \raw_date ->
    sendMsg nsisO8601DateFormatter (mkSelector "stringFromDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- dateFromString:@
dateFromString :: (IsNSISO8601DateFormatter nsisO8601DateFormatter, IsNSString string) => nsisO8601DateFormatter -> string -> IO (Id NSDate)
dateFromString nsisO8601DateFormatter  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsisO8601DateFormatter (mkSelector "dateFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ stringFromDate:timeZone:formatOptions:@
stringFromDate_timeZone_formatOptions :: (IsNSDate date, IsNSTimeZone timeZone) => date -> timeZone -> NSISO8601DateFormatOptions -> IO (Id NSString)
stringFromDate_timeZone_formatOptions date timeZone formatOptions =
  do
    cls' <- getRequiredClass "NSISO8601DateFormatter"
    withObjCPtr date $ \raw_date ->
      withObjCPtr timeZone $ \raw_timeZone ->
        sendClassMsg cls' (mkSelector "stringFromDate:timeZone:formatOptions:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_timeZone :: Ptr ()), argCULong (coerce formatOptions)] >>= retainedObject . castPtr

-- | @- timeZone@
timeZone :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> IO (Id NSTimeZone)
timeZone nsisO8601DateFormatter  =
  sendMsg nsisO8601DateFormatter (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeZone:@
setTimeZone :: (IsNSISO8601DateFormatter nsisO8601DateFormatter, IsNSTimeZone value) => nsisO8601DateFormatter -> value -> IO ()
setTimeZone nsisO8601DateFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsisO8601DateFormatter (mkSelector "setTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- formatOptions@
formatOptions :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> IO NSISO8601DateFormatOptions
formatOptions nsisO8601DateFormatter  =
  fmap (coerce :: CULong -> NSISO8601DateFormatOptions) $ sendMsg nsisO8601DateFormatter (mkSelector "formatOptions") retCULong []

-- | @- setFormatOptions:@
setFormatOptions :: IsNSISO8601DateFormatter nsisO8601DateFormatter => nsisO8601DateFormatter -> NSISO8601DateFormatOptions -> IO ()
setFormatOptions nsisO8601DateFormatter  value =
  sendMsg nsisO8601DateFormatter (mkSelector "setFormatOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @stringFromDate:@
stringFromDateSelector :: Selector
stringFromDateSelector = mkSelector "stringFromDate:"

-- | @Selector@ for @dateFromString:@
dateFromStringSelector :: Selector
dateFromStringSelector = mkSelector "dateFromString:"

-- | @Selector@ for @stringFromDate:timeZone:formatOptions:@
stringFromDate_timeZone_formatOptionsSelector :: Selector
stringFromDate_timeZone_formatOptionsSelector = mkSelector "stringFromDate:timeZone:formatOptions:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @formatOptions@
formatOptionsSelector :: Selector
formatOptionsSelector = mkSelector "formatOptions"

-- | @Selector@ for @setFormatOptions:@
setFormatOptionsSelector :: Selector
setFormatOptionsSelector = mkSelector "setFormatOptions:"

