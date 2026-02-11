{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKRecurringPaymentSummaryItem@.
module ObjC.PassKit.PKRecurringPaymentSummaryItem
  ( PKRecurringPaymentSummaryItem
  , IsPKRecurringPaymentSummaryItem(..)
  , startDate
  , setStartDate
  , intervalUnit
  , setIntervalUnit
  , intervalCount
  , setIntervalCount
  , endDate
  , setEndDate
  , startDateSelector
  , setStartDateSelector
  , intervalUnitSelector
  , setIntervalUnitSelector
  , intervalCountSelector
  , setIntervalCountSelector
  , endDateSelector
  , setEndDateSelector

  -- * Enum types
  , NSCalendarUnit(NSCalendarUnit)
  , pattern NSCalendarUnitEra
  , pattern NSCalendarUnitYear
  , pattern NSCalendarUnitMonth
  , pattern NSCalendarUnitDay
  , pattern NSCalendarUnitHour
  , pattern NSCalendarUnitMinute
  , pattern NSCalendarUnitSecond
  , pattern NSCalendarUnitWeekday
  , pattern NSCalendarUnitWeekdayOrdinal
  , pattern NSCalendarUnitQuarter
  , pattern NSCalendarUnitWeekOfMonth
  , pattern NSCalendarUnitWeekOfYear
  , pattern NSCalendarUnitYearForWeekOfYear
  , pattern NSCalendarUnitNanosecond
  , pattern NSCalendarUnitDayOfYear
  , pattern NSCalendarUnitCalendar
  , pattern NSCalendarUnitTimeZone
  , pattern NSCalendarUnitIsLeapMonth
  , pattern NSCalendarUnitIsRepeatedDay
  , pattern NSEraCalendarUnit
  , pattern NSYearCalendarUnit
  , pattern NSMonthCalendarUnit
  , pattern NSDayCalendarUnit
  , pattern NSHourCalendarUnit
  , pattern NSMinuteCalendarUnit
  , pattern NSSecondCalendarUnit
  , pattern NSWeekCalendarUnit
  , pattern NSWeekdayCalendarUnit
  , pattern NSWeekdayOrdinalCalendarUnit
  , pattern NSQuarterCalendarUnit
  , pattern NSWeekOfMonthCalendarUnit
  , pattern NSWeekOfYearCalendarUnit
  , pattern NSYearForWeekOfYearCalendarUnit
  , pattern NSCalendarCalendarUnit
  , pattern NSTimeZoneCalendarUnit

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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- startDate@
startDate :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO (Id NSDate)
startDate pkRecurringPaymentSummaryItem  =
  sendMsg pkRecurringPaymentSummaryItem (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartDate:@
setStartDate :: (IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem, IsNSDate value) => pkRecurringPaymentSummaryItem -> value -> IO ()
setStartDate pkRecurringPaymentSummaryItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentSummaryItem (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- intervalUnit@
intervalUnit :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO NSCalendarUnit
intervalUnit pkRecurringPaymentSummaryItem  =
  fmap (coerce :: CULong -> NSCalendarUnit) $ sendMsg pkRecurringPaymentSummaryItem (mkSelector "intervalUnit") retCULong []

-- | @- setIntervalUnit:@
setIntervalUnit :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> NSCalendarUnit -> IO ()
setIntervalUnit pkRecurringPaymentSummaryItem  value =
  sendMsg pkRecurringPaymentSummaryItem (mkSelector "setIntervalUnit:") retVoid [argCULong (coerce value)]

-- | @- intervalCount@
intervalCount :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO CLong
intervalCount pkRecurringPaymentSummaryItem  =
  sendMsg pkRecurringPaymentSummaryItem (mkSelector "intervalCount") retCLong []

-- | @- setIntervalCount:@
setIntervalCount :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> CLong -> IO ()
setIntervalCount pkRecurringPaymentSummaryItem  value =
  sendMsg pkRecurringPaymentSummaryItem (mkSelector "setIntervalCount:") retVoid [argCLong (fromIntegral value)]

-- | @- endDate@
endDate :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO (Id NSDate)
endDate pkRecurringPaymentSummaryItem  =
  sendMsg pkRecurringPaymentSummaryItem (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndDate:@
setEndDate :: (IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem, IsNSDate value) => pkRecurringPaymentSummaryItem -> value -> IO ()
setEndDate pkRecurringPaymentSummaryItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkRecurringPaymentSummaryItem (mkSelector "setEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @intervalUnit@
intervalUnitSelector :: Selector
intervalUnitSelector = mkSelector "intervalUnit"

-- | @Selector@ for @setIntervalUnit:@
setIntervalUnitSelector :: Selector
setIntervalUnitSelector = mkSelector "setIntervalUnit:"

-- | @Selector@ for @intervalCount@
intervalCountSelector :: Selector
intervalCountSelector = mkSelector "intervalCount"

-- | @Selector@ for @setIntervalCount:@
setIntervalCountSelector :: Selector
setIntervalCountSelector = mkSelector "setIntervalCount:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector
setEndDateSelector = mkSelector "setEndDate:"

