{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , endDateSelector
  , intervalCountSelector
  , intervalUnitSelector
  , setEndDateSelector
  , setIntervalCountSelector
  , setIntervalUnitSelector
  , setStartDateSelector
  , startDateSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- startDate@
startDate :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO (Id NSDate)
startDate pkRecurringPaymentSummaryItem =
  sendMessage pkRecurringPaymentSummaryItem startDateSelector

-- | @- setStartDate:@
setStartDate :: (IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem, IsNSDate value) => pkRecurringPaymentSummaryItem -> value -> IO ()
setStartDate pkRecurringPaymentSummaryItem value =
  sendMessage pkRecurringPaymentSummaryItem setStartDateSelector (toNSDate value)

-- | @- intervalUnit@
intervalUnit :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO NSCalendarUnit
intervalUnit pkRecurringPaymentSummaryItem =
  sendMessage pkRecurringPaymentSummaryItem intervalUnitSelector

-- | @- setIntervalUnit:@
setIntervalUnit :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> NSCalendarUnit -> IO ()
setIntervalUnit pkRecurringPaymentSummaryItem value =
  sendMessage pkRecurringPaymentSummaryItem setIntervalUnitSelector value

-- | @- intervalCount@
intervalCount :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO CLong
intervalCount pkRecurringPaymentSummaryItem =
  sendMessage pkRecurringPaymentSummaryItem intervalCountSelector

-- | @- setIntervalCount:@
setIntervalCount :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> CLong -> IO ()
setIntervalCount pkRecurringPaymentSummaryItem value =
  sendMessage pkRecurringPaymentSummaryItem setIntervalCountSelector value

-- | @- endDate@
endDate :: IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem => pkRecurringPaymentSummaryItem -> IO (Id NSDate)
endDate pkRecurringPaymentSummaryItem =
  sendMessage pkRecurringPaymentSummaryItem endDateSelector

-- | @- setEndDate:@
setEndDate :: (IsPKRecurringPaymentSummaryItem pkRecurringPaymentSummaryItem, IsNSDate value) => pkRecurringPaymentSummaryItem -> value -> IO ()
setEndDate pkRecurringPaymentSummaryItem value =
  sendMessage pkRecurringPaymentSummaryItem setEndDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector '[Id NSDate] ()
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @intervalUnit@
intervalUnitSelector :: Selector '[] NSCalendarUnit
intervalUnitSelector = mkSelector "intervalUnit"

-- | @Selector@ for @setIntervalUnit:@
setIntervalUnitSelector :: Selector '[NSCalendarUnit] ()
setIntervalUnitSelector = mkSelector "setIntervalUnit:"

-- | @Selector@ for @intervalCount@
intervalCountSelector :: Selector '[] CLong
intervalCountSelector = mkSelector "intervalCount"

-- | @Selector@ for @setIntervalCount:@
setIntervalCountSelector :: Selector '[CLong] ()
setIntervalCountSelector = mkSelector "setIntervalCount:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector '[Id NSDate] ()
setEndDateSelector = mkSelector "setEndDate:"

