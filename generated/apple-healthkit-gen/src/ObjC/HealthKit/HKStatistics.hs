{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKStatistics
--
-- Represents statistics for quantity samples over a period of time.
--
-- Generated bindings for @HKStatistics@.
module ObjC.HealthKit.HKStatistics
  ( HKStatistics
  , IsHKStatistics(..)
  , init_
  , averageQuantityForSource
  , averageQuantity
  , minimumQuantityForSource
  , minimumQuantity
  , maximumQuantityForSource
  , maximumQuantity
  , mostRecentQuantityForSource
  , mostRecentQuantity
  , mostRecentQuantityDateIntervalForSource
  , mostRecentQuantityDateInterval
  , sumQuantityForSource
  , sumQuantity
  , duration
  , durationForSource
  , quantityType
  , startDate
  , endDate
  , sources
  , averageQuantityForSourceSelector
  , averageQuantitySelector
  , durationForSourceSelector
  , durationSelector
  , endDateSelector
  , initSelector
  , maximumQuantityForSourceSelector
  , maximumQuantitySelector
  , minimumQuantityForSourceSelector
  , minimumQuantitySelector
  , mostRecentQuantityDateIntervalForSourceSelector
  , mostRecentQuantityDateIntervalSelector
  , mostRecentQuantityForSourceSelector
  , mostRecentQuantitySelector
  , quantityTypeSelector
  , sourcesSelector
  , startDateSelector
  , sumQuantityForSourceSelector
  , sumQuantitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKStatistics)
init_ hkStatistics =
  sendOwnedMessage hkStatistics initSelector

-- | averageQuantityForSource:
--
-- Returns the average quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- averageQuantityForSource:@
averageQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
averageQuantityForSource hkStatistics source =
  sendMessage hkStatistics averageQuantityForSourceSelector (toHKSource source)

-- | averageQuantity
--
-- Returns the average quantity in the time period represented by the receiver.
--
-- ObjC selector: @- averageQuantity@
averageQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
averageQuantity hkStatistics =
  sendMessage hkStatistics averageQuantitySelector

-- | minimumQuantityForSource:
--
-- Returns the minimum quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- minimumQuantityForSource:@
minimumQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
minimumQuantityForSource hkStatistics source =
  sendMessage hkStatistics minimumQuantityForSourceSelector (toHKSource source)

-- | minimumQuantity
--
-- Returns the minimum quantity in the time period represented by the receiver.
--
-- ObjC selector: @- minimumQuantity@
minimumQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
minimumQuantity hkStatistics =
  sendMessage hkStatistics minimumQuantitySelector

-- | maximumQuantityForSource:
--
-- Returns the maximum quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- maximumQuantityForSource:@
maximumQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
maximumQuantityForSource hkStatistics source =
  sendMessage hkStatistics maximumQuantityForSourceSelector (toHKSource source)

-- | maximumQuantity
--
-- Returns the maximum quantity in the time period represented by the receiver.
--
-- ObjC selector: @- maximumQuantity@
maximumQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
maximumQuantity hkStatistics =
  sendMessage hkStatistics maximumQuantitySelector

-- | mostRecentQuantityForSource:
--
-- Returns the most recent quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- mostRecentQuantityForSource:@
mostRecentQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
mostRecentQuantityForSource hkStatistics source =
  sendMessage hkStatistics mostRecentQuantityForSourceSelector (toHKSource source)

-- | mostRecentQuantity
--
-- Returns the most recent quantity in the time period represented by the receiver.
--
-- ObjC selector: @- mostRecentQuantity@
mostRecentQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
mostRecentQuantity hkStatistics =
  sendMessage hkStatistics mostRecentQuantitySelector

-- | mostRecentQuantityDateIntervalForSource:
--
-- Returns the date interval of the most recent quantity for the given source in the time period                represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- mostRecentQuantityDateIntervalForSource:@
mostRecentQuantityDateIntervalForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id NSDateInterval)
mostRecentQuantityDateIntervalForSource hkStatistics source =
  sendMessage hkStatistics mostRecentQuantityDateIntervalForSourceSelector (toHKSource source)

-- | mostRecentQuantityDateInterval
--
-- Returns the date interval of the most recent quantity in the time period represented by the receiver.
--
-- ObjC selector: @- mostRecentQuantityDateInterval@
mostRecentQuantityDateInterval :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSDateInterval)
mostRecentQuantityDateInterval hkStatistics =
  sendMessage hkStatistics mostRecentQuantityDateIntervalSelector

-- | sumQuantityForSource:
--
-- Returns the sum quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- sumQuantityForSource:@
sumQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
sumQuantityForSource hkStatistics source =
  sendMessage hkStatistics sumQuantityForSourceSelector (toHKSource source)

-- | sumQuantity
--
-- Returns the sum of quantities in the time period represented by the receiver.
--
-- ObjC selector: @- sumQuantity@
sumQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
sumQuantity hkStatistics =
  sendMessage hkStatistics sumQuantitySelector

-- | Total duration (in seconds) covered by the samples represented by these statistics. Only present if HKStatisticsOptionDuration is is specified.
--
-- duration
--
-- Total duration, as a time-unit compatible quantity, covered by the samples represented by these statistics.
--
-- Only present if HKStatisticsOptionDuration is is specified.
--
-- ObjC selector: @- duration@
duration :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
duration hkStatistics =
  sendMessage hkStatistics durationSelector

-- | durationForSource:
--
-- Returns the duration, as a time-unit compatible quantity, for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- durationForSource:@
durationForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
durationForSource hkStatistics source =
  sendMessage hkStatistics durationForSourceSelector (toHKSource source)

-- | @- quantityType@
quantityType :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantityType)
quantityType hkStatistics =
  sendMessage hkStatistics quantityTypeSelector

-- | @- startDate@
startDate :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSDate)
startDate hkStatistics =
  sendMessage hkStatistics startDateSelector

-- | @- endDate@
endDate :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSDate)
endDate hkStatistics =
  sendMessage hkStatistics endDateSelector

-- | @- sources@
sources :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSArray)
sources hkStatistics =
  sendMessage hkStatistics sourcesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKStatistics)
initSelector = mkSelector "init"

-- | @Selector@ for @averageQuantityForSource:@
averageQuantityForSourceSelector :: Selector '[Id HKSource] (Id HKQuantity)
averageQuantityForSourceSelector = mkSelector "averageQuantityForSource:"

-- | @Selector@ for @averageQuantity@
averageQuantitySelector :: Selector '[] (Id HKQuantity)
averageQuantitySelector = mkSelector "averageQuantity"

-- | @Selector@ for @minimumQuantityForSource:@
minimumQuantityForSourceSelector :: Selector '[Id HKSource] (Id HKQuantity)
minimumQuantityForSourceSelector = mkSelector "minimumQuantityForSource:"

-- | @Selector@ for @minimumQuantity@
minimumQuantitySelector :: Selector '[] (Id HKQuantity)
minimumQuantitySelector = mkSelector "minimumQuantity"

-- | @Selector@ for @maximumQuantityForSource:@
maximumQuantityForSourceSelector :: Selector '[Id HKSource] (Id HKQuantity)
maximumQuantityForSourceSelector = mkSelector "maximumQuantityForSource:"

-- | @Selector@ for @maximumQuantity@
maximumQuantitySelector :: Selector '[] (Id HKQuantity)
maximumQuantitySelector = mkSelector "maximumQuantity"

-- | @Selector@ for @mostRecentQuantityForSource:@
mostRecentQuantityForSourceSelector :: Selector '[Id HKSource] (Id HKQuantity)
mostRecentQuantityForSourceSelector = mkSelector "mostRecentQuantityForSource:"

-- | @Selector@ for @mostRecentQuantity@
mostRecentQuantitySelector :: Selector '[] (Id HKQuantity)
mostRecentQuantitySelector = mkSelector "mostRecentQuantity"

-- | @Selector@ for @mostRecentQuantityDateIntervalForSource:@
mostRecentQuantityDateIntervalForSourceSelector :: Selector '[Id HKSource] (Id NSDateInterval)
mostRecentQuantityDateIntervalForSourceSelector = mkSelector "mostRecentQuantityDateIntervalForSource:"

-- | @Selector@ for @mostRecentQuantityDateInterval@
mostRecentQuantityDateIntervalSelector :: Selector '[] (Id NSDateInterval)
mostRecentQuantityDateIntervalSelector = mkSelector "mostRecentQuantityDateInterval"

-- | @Selector@ for @sumQuantityForSource:@
sumQuantityForSourceSelector :: Selector '[Id HKSource] (Id HKQuantity)
sumQuantityForSourceSelector = mkSelector "sumQuantityForSource:"

-- | @Selector@ for @sumQuantity@
sumQuantitySelector :: Selector '[] (Id HKQuantity)
sumQuantitySelector = mkSelector "sumQuantity"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id HKQuantity)
durationSelector = mkSelector "duration"

-- | @Selector@ for @durationForSource:@
durationForSourceSelector :: Selector '[Id HKSource] (Id HKQuantity)
durationForSourceSelector = mkSelector "durationForSource:"

-- | @Selector@ for @quantityType@
quantityTypeSelector :: Selector '[] (Id HKQuantityType)
quantityTypeSelector = mkSelector "quantityType"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @sources@
sourcesSelector :: Selector '[] (Id NSArray)
sourcesSelector = mkSelector "sources"

