{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKStatisticsCollection@.
module ObjC.HealthKit.HKStatisticsCollection
  ( HKStatisticsCollection
  , IsHKStatisticsCollection(..)
  , init_
  , statisticsForDate
  , enumerateStatisticsFromDate_toDate_withBlock
  , statistics
  , sources
  , enumerateStatisticsFromDate_toDate_withBlockSelector
  , initSelector
  , sourcesSelector
  , statisticsForDateSelector
  , statisticsSelector


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
init_ :: IsHKStatisticsCollection hkStatisticsCollection => hkStatisticsCollection -> IO (Id HKStatisticsCollection)
init_ hkStatisticsCollection =
  sendOwnedMessage hkStatisticsCollection initSelector

-- | statisticsForDate:
--
-- Returns the statistics object that this date is inside of
--
-- If there are no samples for the given date, an HKStatistics instance with nil quantities will be returned.
--
-- ObjC selector: @- statisticsForDate:@
statisticsForDate :: (IsHKStatisticsCollection hkStatisticsCollection, IsNSDate date) => hkStatisticsCollection -> date -> IO (Id HKStatistics)
statisticsForDate hkStatisticsCollection date =
  sendMessage hkStatisticsCollection statisticsForDateSelector (toNSDate date)

-- | enumerateStatisticsFromDate:toDate:withBlock:
--
-- Enumerates all statistics objects from startDate to endDate.
--
-- Statistics objects will be enumerated in chronological order. If there are no samples for an interval                between the start and end date, then the HKStatistics object for that interval will have nil quantities.
--
-- ObjC selector: @- enumerateStatisticsFromDate:toDate:withBlock:@
enumerateStatisticsFromDate_toDate_withBlock :: (IsHKStatisticsCollection hkStatisticsCollection, IsNSDate startDate, IsNSDate endDate) => hkStatisticsCollection -> startDate -> endDate -> Ptr () -> IO ()
enumerateStatisticsFromDate_toDate_withBlock hkStatisticsCollection startDate endDate block =
  sendMessage hkStatisticsCollection enumerateStatisticsFromDate_toDate_withBlockSelector (toNSDate startDate) (toNSDate endDate) block

-- | statistics
--
-- Returns a copy of the populated statistics objects.
--
-- The statistics objects are ordered chronologically.
--
-- ObjC selector: @- statistics@
statistics :: IsHKStatisticsCollection hkStatisticsCollection => hkStatisticsCollection -> IO (Id NSArray)
statistics hkStatisticsCollection =
  sendMessage hkStatisticsCollection statisticsSelector

-- | sources
--
-- Returns all HKSources found in the contained HKStatistics objects.
--
-- Sources will be empty unless HKStatisticsOptionSeparateBySource is specified in the                HKStatisticsCollectionQuery options.
--
-- ObjC selector: @- sources@
sources :: IsHKStatisticsCollection hkStatisticsCollection => hkStatisticsCollection -> IO (Id NSSet)
sources hkStatisticsCollection =
  sendMessage hkStatisticsCollection sourcesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKStatisticsCollection)
initSelector = mkSelector "init"

-- | @Selector@ for @statisticsForDate:@
statisticsForDateSelector :: Selector '[Id NSDate] (Id HKStatistics)
statisticsForDateSelector = mkSelector "statisticsForDate:"

-- | @Selector@ for @enumerateStatisticsFromDate:toDate:withBlock:@
enumerateStatisticsFromDate_toDate_withBlockSelector :: Selector '[Id NSDate, Id NSDate, Ptr ()] ()
enumerateStatisticsFromDate_toDate_withBlockSelector = mkSelector "enumerateStatisticsFromDate:toDate:withBlock:"

-- | @Selector@ for @statistics@
statisticsSelector :: Selector '[] (Id NSArray)
statisticsSelector = mkSelector "statistics"

-- | @Selector@ for @sources@
sourcesSelector :: Selector '[] (Id NSSet)
sourcesSelector = mkSelector "sources"

