{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXSignpostIntervalData
--
-- A class that encapsulates metrics associated with app specific signpost intervals.
--
-- These metrics will be collected and aggregated if the associated signposts were emit using MXSignpost or MXSignpostAnimation APIs
--
-- To limit on-device overhead, the system will automatically limit the number of signposts (emitted using the MetricKit log handle) processed.
--
-- Avoid losing telemetry by limiting usage of signposts (emitted using the MetricKit log handle) to critical sections of code.
--
-- Generated bindings for @MXSignpostIntervalData@.
module ObjC.MetricKit.MXSignpostIntervalData
  ( MXSignpostIntervalData
  , IsMXSignpostIntervalData(..)
  , histogrammedSignpostDuration
  , cumulativeCPUTime
  , averageMemory
  , cumulativeLogicalWrites
  , cumulativeHitchTimeRatio
  , averageMemorySelector
  , cumulativeCPUTimeSelector
  , cumulativeHitchTimeRatioSelector
  , cumulativeLogicalWritesSelector
  , histogrammedSignpostDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | histogrammedSignpostDuration
--
-- A histogram of signpost intervals durations associated with the given signposts with signpostName and signpostCategory.
--
-- ObjC selector: @- histogrammedSignpostDuration@
histogrammedSignpostDuration :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id MXHistogram)
histogrammedSignpostDuration mxSignpostIntervalData =
  sendMessage mxSignpostIntervalData histogrammedSignpostDurationSelector

-- | cumulativeCPUTime
--
-- Cumulative CPU time aggregated over the MXSignpost intervals.
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- cumulativeCPUTime@
cumulativeCPUTime :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id NSMeasurement)
cumulativeCPUTime mxSignpostIntervalData =
  sendMessage mxSignpostIntervalData cumulativeCPUTimeSelector

-- | averageMemory
--
-- Average value of memory snapshots taken at beginning and end of MXSignpost intervals
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- averageMemory@
averageMemory :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id MXAverage)
averageMemory mxSignpostIntervalData =
  sendMessage mxSignpostIntervalData averageMemorySelector

-- | cumulativeLogicalWrites
--
-- Cumulative logical writes aggregated over the MXSignpost intervals.
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- cumulativeLogicalWrites@
cumulativeLogicalWrites :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id NSMeasurement)
cumulativeLogicalWrites mxSignpostIntervalData =
  sendMessage mxSignpostIntervalData cumulativeLogicalWritesSelector

-- | cumulativeHitchTimeRatio
--
-- Cumulative hitch time ratio aggregated over the MXSignpostAnimation intervals.
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- cumulativeHitchTimeRatio@
cumulativeHitchTimeRatio :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id NSMeasurement)
cumulativeHitchTimeRatio mxSignpostIntervalData =
  sendMessage mxSignpostIntervalData cumulativeHitchTimeRatioSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedSignpostDuration@
histogrammedSignpostDurationSelector :: Selector '[] (Id MXHistogram)
histogrammedSignpostDurationSelector = mkSelector "histogrammedSignpostDuration"

-- | @Selector@ for @cumulativeCPUTime@
cumulativeCPUTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeCPUTimeSelector = mkSelector "cumulativeCPUTime"

-- | @Selector@ for @averageMemory@
averageMemorySelector :: Selector '[] (Id MXAverage)
averageMemorySelector = mkSelector "averageMemory"

-- | @Selector@ for @cumulativeLogicalWrites@
cumulativeLogicalWritesSelector :: Selector '[] (Id NSMeasurement)
cumulativeLogicalWritesSelector = mkSelector "cumulativeLogicalWrites"

-- | @Selector@ for @cumulativeHitchTimeRatio@
cumulativeHitchTimeRatioSelector :: Selector '[] (Id NSMeasurement)
cumulativeHitchTimeRatioSelector = mkSelector "cumulativeHitchTimeRatio"

