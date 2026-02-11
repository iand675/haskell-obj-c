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
  , histogrammedSignpostDurationSelector
  , cumulativeCPUTimeSelector
  , averageMemorySelector
  , cumulativeLogicalWritesSelector
  , cumulativeHitchTimeRatioSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | histogrammedSignpostDuration
--
-- A histogram of signpost intervals durations associated with the given signposts with signpostName and signpostCategory.
--
-- ObjC selector: @- histogrammedSignpostDuration@
histogrammedSignpostDuration :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id MXHistogram)
histogrammedSignpostDuration mxSignpostIntervalData  =
  sendMsg mxSignpostIntervalData (mkSelector "histogrammedSignpostDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeCPUTime
--
-- Cumulative CPU time aggregated over the MXSignpost intervals.
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- cumulativeCPUTime@
cumulativeCPUTime :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id NSMeasurement)
cumulativeCPUTime mxSignpostIntervalData  =
  sendMsg mxSignpostIntervalData (mkSelector "cumulativeCPUTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | averageMemory
--
-- Average value of memory snapshots taken at beginning and end of MXSignpost intervals
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- averageMemory@
averageMemory :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id MXAverage)
averageMemory mxSignpostIntervalData  =
  sendMsg mxSignpostIntervalData (mkSelector "averageMemory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeLogicalWrites
--
-- Cumulative logical writes aggregated over the MXSignpost intervals.
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- cumulativeLogicalWrites@
cumulativeLogicalWrites :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id NSMeasurement)
cumulativeLogicalWrites mxSignpostIntervalData  =
  sendMsg mxSignpostIntervalData (mkSelector "cumulativeLogicalWrites") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeHitchTimeRatio
--
-- Cumulative hitch time ratio aggregated over the MXSignpostAnimation intervals.
--
-- This property is null when signposts with the associated signpostName and signpostCategory contain no interval metric data.
--
-- ObjC selector: @- cumulativeHitchTimeRatio@
cumulativeHitchTimeRatio :: IsMXSignpostIntervalData mxSignpostIntervalData => mxSignpostIntervalData -> IO (Id NSMeasurement)
cumulativeHitchTimeRatio mxSignpostIntervalData  =
  sendMsg mxSignpostIntervalData (mkSelector "cumulativeHitchTimeRatio") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedSignpostDuration@
histogrammedSignpostDurationSelector :: Selector
histogrammedSignpostDurationSelector = mkSelector "histogrammedSignpostDuration"

-- | @Selector@ for @cumulativeCPUTime@
cumulativeCPUTimeSelector :: Selector
cumulativeCPUTimeSelector = mkSelector "cumulativeCPUTime"

-- | @Selector@ for @averageMemory@
averageMemorySelector :: Selector
averageMemorySelector = mkSelector "averageMemory"

-- | @Selector@ for @cumulativeLogicalWrites@
cumulativeLogicalWritesSelector :: Selector
cumulativeLogicalWritesSelector = mkSelector "cumulativeLogicalWrites"

-- | @Selector@ for @cumulativeHitchTimeRatio@
cumulativeHitchTimeRatioSelector :: Selector
cumulativeHitchTimeRatioSelector = mkSelector "cumulativeHitchTimeRatio"

