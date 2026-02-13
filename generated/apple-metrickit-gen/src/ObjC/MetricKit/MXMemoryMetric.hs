{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXMemoryMetric
--
-- An MXMetric subclass that encapsulates memory metrics.
--
-- Generated bindings for @MXMemoryMetric@.
module ObjC.MetricKit.MXMemoryMetric
  ( MXMemoryMetric
  , IsMXMemoryMetric(..)
  , peakMemoryUsage
  , averageSuspendedMemory
  , averageSuspendedMemorySelector
  , peakMemoryUsageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | peakMemoryUsage
--
-- A single value representing the peak memory consumption of the application.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- peakMemoryUsage@
peakMemoryUsage :: IsMXMemoryMetric mxMemoryMetric => mxMemoryMetric -> IO (Id NSMeasurement)
peakMemoryUsage mxMemoryMetric =
  sendMessage mxMemoryMetric peakMemoryUsageSelector

-- | averageSuspendedMemory
--
-- Average memory of the application upon suspend.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- averageSuspendedMemory@
averageSuspendedMemory :: IsMXMemoryMetric mxMemoryMetric => mxMemoryMetric -> IO (Id MXAverage)
averageSuspendedMemory mxMemoryMetric =
  sendMessage mxMemoryMetric averageSuspendedMemorySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peakMemoryUsage@
peakMemoryUsageSelector :: Selector '[] (Id NSMeasurement)
peakMemoryUsageSelector = mkSelector "peakMemoryUsage"

-- | @Selector@ for @averageSuspendedMemory@
averageSuspendedMemorySelector :: Selector '[] (Id MXAverage)
averageSuspendedMemorySelector = mkSelector "averageSuspendedMemory"

