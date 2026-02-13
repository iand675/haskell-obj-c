{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXCellConditionMetric
--
-- An MXMetric subclass that encapsulates cellular condition metrics.
--
-- Generated bindings for @MXCellularConditionMetric@.
module ObjC.MetricKit.MXCellularConditionMetric
  ( MXCellularConditionMetric
  , IsMXCellularConditionMetric(..)
  , histogrammedCellularConditionTime
  , histogrammedCellularConditionTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cellularConditionTime
--
-- Application run time bucketized by cellular condition.
--
-- This data represents the percentage of time an application spent running in different cellular conditions.
--
-- In the event that no data for any buckets is available, the histogram data will be empty.
--
-- Dimensioned as MXUnitSignalBars.
--
-- ObjC selector: @- histogrammedCellularConditionTime@
histogrammedCellularConditionTime :: IsMXCellularConditionMetric mxCellularConditionMetric => mxCellularConditionMetric -> IO (Id MXHistogram)
histogrammedCellularConditionTime mxCellularConditionMetric =
  sendMessage mxCellularConditionMetric histogrammedCellularConditionTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedCellularConditionTime@
histogrammedCellularConditionTimeSelector :: Selector '[] (Id MXHistogram)
histogrammedCellularConditionTimeSelector = mkSelector "histogrammedCellularConditionTime"

