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
histogrammedCellularConditionTime mxCellularConditionMetric  =
  sendMsg mxCellularConditionMetric (mkSelector "histogrammedCellularConditionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedCellularConditionTime@
histogrammedCellularConditionTimeSelector :: Selector
histogrammedCellularConditionTimeSelector = mkSelector "histogrammedCellularConditionTime"

