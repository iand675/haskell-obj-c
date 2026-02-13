{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXGPUMetric
--
-- An MXMetric subclass that encapsulates GPU metrics.
--
-- Generated bindings for @MXGPUMetric@.
module ObjC.MetricKit.MXGPUMetric
  ( MXGPUMetric
  , IsMXGPUMetric(..)
  , cumulativeGPUTime
  , cumulativeGPUTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cumulativeGPUTime
--
-- GPU time aggregated cumulatively.
--
-- The data here represents the total GPU time an application consumed over the date range of the containing payload.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeGPUTime@
cumulativeGPUTime :: IsMXGPUMetric mxgpuMetric => mxgpuMetric -> IO (Id NSMeasurement)
cumulativeGPUTime mxgpuMetric =
  sendMessage mxgpuMetric cumulativeGPUTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeGPUTime@
cumulativeGPUTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeGPUTimeSelector = mkSelector "cumulativeGPUTime"

