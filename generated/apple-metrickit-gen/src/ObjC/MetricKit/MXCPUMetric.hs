{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXCPUMetric
--
-- An MXMetric subclass that encapsulates CPU metrics.
--
-- Generated bindings for @MXCPUMetric@.
module ObjC.MetricKit.MXCPUMetric
  ( MXCPUMetric
  , IsMXCPUMetric(..)
  , cumulativeCPUTime
  , cumulativeCPUInstructions
  , cumulativeCPUInstructionsSelector
  , cumulativeCPUTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cumulativeCPUTime
--
-- CPU time aggregated cumulatively.
--
-- The data here represents the total CPU time an application consumed over the date range of the containing payload.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeCPUTime@
cumulativeCPUTime :: IsMXCPUMetric mxcpuMetric => mxcpuMetric -> IO (Id NSMeasurement)
cumulativeCPUTime mxcpuMetric =
  sendMessage mxcpuMetric cumulativeCPUTimeSelector

-- | cumulativeCPUInstructions
--
-- CPU instructions retired aggregated cumulatively.
--
-- The data here represents the total number of CPU instructions an application retired over the date range of the containing payload.
--
-- Dimensionless.
--
-- ObjC selector: @- cumulativeCPUInstructions@
cumulativeCPUInstructions :: IsMXCPUMetric mxcpuMetric => mxcpuMetric -> IO (Id NSMeasurement)
cumulativeCPUInstructions mxcpuMetric =
  sendMessage mxcpuMetric cumulativeCPUInstructionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeCPUTime@
cumulativeCPUTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeCPUTimeSelector = mkSelector "cumulativeCPUTime"

-- | @Selector@ for @cumulativeCPUInstructions@
cumulativeCPUInstructionsSelector :: Selector '[] (Id NSMeasurement)
cumulativeCPUInstructionsSelector = mkSelector "cumulativeCPUInstructions"

