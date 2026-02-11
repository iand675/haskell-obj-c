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
  , cumulativeCPUTimeSelector
  , cumulativeCPUInstructionsSelector


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
cumulativeCPUTime mxcpuMetric  =
  sendMsg mxcpuMetric (mkSelector "cumulativeCPUTime") (retPtr retVoid) [] >>= retainedObject . castPtr

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
cumulativeCPUInstructions mxcpuMetric  =
  sendMsg mxcpuMetric (mkSelector "cumulativeCPUInstructions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeCPUTime@
cumulativeCPUTimeSelector :: Selector
cumulativeCPUTimeSelector = mkSelector "cumulativeCPUTime"

-- | @Selector@ for @cumulativeCPUInstructions@
cumulativeCPUInstructionsSelector :: Selector
cumulativeCPUInstructionsSelector = mkSelector "cumulativeCPUInstructions"

