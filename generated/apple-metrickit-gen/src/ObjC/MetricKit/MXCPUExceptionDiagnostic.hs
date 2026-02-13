{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXCPUExceptionDiagnostic
--
-- An MXDiagnostic subclass that encapsulates CPU exception diagnostic reports.
--
-- CPU exceptions occur when your application consumes excessive CPU time in a short period of time.
--
-- CPU exceptions can be both fatal and non-fatal to your application.
--
-- Generated bindings for @MXCPUExceptionDiagnostic@.
module ObjC.MetricKit.MXCPUExceptionDiagnostic
  ( MXCPUExceptionDiagnostic
  , IsMXCPUExceptionDiagnostic(..)
  , callStackTree
  , totalCPUTime
  , totalSampledTime
  , callStackTreeSelector
  , totalCPUTimeSelector
  , totalSampledTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | callStackTree
--
-- The application call stack tree associated with the excessive CPU consumption.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXCPUExceptionDiagnostic mxcpuExceptionDiagnostic => mxcpuExceptionDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxcpuExceptionDiagnostic =
  sendMessage mxcpuExceptionDiagnostic callStackTreeSelector

-- | totalCPUTime
--
-- Total CPU time consumed in the scope of this CPU exception.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- totalCPUTime@
totalCPUTime :: IsMXCPUExceptionDiagnostic mxcpuExceptionDiagnostic => mxcpuExceptionDiagnostic -> IO (Id NSMeasurement)
totalCPUTime mxcpuExceptionDiagnostic =
  sendMessage mxcpuExceptionDiagnostic totalCPUTimeSelector

-- | totalSampledTime
--
-- Total time that the application was sampled for during the CPU exception.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- totalSampledTime@
totalSampledTime :: IsMXCPUExceptionDiagnostic mxcpuExceptionDiagnostic => mxcpuExceptionDiagnostic -> IO (Id NSMeasurement)
totalSampledTime mxcpuExceptionDiagnostic =
  sendMessage mxcpuExceptionDiagnostic totalSampledTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector '[] (Id MXCallStackTree)
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @totalCPUTime@
totalCPUTimeSelector :: Selector '[] (Id NSMeasurement)
totalCPUTimeSelector = mkSelector "totalCPUTime"

-- | @Selector@ for @totalSampledTime@
totalSampledTimeSelector :: Selector '[] (Id NSMeasurement)
totalSampledTimeSelector = mkSelector "totalSampledTime"

