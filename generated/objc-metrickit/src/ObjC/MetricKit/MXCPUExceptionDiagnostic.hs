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

-- | callStackTree
--
-- The application call stack tree associated with the excessive CPU consumption.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXCPUExceptionDiagnostic mxcpuExceptionDiagnostic => mxcpuExceptionDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxcpuExceptionDiagnostic  =
  sendMsg mxcpuExceptionDiagnostic (mkSelector "callStackTree") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalCPUTime
--
-- Total CPU time consumed in the scope of this CPU exception.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- totalCPUTime@
totalCPUTime :: IsMXCPUExceptionDiagnostic mxcpuExceptionDiagnostic => mxcpuExceptionDiagnostic -> IO (Id NSMeasurement)
totalCPUTime mxcpuExceptionDiagnostic  =
  sendMsg mxcpuExceptionDiagnostic (mkSelector "totalCPUTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | totalSampledTime
--
-- Total time that the application was sampled for during the CPU exception.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- totalSampledTime@
totalSampledTime :: IsMXCPUExceptionDiagnostic mxcpuExceptionDiagnostic => mxcpuExceptionDiagnostic -> IO (Id NSMeasurement)
totalSampledTime mxcpuExceptionDiagnostic  =
  sendMsg mxcpuExceptionDiagnostic (mkSelector "totalSampledTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @totalCPUTime@
totalCPUTimeSelector :: Selector
totalCPUTimeSelector = mkSelector "totalCPUTime"

-- | @Selector@ for @totalSampledTime@
totalSampledTimeSelector :: Selector
totalSampledTimeSelector = mkSelector "totalSampledTime"

