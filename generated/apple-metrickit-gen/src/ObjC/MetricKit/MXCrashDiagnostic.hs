{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXCrashDiagnostic
--
-- An MXDiagnostic subclass that encapsulates crash reports.
--
-- See "Analyzing a Crash Report" for more information on crash diagnostics.
--
-- Generated bindings for @MXCrashDiagnostic@.
module ObjC.MetricKit.MXCrashDiagnostic
  ( MXCrashDiagnostic
  , IsMXCrashDiagnostic(..)
  , callStackTree
  , terminationReason
  , virtualMemoryRegionInfo
  , exceptionType
  , exceptionCode
  , signal
  , exceptionReason
  , callStackTreeSelector
  , exceptionCodeSelector
  , exceptionReasonSelector
  , exceptionTypeSelector
  , signalSelector
  , terminationReasonSelector
  , virtualMemoryRegionInfoSelector


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
-- The application call stack tree associated with this crash.
--
-- This call stack tree includes those stack frames present at the time of the crash.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic callStackTreeSelector

-- | terminationReason
--
-- The termination reason associated with this crash.
--
-- Exit reason information specified when a process is terminated. Key system components, both inside and outside of a process, will terminate the process upon encountering a fatal error (e.g. a bad code signature, a missing dependent library, or accessing privacy sensitive information without the proper entitlement).
--
-- ObjC selector: @- terminationReason@
terminationReason :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSString)
terminationReason mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic terminationReasonSelector

-- | virtualMemoryRegionInfo
--
-- Details about memory that the app incorrectly accessed in relation to other sections of the app’s virtual memory address space.
--
-- This property is set when a bad memory access crash occurs.
--
-- ObjC selector: @- virtualMemoryRegionInfo@
virtualMemoryRegionInfo :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSString)
virtualMemoryRegionInfo mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic virtualMemoryRegionInfoSelector

-- | exceptionType
--
-- The name of the Mach exception that terminated the app.
--
-- See: sys/exception_types.h
--
-- ObjC selector: @- exceptionType@
exceptionType :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSNumber)
exceptionType mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic exceptionTypeSelector

-- | exceptionCode
--
-- Processor specific information about the exception encoded into one or more 64-bit hexadecimal numbers
--
-- See: sys/exception_types.h
--
-- ObjC selector: @- exceptionCode@
exceptionCode :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSNumber)
exceptionCode mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic exceptionCodeSelector

-- | signal
--
-- The signal associated with this crash.
--
-- See: sys/signal.h
--
-- ObjC selector: @- signal@
signal :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSNumber)
signal mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic signalSelector

-- | exceptionReason
--
-- The MXCrashDiagnosticObjectiveCExceptionReason object associated with this crash.
--
-- See: <MetricKit/MXCrashDiagnosticObjectiveCExceptionReason.h>
--
-- ObjC selector: @- exceptionReason@
exceptionReason :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id MXCrashDiagnosticObjectiveCExceptionReason)
exceptionReason mxCrashDiagnostic =
  sendMessage mxCrashDiagnostic exceptionReasonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector '[] (Id MXCallStackTree)
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @terminationReason@
terminationReasonSelector :: Selector '[] (Id NSString)
terminationReasonSelector = mkSelector "terminationReason"

-- | @Selector@ for @virtualMemoryRegionInfo@
virtualMemoryRegionInfoSelector :: Selector '[] (Id NSString)
virtualMemoryRegionInfoSelector = mkSelector "virtualMemoryRegionInfo"

-- | @Selector@ for @exceptionType@
exceptionTypeSelector :: Selector '[] (Id NSNumber)
exceptionTypeSelector = mkSelector "exceptionType"

-- | @Selector@ for @exceptionCode@
exceptionCodeSelector :: Selector '[] (Id NSNumber)
exceptionCodeSelector = mkSelector "exceptionCode"

-- | @Selector@ for @signal@
signalSelector :: Selector '[] (Id NSNumber)
signalSelector = mkSelector "signal"

-- | @Selector@ for @exceptionReason@
exceptionReasonSelector :: Selector '[] (Id MXCrashDiagnosticObjectiveCExceptionReason)
exceptionReasonSelector = mkSelector "exceptionReason"

