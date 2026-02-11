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
  , terminationReasonSelector
  , virtualMemoryRegionInfoSelector
  , exceptionTypeSelector
  , exceptionCodeSelector
  , signalSelector
  , exceptionReasonSelector


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
-- The application call stack tree associated with this crash.
--
-- This call stack tree includes those stack frames present at the time of the crash.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "callStackTree") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | terminationReason
--
-- The termination reason associated with this crash.
--
-- Exit reason information specified when a process is terminated. Key system components, both inside and outside of a process, will terminate the process upon encountering a fatal error (e.g. a bad code signature, a missing dependent library, or accessing privacy sensitive information without the proper entitlement).
--
-- ObjC selector: @- terminationReason@
terminationReason :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSString)
terminationReason mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "terminationReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | virtualMemoryRegionInfo
--
-- Details about memory that the app incorrectly accessed in relation to other sections of the app’s virtual memory address space.
--
-- This property is set when a bad memory access crash occurs.
--
-- ObjC selector: @- virtualMemoryRegionInfo@
virtualMemoryRegionInfo :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSString)
virtualMemoryRegionInfo mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "virtualMemoryRegionInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exceptionType
--
-- The name of the Mach exception that terminated the app.
--
-- See: sys/exception_types.h
--
-- ObjC selector: @- exceptionType@
exceptionType :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSNumber)
exceptionType mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "exceptionType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exceptionCode
--
-- Processor specific information about the exception encoded into one or more 64-bit hexadecimal numbers
--
-- See: sys/exception_types.h
--
-- ObjC selector: @- exceptionCode@
exceptionCode :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSNumber)
exceptionCode mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "exceptionCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | signal
--
-- The signal associated with this crash.
--
-- See: sys/signal.h
--
-- ObjC selector: @- signal@
signal :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id NSNumber)
signal mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "signal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exceptionReason
--
-- The MXCrashDiagnosticObjectiveCExceptionReason object associated with this crash.
--
-- See: <MetricKit/MXCrashDiagnosticObjectiveCExceptionReason.h>
--
-- ObjC selector: @- exceptionReason@
exceptionReason :: IsMXCrashDiagnostic mxCrashDiagnostic => mxCrashDiagnostic -> IO (Id MXCrashDiagnosticObjectiveCExceptionReason)
exceptionReason mxCrashDiagnostic  =
  sendMsg mxCrashDiagnostic (mkSelector "exceptionReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @terminationReason@
terminationReasonSelector :: Selector
terminationReasonSelector = mkSelector "terminationReason"

-- | @Selector@ for @virtualMemoryRegionInfo@
virtualMemoryRegionInfoSelector :: Selector
virtualMemoryRegionInfoSelector = mkSelector "virtualMemoryRegionInfo"

-- | @Selector@ for @exceptionType@
exceptionTypeSelector :: Selector
exceptionTypeSelector = mkSelector "exceptionType"

-- | @Selector@ for @exceptionCode@
exceptionCodeSelector :: Selector
exceptionCodeSelector = mkSelector "exceptionCode"

-- | @Selector@ for @signal@
signalSelector :: Selector
signalSelector = mkSelector "signal"

-- | @Selector@ for @exceptionReason@
exceptionReasonSelector :: Selector
exceptionReasonSelector = mkSelector "exceptionReason"

