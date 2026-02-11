{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXBackgroundExitData
--
-- A class that encapsulates cumulative application exit metrics when the application is off screen.
--
-- Background exits are terminations that, when unexpected, can impact performance metrics, such as launch time.
--
-- Not all background exits are unexpected. See the documentation for each exit reason for more information.
--
-- Generated bindings for @MXBackgroundExitData@.
module ObjC.MetricKit.MXBackgroundExitData
  ( MXBackgroundExitData
  , IsMXBackgroundExitData(..)
  , cumulativeNormalAppExitCount
  , cumulativeMemoryResourceLimitExitCount
  , cumulativeCPUResourceLimitExitCount
  , cumulativeMemoryPressureExitCount
  , cumulativeBadAccessExitCount
  , cumulativeAbnormalExitCount
  , cumulativeIllegalInstructionExitCount
  , cumulativeAppWatchdogExitCount
  , cumulativeSuspendedWithLockedFileExitCount
  , cumulativeBackgroundTaskAssertionTimeoutExitCount
  , cumulativeNormalAppExitCountSelector
  , cumulativeMemoryResourceLimitExitCountSelector
  , cumulativeCPUResourceLimitExitCountSelector
  , cumulativeMemoryPressureExitCountSelector
  , cumulativeBadAccessExitCountSelector
  , cumulativeAbnormalExitCountSelector
  , cumulativeIllegalInstructionExitCountSelector
  , cumulativeAppWatchdogExitCountSelector
  , cumulativeSuspendedWithLockedFileExitCountSelector
  , cumulativeBackgroundTaskAssertionTimeoutExitCountSelector


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

-- | cumulativeNormalAppExitCount
--
-- Cumulative number of times the application exited normally, or was gracefully terminated by the system.
--
-- ObjC selector: @- cumulativeNormalAppExitCount@
cumulativeNormalAppExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeNormalAppExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeNormalAppExitCount") retCULong []

-- | cumulativeMemoryResourceLimitExitCount
--
-- Cumulative number of times the application was terminated for exceeding a memory consumption limit.
--
-- ObjC selector: @- cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeMemoryResourceLimitExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeMemoryResourceLimitExitCount") retCULong []

-- | cumulativeCPUResourceLimitExitCount
--
-- Cumulative number of times the application was terminated for exceeding a CPU consumption limit.
--
-- ObjC selector: @- cumulativeCPUResourceLimitExitCount@
cumulativeCPUResourceLimitExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeCPUResourceLimitExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeCPUResourceLimitExitCount") retCULong []

-- | cumulativeMemoryPressureExitCount
--
-- Cumulative number of times the application exited due to memory pressure on the system.
--
-- ObjC selector: @- cumulativeMemoryPressureExitCount@
cumulativeMemoryPressureExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeMemoryPressureExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeMemoryPressureExitCount") retCULong []

-- | cumulativeBadAccessExitCount
--
-- Cumulative number of times the application was terminated for attempting to access invalid memory, or attempting to access memory in a manner not allowed by the memory's protection level (e.g. writing to read-only memory).
--
-- ObjC selector: @- cumulativeBadAccessExitCount@
cumulativeBadAccessExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeBadAccessExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeBadAccessExitCount") retCULong []

-- | cumulativeAbnormalExitCount
--
-- Cumulative number of times the application exited abnormally.
--
-- The most common causes of crashes with this exception type are uncaught Objective-C/C++ exceptions and calls to abort().
--
-- ObjC selector: @- cumulativeAbnormalExitCount@
cumulativeAbnormalExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeAbnormalExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeAbnormalExitCount") retCULong []

-- | cumulativeIllegalInstructionExitCount
--
-- Cumulative number of times the application terminated for attempting to execute an illegal or undefined instruction.
--
-- The process may have attempted to jump to an invalid address via a misconfigured function pointer.
--
-- ObjC selector: @- cumulativeIllegalInstructionExitCount@
cumulativeIllegalInstructionExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeIllegalInstructionExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeIllegalInstructionExitCount") retCULong []

-- | cumulativeAppWatchdogExitCount
--
-- Cumulative number of times the application was terminated because a watchdog timeout occured.
--
-- These can occur when the application took too long to launch, terminate, or respond to system events.
--
-- ObjC selector: @- cumulativeAppWatchdogExitCount@
cumulativeAppWatchdogExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeAppWatchdogExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeAppWatchdogExitCount") retCULong []

-- | cumulativeSuspendedWithLockedFileExitCount
--
-- Cumulative number of times the application was terminated because it became suspended while holding onto file locks or sqlite database locks.
--
-- If your application is performing operations on a locked file or sqlite database at suspension time, it must request additional background execution time to complete those operations and relinquish the lock before suspending.
--
-- ObjC selector: @- cumulativeSuspendedWithLockedFileExitCount@
cumulativeSuspendedWithLockedFileExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeSuspendedWithLockedFileExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeSuspendedWithLockedFileExitCount") retCULong []

-- | cumulativeBackgroundTaskAssertionTimeoutExitCount
--
-- Cumulative number of times the application was terminated for exceeding the alotted time limit associated with a background tasks.
--
-- If your application begins a background task, you must call endBackgroundTask() to signal completion of the task to prevent your application from being terminated. You can do this in the expiration handler of the task, but it must be done immediately.
--
-- ObjC selector: @- cumulativeBackgroundTaskAssertionTimeoutExitCount@
cumulativeBackgroundTaskAssertionTimeoutExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeBackgroundTaskAssertionTimeoutExitCount mxBackgroundExitData  =
  sendMsg mxBackgroundExitData (mkSelector "cumulativeBackgroundTaskAssertionTimeoutExitCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeNormalAppExitCount@
cumulativeNormalAppExitCountSelector :: Selector
cumulativeNormalAppExitCountSelector = mkSelector "cumulativeNormalAppExitCount"

-- | @Selector@ for @cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCountSelector :: Selector
cumulativeMemoryResourceLimitExitCountSelector = mkSelector "cumulativeMemoryResourceLimitExitCount"

-- | @Selector@ for @cumulativeCPUResourceLimitExitCount@
cumulativeCPUResourceLimitExitCountSelector :: Selector
cumulativeCPUResourceLimitExitCountSelector = mkSelector "cumulativeCPUResourceLimitExitCount"

-- | @Selector@ for @cumulativeMemoryPressureExitCount@
cumulativeMemoryPressureExitCountSelector :: Selector
cumulativeMemoryPressureExitCountSelector = mkSelector "cumulativeMemoryPressureExitCount"

-- | @Selector@ for @cumulativeBadAccessExitCount@
cumulativeBadAccessExitCountSelector :: Selector
cumulativeBadAccessExitCountSelector = mkSelector "cumulativeBadAccessExitCount"

-- | @Selector@ for @cumulativeAbnormalExitCount@
cumulativeAbnormalExitCountSelector :: Selector
cumulativeAbnormalExitCountSelector = mkSelector "cumulativeAbnormalExitCount"

-- | @Selector@ for @cumulativeIllegalInstructionExitCount@
cumulativeIllegalInstructionExitCountSelector :: Selector
cumulativeIllegalInstructionExitCountSelector = mkSelector "cumulativeIllegalInstructionExitCount"

-- | @Selector@ for @cumulativeAppWatchdogExitCount@
cumulativeAppWatchdogExitCountSelector :: Selector
cumulativeAppWatchdogExitCountSelector = mkSelector "cumulativeAppWatchdogExitCount"

-- | @Selector@ for @cumulativeSuspendedWithLockedFileExitCount@
cumulativeSuspendedWithLockedFileExitCountSelector :: Selector
cumulativeSuspendedWithLockedFileExitCountSelector = mkSelector "cumulativeSuspendedWithLockedFileExitCount"

-- | @Selector@ for @cumulativeBackgroundTaskAssertionTimeoutExitCount@
cumulativeBackgroundTaskAssertionTimeoutExitCountSelector :: Selector
cumulativeBackgroundTaskAssertionTimeoutExitCountSelector = mkSelector "cumulativeBackgroundTaskAssertionTimeoutExitCount"

