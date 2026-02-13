{-# LANGUAGE DataKinds #-}
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
  , cumulativeAbnormalExitCountSelector
  , cumulativeAppWatchdogExitCountSelector
  , cumulativeBackgroundTaskAssertionTimeoutExitCountSelector
  , cumulativeBadAccessExitCountSelector
  , cumulativeCPUResourceLimitExitCountSelector
  , cumulativeIllegalInstructionExitCountSelector
  , cumulativeMemoryPressureExitCountSelector
  , cumulativeMemoryResourceLimitExitCountSelector
  , cumulativeNormalAppExitCountSelector
  , cumulativeSuspendedWithLockedFileExitCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
cumulativeNormalAppExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeNormalAppExitCountSelector

-- | cumulativeMemoryResourceLimitExitCount
--
-- Cumulative number of times the application was terminated for exceeding a memory consumption limit.
--
-- ObjC selector: @- cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeMemoryResourceLimitExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeMemoryResourceLimitExitCountSelector

-- | cumulativeCPUResourceLimitExitCount
--
-- Cumulative number of times the application was terminated for exceeding a CPU consumption limit.
--
-- ObjC selector: @- cumulativeCPUResourceLimitExitCount@
cumulativeCPUResourceLimitExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeCPUResourceLimitExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeCPUResourceLimitExitCountSelector

-- | cumulativeMemoryPressureExitCount
--
-- Cumulative number of times the application exited due to memory pressure on the system.
--
-- ObjC selector: @- cumulativeMemoryPressureExitCount@
cumulativeMemoryPressureExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeMemoryPressureExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeMemoryPressureExitCountSelector

-- | cumulativeBadAccessExitCount
--
-- Cumulative number of times the application was terminated for attempting to access invalid memory, or attempting to access memory in a manner not allowed by the memory's protection level (e.g. writing to read-only memory).
--
-- ObjC selector: @- cumulativeBadAccessExitCount@
cumulativeBadAccessExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeBadAccessExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeBadAccessExitCountSelector

-- | cumulativeAbnormalExitCount
--
-- Cumulative number of times the application exited abnormally.
--
-- The most common causes of crashes with this exception type are uncaught Objective-C/C++ exceptions and calls to abort().
--
-- ObjC selector: @- cumulativeAbnormalExitCount@
cumulativeAbnormalExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeAbnormalExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeAbnormalExitCountSelector

-- | cumulativeIllegalInstructionExitCount
--
-- Cumulative number of times the application terminated for attempting to execute an illegal or undefined instruction.
--
-- The process may have attempted to jump to an invalid address via a misconfigured function pointer.
--
-- ObjC selector: @- cumulativeIllegalInstructionExitCount@
cumulativeIllegalInstructionExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeIllegalInstructionExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeIllegalInstructionExitCountSelector

-- | cumulativeAppWatchdogExitCount
--
-- Cumulative number of times the application was terminated because a watchdog timeout occured.
--
-- These can occur when the application took too long to launch, terminate, or respond to system events.
--
-- ObjC selector: @- cumulativeAppWatchdogExitCount@
cumulativeAppWatchdogExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeAppWatchdogExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeAppWatchdogExitCountSelector

-- | cumulativeSuspendedWithLockedFileExitCount
--
-- Cumulative number of times the application was terminated because it became suspended while holding onto file locks or sqlite database locks.
--
-- If your application is performing operations on a locked file or sqlite database at suspension time, it must request additional background execution time to complete those operations and relinquish the lock before suspending.
--
-- ObjC selector: @- cumulativeSuspendedWithLockedFileExitCount@
cumulativeSuspendedWithLockedFileExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeSuspendedWithLockedFileExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeSuspendedWithLockedFileExitCountSelector

-- | cumulativeBackgroundTaskAssertionTimeoutExitCount
--
-- Cumulative number of times the application was terminated for exceeding the alotted time limit associated with a background tasks.
--
-- If your application begins a background task, you must call endBackgroundTask() to signal completion of the task to prevent your application from being terminated. You can do this in the expiration handler of the task, but it must be done immediately.
--
-- ObjC selector: @- cumulativeBackgroundTaskAssertionTimeoutExitCount@
cumulativeBackgroundTaskAssertionTimeoutExitCount :: IsMXBackgroundExitData mxBackgroundExitData => mxBackgroundExitData -> IO CULong
cumulativeBackgroundTaskAssertionTimeoutExitCount mxBackgroundExitData =
  sendMessage mxBackgroundExitData cumulativeBackgroundTaskAssertionTimeoutExitCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeNormalAppExitCount@
cumulativeNormalAppExitCountSelector :: Selector '[] CULong
cumulativeNormalAppExitCountSelector = mkSelector "cumulativeNormalAppExitCount"

-- | @Selector@ for @cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCountSelector :: Selector '[] CULong
cumulativeMemoryResourceLimitExitCountSelector = mkSelector "cumulativeMemoryResourceLimitExitCount"

-- | @Selector@ for @cumulativeCPUResourceLimitExitCount@
cumulativeCPUResourceLimitExitCountSelector :: Selector '[] CULong
cumulativeCPUResourceLimitExitCountSelector = mkSelector "cumulativeCPUResourceLimitExitCount"

-- | @Selector@ for @cumulativeMemoryPressureExitCount@
cumulativeMemoryPressureExitCountSelector :: Selector '[] CULong
cumulativeMemoryPressureExitCountSelector = mkSelector "cumulativeMemoryPressureExitCount"

-- | @Selector@ for @cumulativeBadAccessExitCount@
cumulativeBadAccessExitCountSelector :: Selector '[] CULong
cumulativeBadAccessExitCountSelector = mkSelector "cumulativeBadAccessExitCount"

-- | @Selector@ for @cumulativeAbnormalExitCount@
cumulativeAbnormalExitCountSelector :: Selector '[] CULong
cumulativeAbnormalExitCountSelector = mkSelector "cumulativeAbnormalExitCount"

-- | @Selector@ for @cumulativeIllegalInstructionExitCount@
cumulativeIllegalInstructionExitCountSelector :: Selector '[] CULong
cumulativeIllegalInstructionExitCountSelector = mkSelector "cumulativeIllegalInstructionExitCount"

-- | @Selector@ for @cumulativeAppWatchdogExitCount@
cumulativeAppWatchdogExitCountSelector :: Selector '[] CULong
cumulativeAppWatchdogExitCountSelector = mkSelector "cumulativeAppWatchdogExitCount"

-- | @Selector@ for @cumulativeSuspendedWithLockedFileExitCount@
cumulativeSuspendedWithLockedFileExitCountSelector :: Selector '[] CULong
cumulativeSuspendedWithLockedFileExitCountSelector = mkSelector "cumulativeSuspendedWithLockedFileExitCount"

-- | @Selector@ for @cumulativeBackgroundTaskAssertionTimeoutExitCount@
cumulativeBackgroundTaskAssertionTimeoutExitCountSelector :: Selector '[] CULong
cumulativeBackgroundTaskAssertionTimeoutExitCountSelector = mkSelector "cumulativeBackgroundTaskAssertionTimeoutExitCount"

