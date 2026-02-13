{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXForegroundExitData
--
-- A class that encapsulates cumulative application exit metrics when the application is on screen.
--
-- Foreground exits are user visible terminations that, when unexpected, interrupt usage.
--
-- Not all foreground exits are unexpected. See the documentation for each exit reason for more information.
--
-- Generated bindings for @MXForegroundExitData@.
module ObjC.MetricKit.MXForegroundExitData
  ( MXForegroundExitData
  , IsMXForegroundExitData(..)
  , cumulativeNormalAppExitCount
  , cumulativeMemoryResourceLimitExitCount
  , cumulativeBadAccessExitCount
  , cumulativeAbnormalExitCount
  , cumulativeIllegalInstructionExitCount
  , cumulativeAppWatchdogExitCount
  , cumulativeAbnormalExitCountSelector
  , cumulativeAppWatchdogExitCountSelector
  , cumulativeBadAccessExitCountSelector
  , cumulativeIllegalInstructionExitCountSelector
  , cumulativeMemoryResourceLimitExitCountSelector
  , cumulativeNormalAppExitCountSelector


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
cumulativeNormalAppExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeNormalAppExitCount mxForegroundExitData =
  sendMessage mxForegroundExitData cumulativeNormalAppExitCountSelector

-- | cumulativeMemoryResourceLimitExitCount
--
-- Cumulative number of times the application was terminated for exceeding a memory consumption limit.
--
-- ObjC selector: @- cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeMemoryResourceLimitExitCount mxForegroundExitData =
  sendMessage mxForegroundExitData cumulativeMemoryResourceLimitExitCountSelector

-- | cumulativeBadAccessExitCount
--
-- Cumulative number of times the application was terminated for attempting to access invalid memory, or attempting to access memory in a manner not allowed by the memory's protection level (e.g. writing to read-only memory).
--
-- ObjC selector: @- cumulativeBadAccessExitCount@
cumulativeBadAccessExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeBadAccessExitCount mxForegroundExitData =
  sendMessage mxForegroundExitData cumulativeBadAccessExitCountSelector

-- | cumulativeAbnormalExitCount
--
-- Cumulative number of times the application exited abnormally.
--
-- The most common causes of crashes with this exception type are uncaught Objective-C/C++ exceptions and calls to abort().
--
-- ObjC selector: @- cumulativeAbnormalExitCount@
cumulativeAbnormalExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeAbnormalExitCount mxForegroundExitData =
  sendMessage mxForegroundExitData cumulativeAbnormalExitCountSelector

-- | cumulativeIllegalInstructionExitCount
--
-- Cumulative number of times the application terminated for attempting to execute an illegal or undefined instruction.
--
-- The process may have attempted to jump to an invalid address via a misconfigured function pointer.
--
-- ObjC selector: @- cumulativeIllegalInstructionExitCount@
cumulativeIllegalInstructionExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeIllegalInstructionExitCount mxForegroundExitData =
  sendMessage mxForegroundExitData cumulativeIllegalInstructionExitCountSelector

-- | cumulativeAppWatchdogExitCount
--
-- Cumulative number of times the application was terminated because a watchdog timeout occured.
--
-- These can occur when the application took too long to launch, terminate, or respond to system events.
--
-- ObjC selector: @- cumulativeAppWatchdogExitCount@
cumulativeAppWatchdogExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeAppWatchdogExitCount mxForegroundExitData =
  sendMessage mxForegroundExitData cumulativeAppWatchdogExitCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeNormalAppExitCount@
cumulativeNormalAppExitCountSelector :: Selector '[] CULong
cumulativeNormalAppExitCountSelector = mkSelector "cumulativeNormalAppExitCount"

-- | @Selector@ for @cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCountSelector :: Selector '[] CULong
cumulativeMemoryResourceLimitExitCountSelector = mkSelector "cumulativeMemoryResourceLimitExitCount"

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

