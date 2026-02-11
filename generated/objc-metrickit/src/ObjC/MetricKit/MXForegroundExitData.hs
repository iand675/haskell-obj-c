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
  , cumulativeNormalAppExitCountSelector
  , cumulativeMemoryResourceLimitExitCountSelector
  , cumulativeBadAccessExitCountSelector
  , cumulativeAbnormalExitCountSelector
  , cumulativeIllegalInstructionExitCountSelector
  , cumulativeAppWatchdogExitCountSelector


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
cumulativeNormalAppExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeNormalAppExitCount mxForegroundExitData  =
  sendMsg mxForegroundExitData (mkSelector "cumulativeNormalAppExitCount") retCULong []

-- | cumulativeMemoryResourceLimitExitCount
--
-- Cumulative number of times the application was terminated for exceeding a memory consumption limit.
--
-- ObjC selector: @- cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeMemoryResourceLimitExitCount mxForegroundExitData  =
  sendMsg mxForegroundExitData (mkSelector "cumulativeMemoryResourceLimitExitCount") retCULong []

-- | cumulativeBadAccessExitCount
--
-- Cumulative number of times the application was terminated for attempting to access invalid memory, or attempting to access memory in a manner not allowed by the memory's protection level (e.g. writing to read-only memory).
--
-- ObjC selector: @- cumulativeBadAccessExitCount@
cumulativeBadAccessExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeBadAccessExitCount mxForegroundExitData  =
  sendMsg mxForegroundExitData (mkSelector "cumulativeBadAccessExitCount") retCULong []

-- | cumulativeAbnormalExitCount
--
-- Cumulative number of times the application exited abnormally.
--
-- The most common causes of crashes with this exception type are uncaught Objective-C/C++ exceptions and calls to abort().
--
-- ObjC selector: @- cumulativeAbnormalExitCount@
cumulativeAbnormalExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeAbnormalExitCount mxForegroundExitData  =
  sendMsg mxForegroundExitData (mkSelector "cumulativeAbnormalExitCount") retCULong []

-- | cumulativeIllegalInstructionExitCount
--
-- Cumulative number of times the application terminated for attempting to execute an illegal or undefined instruction.
--
-- The process may have attempted to jump to an invalid address via a misconfigured function pointer.
--
-- ObjC selector: @- cumulativeIllegalInstructionExitCount@
cumulativeIllegalInstructionExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeIllegalInstructionExitCount mxForegroundExitData  =
  sendMsg mxForegroundExitData (mkSelector "cumulativeIllegalInstructionExitCount") retCULong []

-- | cumulativeAppWatchdogExitCount
--
-- Cumulative number of times the application was terminated because a watchdog timeout occured.
--
-- These can occur when the application took too long to launch, terminate, or respond to system events.
--
-- ObjC selector: @- cumulativeAppWatchdogExitCount@
cumulativeAppWatchdogExitCount :: IsMXForegroundExitData mxForegroundExitData => mxForegroundExitData -> IO CULong
cumulativeAppWatchdogExitCount mxForegroundExitData  =
  sendMsg mxForegroundExitData (mkSelector "cumulativeAppWatchdogExitCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeNormalAppExitCount@
cumulativeNormalAppExitCountSelector :: Selector
cumulativeNormalAppExitCountSelector = mkSelector "cumulativeNormalAppExitCount"

-- | @Selector@ for @cumulativeMemoryResourceLimitExitCount@
cumulativeMemoryResourceLimitExitCountSelector :: Selector
cumulativeMemoryResourceLimitExitCountSelector = mkSelector "cumulativeMemoryResourceLimitExitCount"

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

