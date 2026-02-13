{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetYearDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterSetYearDayScheduleParams
  ( MTRDoorLockClusterSetYearDayScheduleParams
  , IsMTRDoorLockClusterSetYearDayScheduleParams(..)
  , yearDayIndex
  , setYearDayIndex
  , userIndex
  , setUserIndex
  , localStartTime
  , setLocalStartTime
  , localEndTime
  , setLocalEndTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , localEndTimeSelector
  , localStartTimeSelector
  , serverSideProcessingTimeoutSelector
  , setLocalEndTimeSelector
  , setLocalStartTimeSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setYearDayIndexSelector
  , timedInvokeTimeoutMsSelector
  , userIndexSelector
  , yearDayIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- yearDayIndex@
yearDayIndex :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
yearDayIndex mtrDoorLockClusterSetYearDayScheduleParams =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams yearDayIndexSelector

-- | @- setYearDayIndex:@
setYearDayIndex :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setYearDayIndex mtrDoorLockClusterSetYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams setYearDayIndexSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetYearDayScheduleParams =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams setUserIndexSelector (toNSNumber value)

-- | @- localStartTime@
localStartTime :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
localStartTime mtrDoorLockClusterSetYearDayScheduleParams =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams localStartTimeSelector

-- | @- setLocalStartTime:@
setLocalStartTime :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setLocalStartTime mtrDoorLockClusterSetYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams setLocalStartTimeSelector (toNSNumber value)

-- | @- localEndTime@
localEndTime :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
localEndTime mtrDoorLockClusterSetYearDayScheduleParams =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams localEndTimeSelector

-- | @- setLocalEndTime:@
setLocalEndTime :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setLocalEndTime mtrDoorLockClusterSetYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams setLocalEndTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetYearDayScheduleParams =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams => mtrDoorLockClusterSetYearDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetYearDayScheduleParams =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetYearDayScheduleParams mtrDoorLockClusterSetYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetYearDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetYearDayScheduleParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @yearDayIndex@
yearDayIndexSelector :: Selector '[] (Id NSNumber)
yearDayIndexSelector = mkSelector "yearDayIndex"

-- | @Selector@ for @setYearDayIndex:@
setYearDayIndexSelector :: Selector '[Id NSNumber] ()
setYearDayIndexSelector = mkSelector "setYearDayIndex:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @localStartTime@
localStartTimeSelector :: Selector '[] (Id NSNumber)
localStartTimeSelector = mkSelector "localStartTime"

-- | @Selector@ for @setLocalStartTime:@
setLocalStartTimeSelector :: Selector '[Id NSNumber] ()
setLocalStartTimeSelector = mkSelector "setLocalStartTime:"

-- | @Selector@ for @localEndTime@
localEndTimeSelector :: Selector '[] (Id NSNumber)
localEndTimeSelector = mkSelector "localEndTime"

-- | @Selector@ for @setLocalEndTime:@
setLocalEndTimeSelector :: Selector '[Id NSNumber] ()
setLocalEndTimeSelector = mkSelector "setLocalEndTime:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

