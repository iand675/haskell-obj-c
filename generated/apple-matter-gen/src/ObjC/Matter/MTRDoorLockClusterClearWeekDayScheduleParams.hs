{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterClearWeekDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterClearWeekDayScheduleParams
  ( MTRDoorLockClusterClearWeekDayScheduleParams
  , IsMTRDoorLockClusterClearWeekDayScheduleParams(..)
  , weekDayIndex
  , setWeekDayIndex
  , userIndex
  , setUserIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setWeekDayIndexSelector
  , timedInvokeTimeoutMsSelector
  , userIndexSelector
  , weekDayIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- weekDayIndex@
weekDayIndex :: IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams => mtrDoorLockClusterClearWeekDayScheduleParams -> IO (Id NSNumber)
weekDayIndex mtrDoorLockClusterClearWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams weekDayIndexSelector

-- | @- setWeekDayIndex:@
setWeekDayIndex :: (IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearWeekDayScheduleParams -> value -> IO ()
setWeekDayIndex mtrDoorLockClusterClearWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams setWeekDayIndexSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams => mtrDoorLockClusterClearWeekDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterClearWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearWeekDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterClearWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams setUserIndexSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams => mtrDoorLockClusterClearWeekDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterClearWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearWeekDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterClearWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams => mtrDoorLockClusterClearWeekDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterClearWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterClearWeekDayScheduleParams mtrDoorLockClusterClearWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearWeekDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterClearWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearWeekDayScheduleParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @weekDayIndex@
weekDayIndexSelector :: Selector '[] (Id NSNumber)
weekDayIndexSelector = mkSelector "weekDayIndex"

-- | @Selector@ for @setWeekDayIndex:@
setWeekDayIndexSelector :: Selector '[Id NSNumber] ()
setWeekDayIndexSelector = mkSelector "setWeekDayIndex:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

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

