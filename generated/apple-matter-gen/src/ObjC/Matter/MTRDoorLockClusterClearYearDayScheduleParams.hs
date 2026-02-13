{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterClearYearDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterClearYearDayScheduleParams
  ( MTRDoorLockClusterClearYearDayScheduleParams
  , IsMTRDoorLockClusterClearYearDayScheduleParams(..)
  , yearDayIndex
  , setYearDayIndex
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
yearDayIndex :: IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams => mtrDoorLockClusterClearYearDayScheduleParams -> IO (Id NSNumber)
yearDayIndex mtrDoorLockClusterClearYearDayScheduleParams =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams yearDayIndexSelector

-- | @- setYearDayIndex:@
setYearDayIndex :: (IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearYearDayScheduleParams -> value -> IO ()
setYearDayIndex mtrDoorLockClusterClearYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams setYearDayIndexSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams => mtrDoorLockClusterClearYearDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterClearYearDayScheduleParams =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearYearDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterClearYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams setUserIndexSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams => mtrDoorLockClusterClearYearDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterClearYearDayScheduleParams =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearYearDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterClearYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams => mtrDoorLockClusterClearYearDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterClearYearDayScheduleParams =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterClearYearDayScheduleParams mtrDoorLockClusterClearYearDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterClearYearDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterClearYearDayScheduleParams value =
  sendMessage mtrDoorLockClusterClearYearDayScheduleParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

