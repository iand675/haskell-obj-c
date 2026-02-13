{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetWeekDayScheduleParams@.
module ObjC.Matter.MTRDoorLockClusterSetWeekDayScheduleParams
  ( MTRDoorLockClusterSetWeekDayScheduleParams
  , IsMTRDoorLockClusterSetWeekDayScheduleParams(..)
  , weekDayIndex
  , setWeekDayIndex
  , userIndex
  , setUserIndex
  , daysMask
  , setDaysMask
  , startHour
  , setStartHour
  , startMinute
  , setStartMinute
  , endHour
  , setEndHour
  , endMinute
  , setEndMinute
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , daysMaskSelector
  , endHourSelector
  , endMinuteSelector
  , serverSideProcessingTimeoutSelector
  , setDaysMaskSelector
  , setEndHourSelector
  , setEndMinuteSelector
  , setServerSideProcessingTimeoutSelector
  , setStartHourSelector
  , setStartMinuteSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setWeekDayIndexSelector
  , startHourSelector
  , startMinuteSelector
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
weekDayIndex :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
weekDayIndex mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams weekDayIndexSelector

-- | @- setWeekDayIndex:@
setWeekDayIndex :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setWeekDayIndex mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setWeekDayIndexSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setUserIndexSelector (toNSNumber value)

-- | @- daysMask@
daysMask :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
daysMask mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams daysMaskSelector

-- | @- setDaysMask:@
setDaysMask :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setDaysMask mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setDaysMaskSelector (toNSNumber value)

-- | @- startHour@
startHour :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
startHour mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams startHourSelector

-- | @- setStartHour:@
setStartHour :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setStartHour mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setStartHourSelector (toNSNumber value)

-- | @- startMinute@
startMinute :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
startMinute mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams startMinuteSelector

-- | @- setStartMinute:@
setStartMinute :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setStartMinute mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setStartMinuteSelector (toNSNumber value)

-- | @- endHour@
endHour :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
endHour mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams endHourSelector

-- | @- setEndHour:@
setEndHour :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setEndHour mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setEndHourSelector (toNSNumber value)

-- | @- endMinute@
endMinute :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
endMinute mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams endMinuteSelector

-- | @- setEndMinute:@
setEndMinute :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setEndMinute mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setEndMinuteSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams => mtrDoorLockClusterSetWeekDayScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetWeekDayScheduleParams =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetWeekDayScheduleParams mtrDoorLockClusterSetWeekDayScheduleParams, IsNSNumber value) => mtrDoorLockClusterSetWeekDayScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetWeekDayScheduleParams value =
  sendMessage mtrDoorLockClusterSetWeekDayScheduleParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @daysMask@
daysMaskSelector :: Selector '[] (Id NSNumber)
daysMaskSelector = mkSelector "daysMask"

-- | @Selector@ for @setDaysMask:@
setDaysMaskSelector :: Selector '[Id NSNumber] ()
setDaysMaskSelector = mkSelector "setDaysMask:"

-- | @Selector@ for @startHour@
startHourSelector :: Selector '[] (Id NSNumber)
startHourSelector = mkSelector "startHour"

-- | @Selector@ for @setStartHour:@
setStartHourSelector :: Selector '[Id NSNumber] ()
setStartHourSelector = mkSelector "setStartHour:"

-- | @Selector@ for @startMinute@
startMinuteSelector :: Selector '[] (Id NSNumber)
startMinuteSelector = mkSelector "startMinute"

-- | @Selector@ for @setStartMinute:@
setStartMinuteSelector :: Selector '[Id NSNumber] ()
setStartMinuteSelector = mkSelector "setStartMinute:"

-- | @Selector@ for @endHour@
endHourSelector :: Selector '[] (Id NSNumber)
endHourSelector = mkSelector "endHour"

-- | @Selector@ for @setEndHour:@
setEndHourSelector :: Selector '[Id NSNumber] ()
setEndHourSelector = mkSelector "setEndHour:"

-- | @Selector@ for @endMinute@
endMinuteSelector :: Selector '[] (Id NSNumber)
endMinuteSelector = mkSelector "endMinute"

-- | @Selector@ for @setEndMinute:@
setEndMinuteSelector :: Selector '[Id NSNumber] ()
setEndMinuteSelector = mkSelector "setEndMinute:"

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

