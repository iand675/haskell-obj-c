{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetWeekDayScheduleResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetWeekDayScheduleResponseParams
  ( MTRDoorLockClusterGetWeekDayScheduleResponseParams
  , IsMTRDoorLockClusterGetWeekDayScheduleResponseParams(..)
  , initWithResponseValue_error
  , weekDayIndex
  , setWeekDayIndex
  , userIndex
  , setUserIndex
  , status
  , setStatus
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
  , daysMaskSelector
  , endHourSelector
  , endMinuteSelector
  , initWithResponseValue_errorSelector
  , setDaysMaskSelector
  , setEndHourSelector
  , setEndMinuteSelector
  , setStartHourSelector
  , setStartMinuteSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setWeekDayIndexSelector
  , startHourSelector
  , startMinuteSelector
  , statusSelector
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

-- | Initialize an MTRDoorLockClusterGetWeekDayScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetWeekDayScheduleResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetWeekDayScheduleResponseParams responseValue error_ =
  sendOwnedMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- weekDayIndex@
weekDayIndex :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
weekDayIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams weekDayIndexSelector

-- | @- setWeekDayIndex:@
setWeekDayIndex :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setWeekDayIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setWeekDayIndexSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setUserIndexSelector (toNSNumber value)

-- | @- status@
status :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setStatusSelector (toNSNumber value)

-- | @- daysMask@
daysMask :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
daysMask mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams daysMaskSelector

-- | @- setDaysMask:@
setDaysMask :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setDaysMask mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setDaysMaskSelector (toNSNumber value)

-- | @- startHour@
startHour :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
startHour mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams startHourSelector

-- | @- setStartHour:@
setStartHour :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setStartHour mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setStartHourSelector (toNSNumber value)

-- | @- startMinute@
startMinute :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
startMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams startMinuteSelector

-- | @- setStartMinute:@
setStartMinute :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setStartMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setStartMinuteSelector (toNSNumber value)

-- | @- endHour@
endHour :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
endHour mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams endHourSelector

-- | @- setEndHour:@
setEndHour :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setEndHour mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setEndHourSelector (toNSNumber value)

-- | @- endMinute@
endMinute :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
endMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams endMinuteSelector

-- | @- setEndMinute:@
setEndMinute :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setEndMinute mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setEndMinuteSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetWeekDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetWeekDayScheduleResponseParams mtrDoorLockClusterGetWeekDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetWeekDayScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetWeekDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetWeekDayScheduleResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDoorLockClusterGetWeekDayScheduleResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

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

