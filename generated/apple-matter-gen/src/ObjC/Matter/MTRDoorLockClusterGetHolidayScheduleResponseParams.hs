{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetHolidayScheduleResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetHolidayScheduleResponseParams
  ( MTRDoorLockClusterGetHolidayScheduleResponseParams
  , IsMTRDoorLockClusterGetHolidayScheduleResponseParams(..)
  , initWithResponseValue_error
  , holidayIndex
  , setHolidayIndex
  , status
  , setStatus
  , localStartTime
  , setLocalStartTime
  , localEndTime
  , setLocalEndTime
  , operatingMode
  , setOperatingMode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , holidayIndexSelector
  , initWithResponseValue_errorSelector
  , localEndTimeSelector
  , localStartTimeSelector
  , operatingModeSelector
  , setHolidayIndexSelector
  , setLocalEndTimeSelector
  , setLocalStartTimeSelector
  , setOperatingModeSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , statusSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRDoorLockClusterGetHolidayScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetHolidayScheduleResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetHolidayScheduleResponseParams responseValue error_ =
  sendOwnedMessage mtrDoorLockClusterGetHolidayScheduleResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- holidayIndex@
holidayIndex :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
holidayIndex mtrDoorLockClusterGetHolidayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams holidayIndexSelector

-- | @- setHolidayIndex:@
setHolidayIndex :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setHolidayIndex mtrDoorLockClusterGetHolidayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams setHolidayIndexSelector (toNSNumber value)

-- | @- status@
status :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterGetHolidayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterGetHolidayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams setStatusSelector (toNSNumber value)

-- | @- localStartTime@
localStartTime :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
localStartTime mtrDoorLockClusterGetHolidayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams localStartTimeSelector

-- | @- setLocalStartTime:@
setLocalStartTime :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setLocalStartTime mtrDoorLockClusterGetHolidayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams setLocalStartTimeSelector (toNSNumber value)

-- | @- localEndTime@
localEndTime :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
localEndTime mtrDoorLockClusterGetHolidayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams localEndTimeSelector

-- | @- setLocalEndTime:@
setLocalEndTime :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setLocalEndTime mtrDoorLockClusterGetHolidayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams setLocalEndTimeSelector (toNSNumber value)

-- | @- operatingMode@
operatingMode :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
operatingMode mtrDoorLockClusterGetHolidayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams operatingModeSelector

-- | @- setOperatingMode:@
setOperatingMode :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setOperatingMode mtrDoorLockClusterGetHolidayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams setOperatingModeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams => mtrDoorLockClusterGetHolidayScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetHolidayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetHolidayScheduleResponseParams mtrDoorLockClusterGetHolidayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetHolidayScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetHolidayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetHolidayScheduleResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDoorLockClusterGetHolidayScheduleResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @holidayIndex@
holidayIndexSelector :: Selector '[] (Id NSNumber)
holidayIndexSelector = mkSelector "holidayIndex"

-- | @Selector@ for @setHolidayIndex:@
setHolidayIndexSelector :: Selector '[Id NSNumber] ()
setHolidayIndexSelector = mkSelector "setHolidayIndex:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

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

-- | @Selector@ for @operatingMode@
operatingModeSelector :: Selector '[] (Id NSNumber)
operatingModeSelector = mkSelector "operatingMode"

-- | @Selector@ for @setOperatingMode:@
setOperatingModeSelector :: Selector '[Id NSNumber] ()
setOperatingModeSelector = mkSelector "setOperatingMode:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

