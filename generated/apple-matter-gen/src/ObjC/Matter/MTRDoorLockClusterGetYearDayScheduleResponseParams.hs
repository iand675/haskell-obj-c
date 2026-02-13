{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetYearDayScheduleResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetYearDayScheduleResponseParams
  ( MTRDoorLockClusterGetYearDayScheduleResponseParams
  , IsMTRDoorLockClusterGetYearDayScheduleResponseParams(..)
  , initWithResponseValue_error
  , yearDayIndex
  , setYearDayIndex
  , userIndex
  , setUserIndex
  , status
  , setStatus
  , localStartTime
  , setLocalStartTime
  , localEndTime
  , setLocalEndTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , localEndTimeSelector
  , localStartTimeSelector
  , setLocalEndTimeSelector
  , setLocalStartTimeSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setYearDayIndexSelector
  , statusSelector
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

-- | Initialize an MTRDoorLockClusterGetYearDayScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetYearDayScheduleResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetYearDayScheduleResponseParams responseValue error_ =
  sendOwnedMessage mtrDoorLockClusterGetYearDayScheduleResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- yearDayIndex@
yearDayIndex :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
yearDayIndex mtrDoorLockClusterGetYearDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams yearDayIndexSelector

-- | @- setYearDayIndex:@
setYearDayIndex :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setYearDayIndex mtrDoorLockClusterGetYearDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams setYearDayIndexSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetYearDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetYearDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams setUserIndexSelector (toNSNumber value)

-- | @- status@
status :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterGetYearDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterGetYearDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams setStatusSelector (toNSNumber value)

-- | @- localStartTime@
localStartTime :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
localStartTime mtrDoorLockClusterGetYearDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams localStartTimeSelector

-- | @- setLocalStartTime:@
setLocalStartTime :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setLocalStartTime mtrDoorLockClusterGetYearDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams setLocalStartTimeSelector (toNSNumber value)

-- | @- localEndTime@
localEndTime :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
localEndTime mtrDoorLockClusterGetYearDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams localEndTimeSelector

-- | @- setLocalEndTime:@
setLocalEndTime :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setLocalEndTime mtrDoorLockClusterGetYearDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams setLocalEndTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams => mtrDoorLockClusterGetYearDayScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetYearDayScheduleResponseParams =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetYearDayScheduleResponseParams mtrDoorLockClusterGetYearDayScheduleResponseParams, IsNSNumber value) => mtrDoorLockClusterGetYearDayScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetYearDayScheduleResponseParams value =
  sendMessage mtrDoorLockClusterGetYearDayScheduleResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDoorLockClusterGetYearDayScheduleResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

