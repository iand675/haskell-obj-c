{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherAlarmClusterResetParams@.
module ObjC.Matter.MTRDishwasherAlarmClusterResetParams
  ( MTRDishwasherAlarmClusterResetParams
  , IsMTRDishwasherAlarmClusterResetParams(..)
  , alarms
  , setAlarms
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , alarmsSelector
  , serverSideProcessingTimeoutSelector
  , setAlarmsSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
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

-- | @- alarms@
alarms :: IsMTRDishwasherAlarmClusterResetParams mtrDishwasherAlarmClusterResetParams => mtrDishwasherAlarmClusterResetParams -> IO (Id NSNumber)
alarms mtrDishwasherAlarmClusterResetParams =
  sendMessage mtrDishwasherAlarmClusterResetParams alarmsSelector

-- | @- setAlarms:@
setAlarms :: (IsMTRDishwasherAlarmClusterResetParams mtrDishwasherAlarmClusterResetParams, IsNSNumber value) => mtrDishwasherAlarmClusterResetParams -> value -> IO ()
setAlarms mtrDishwasherAlarmClusterResetParams value =
  sendMessage mtrDishwasherAlarmClusterResetParams setAlarmsSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDishwasherAlarmClusterResetParams mtrDishwasherAlarmClusterResetParams => mtrDishwasherAlarmClusterResetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDishwasherAlarmClusterResetParams =
  sendMessage mtrDishwasherAlarmClusterResetParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDishwasherAlarmClusterResetParams mtrDishwasherAlarmClusterResetParams, IsNSNumber value) => mtrDishwasherAlarmClusterResetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDishwasherAlarmClusterResetParams value =
  sendMessage mtrDishwasherAlarmClusterResetParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDishwasherAlarmClusterResetParams mtrDishwasherAlarmClusterResetParams => mtrDishwasherAlarmClusterResetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDishwasherAlarmClusterResetParams =
  sendMessage mtrDishwasherAlarmClusterResetParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDishwasherAlarmClusterResetParams mtrDishwasherAlarmClusterResetParams, IsNSNumber value) => mtrDishwasherAlarmClusterResetParams -> value -> IO ()
setServerSideProcessingTimeout mtrDishwasherAlarmClusterResetParams value =
  sendMessage mtrDishwasherAlarmClusterResetParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarms@
alarmsSelector :: Selector '[] (Id NSNumber)
alarmsSelector = mkSelector "alarms"

-- | @Selector@ for @setAlarms:@
setAlarmsSelector :: Selector '[Id NSNumber] ()
setAlarmsSelector = mkSelector "setAlarms:"

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

