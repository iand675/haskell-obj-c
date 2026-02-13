{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterManuallyTriggerTransportParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterManuallyTriggerTransportParams
  ( MTRPushAVStreamTransportClusterManuallyTriggerTransportParams
  , IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams(..)
  , connectionID
  , setConnectionID
  , activationReason
  , setActivationReason
  , timeControl
  , setTimeControl
  , userDefined
  , setUserDefined
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , activationReasonSelector
  , connectionIDSelector
  , serverSideProcessingTimeoutSelector
  , setActivationReasonSelector
  , setConnectionIDSelector
  , setServerSideProcessingTimeoutSelector
  , setTimeControlSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserDefinedSelector
  , timeControlSelector
  , timedInvokeTimeoutMsSelector
  , userDefinedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterManuallyTriggerTransportParams =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams connectionIDSelector

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterManuallyTriggerTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams setConnectionIDSelector (toNSNumber value)

-- | @- activationReason@
activationReason :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
activationReason mtrPushAVStreamTransportClusterManuallyTriggerTransportParams =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams activationReasonSelector

-- | @- setActivationReason:@
setActivationReason :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setActivationReason mtrPushAVStreamTransportClusterManuallyTriggerTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams setActivationReasonSelector (toNSNumber value)

-- | @- timeControl@
timeControl :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct)
timeControl mtrPushAVStreamTransportClusterManuallyTriggerTransportParams =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams timeControlSelector

-- | @- setTimeControl:@
setTimeControl :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setTimeControl mtrPushAVStreamTransportClusterManuallyTriggerTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams setTimeControlSelector (toMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value)

-- | @- userDefined@
userDefined :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSData)
userDefined mtrPushAVStreamTransportClusterManuallyTriggerTransportParams =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams userDefinedSelector

-- | @- setUserDefined:@
setUserDefined :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSData value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setUserDefined mtrPushAVStreamTransportClusterManuallyTriggerTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams setUserDefinedSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrPushAVStreamTransportClusterManuallyTriggerTransportParams =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrPushAVStreamTransportClusterManuallyTriggerTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrPushAVStreamTransportClusterManuallyTriggerTransportParams =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setServerSideProcessingTimeout mtrPushAVStreamTransportClusterManuallyTriggerTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterManuallyTriggerTransportParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector '[] (Id NSNumber)
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector '[Id NSNumber] ()
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @activationReason@
activationReasonSelector :: Selector '[] (Id NSNumber)
activationReasonSelector = mkSelector "activationReason"

-- | @Selector@ for @setActivationReason:@
setActivationReasonSelector :: Selector '[Id NSNumber] ()
setActivationReasonSelector = mkSelector "setActivationReason:"

-- | @Selector@ for @timeControl@
timeControlSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct)
timeControlSelector = mkSelector "timeControl"

-- | @Selector@ for @setTimeControl:@
setTimeControlSelector :: Selector '[Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct] ()
setTimeControlSelector = mkSelector "setTimeControl:"

-- | @Selector@ for @userDefined@
userDefinedSelector :: Selector '[] (Id NSData)
userDefinedSelector = mkSelector "userDefined"

-- | @Selector@ for @setUserDefined:@
setUserDefinedSelector :: Selector '[Id NSData] ()
setUserDefinedSelector = mkSelector "setUserDefined:"

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

