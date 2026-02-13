{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterKeepActiveParams@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterKeepActiveParams
  ( MTRBridgedDeviceBasicInformationClusterKeepActiveParams
  , IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams(..)
  , stayActiveDuration
  , setStayActiveDuration
  , timeoutMs
  , setTimeoutMs
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setStayActiveDurationSelector
  , setTimedInvokeTimeoutMsSelector
  , setTimeoutMsSelector
  , stayActiveDurationSelector
  , timedInvokeTimeoutMsSelector
  , timeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stayActiveDuration@
stayActiveDuration :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
stayActiveDuration mtrBridgedDeviceBasicInformationClusterKeepActiveParams =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams stayActiveDurationSelector

-- | @- setStayActiveDuration:@
setStayActiveDuration :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setStayActiveDuration mtrBridgedDeviceBasicInformationClusterKeepActiveParams value =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams setStayActiveDurationSelector (toNSNumber value)

-- | @- timeoutMs@
timeoutMs :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
timeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams timeoutMsSelector

-- | @- setTimeoutMs:@
setTimeoutMs :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setTimeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams value =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams setTimeoutMsSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams value =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrBridgedDeviceBasicInformationClusterKeepActiveParams =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setServerSideProcessingTimeout mtrBridgedDeviceBasicInformationClusterKeepActiveParams value =
  sendMessage mtrBridgedDeviceBasicInformationClusterKeepActiveParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stayActiveDuration@
stayActiveDurationSelector :: Selector '[] (Id NSNumber)
stayActiveDurationSelector = mkSelector "stayActiveDuration"

-- | @Selector@ for @setStayActiveDuration:@
setStayActiveDurationSelector :: Selector '[Id NSNumber] ()
setStayActiveDurationSelector = mkSelector "setStayActiveDuration:"

-- | @Selector@ for @timeoutMs@
timeoutMsSelector :: Selector '[] (Id NSNumber)
timeoutMsSelector = mkSelector "timeoutMs"

-- | @Selector@ for @setTimeoutMs:@
setTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimeoutMsSelector = mkSelector "setTimeoutMs:"

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

