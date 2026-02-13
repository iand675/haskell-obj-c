{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterStayActiveRequestParams@.
module ObjC.Matter.MTRICDManagementClusterStayActiveRequestParams
  ( MTRICDManagementClusterStayActiveRequestParams
  , IsMTRICDManagementClusterStayActiveRequestParams(..)
  , stayActiveDuration
  , setStayActiveDuration
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setStayActiveDurationSelector
  , setTimedInvokeTimeoutMsSelector
  , stayActiveDurationSelector
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

-- | @- stayActiveDuration@
stayActiveDuration :: IsMTRICDManagementClusterStayActiveRequestParams mtricdManagementClusterStayActiveRequestParams => mtricdManagementClusterStayActiveRequestParams -> IO (Id NSNumber)
stayActiveDuration mtricdManagementClusterStayActiveRequestParams =
  sendMessage mtricdManagementClusterStayActiveRequestParams stayActiveDurationSelector

-- | @- setStayActiveDuration:@
setStayActiveDuration :: (IsMTRICDManagementClusterStayActiveRequestParams mtricdManagementClusterStayActiveRequestParams, IsNSNumber value) => mtricdManagementClusterStayActiveRequestParams -> value -> IO ()
setStayActiveDuration mtricdManagementClusterStayActiveRequestParams value =
  sendMessage mtricdManagementClusterStayActiveRequestParams setStayActiveDurationSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRICDManagementClusterStayActiveRequestParams mtricdManagementClusterStayActiveRequestParams => mtricdManagementClusterStayActiveRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtricdManagementClusterStayActiveRequestParams =
  sendMessage mtricdManagementClusterStayActiveRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRICDManagementClusterStayActiveRequestParams mtricdManagementClusterStayActiveRequestParams, IsNSNumber value) => mtricdManagementClusterStayActiveRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtricdManagementClusterStayActiveRequestParams value =
  sendMessage mtricdManagementClusterStayActiveRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRICDManagementClusterStayActiveRequestParams mtricdManagementClusterStayActiveRequestParams => mtricdManagementClusterStayActiveRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtricdManagementClusterStayActiveRequestParams =
  sendMessage mtricdManagementClusterStayActiveRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRICDManagementClusterStayActiveRequestParams mtricdManagementClusterStayActiveRequestParams, IsNSNumber value) => mtricdManagementClusterStayActiveRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtricdManagementClusterStayActiveRequestParams value =
  sendMessage mtricdManagementClusterStayActiveRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stayActiveDuration@
stayActiveDurationSelector :: Selector '[] (Id NSNumber)
stayActiveDurationSelector = mkSelector "stayActiveDuration"

-- | @Selector@ for @setStayActiveDuration:@
setStayActiveDurationSelector :: Selector '[Id NSNumber] ()
setStayActiveDurationSelector = mkSelector "setStayActiveDuration:"

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

