{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterCommissionNodeParams@.
module ObjC.Matter.MTRCommissionerControlClusterCommissionNodeParams
  ( MTRCommissionerControlClusterCommissionNodeParams
  , IsMTRCommissionerControlClusterCommissionNodeParams(..)
  , requestID
  , setRequestID
  , responseTimeoutSeconds
  , setResponseTimeoutSeconds
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , requestIDSelector
  , responseTimeoutSecondsSelector
  , serverSideProcessingTimeoutSelector
  , setRequestIDSelector
  , setResponseTimeoutSecondsSelector
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

-- | @- requestID@
requestID :: IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams => mtrCommissionerControlClusterCommissionNodeParams -> IO (Id NSNumber)
requestID mtrCommissionerControlClusterCommissionNodeParams =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams requestIDSelector

-- | @- setRequestID:@
setRequestID :: (IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams, IsNSNumber value) => mtrCommissionerControlClusterCommissionNodeParams -> value -> IO ()
setRequestID mtrCommissionerControlClusterCommissionNodeParams value =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams setRequestIDSelector (toNSNumber value)

-- | @- responseTimeoutSeconds@
responseTimeoutSeconds :: IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams => mtrCommissionerControlClusterCommissionNodeParams -> IO (Id NSNumber)
responseTimeoutSeconds mtrCommissionerControlClusterCommissionNodeParams =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams responseTimeoutSecondsSelector

-- | @- setResponseTimeoutSeconds:@
setResponseTimeoutSeconds :: (IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams, IsNSNumber value) => mtrCommissionerControlClusterCommissionNodeParams -> value -> IO ()
setResponseTimeoutSeconds mtrCommissionerControlClusterCommissionNodeParams value =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams setResponseTimeoutSecondsSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams => mtrCommissionerControlClusterCommissionNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCommissionerControlClusterCommissionNodeParams =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams, IsNSNumber value) => mtrCommissionerControlClusterCommissionNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCommissionerControlClusterCommissionNodeParams value =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams => mtrCommissionerControlClusterCommissionNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCommissionerControlClusterCommissionNodeParams =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCommissionerControlClusterCommissionNodeParams mtrCommissionerControlClusterCommissionNodeParams, IsNSNumber value) => mtrCommissionerControlClusterCommissionNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrCommissionerControlClusterCommissionNodeParams value =
  sendMessage mtrCommissionerControlClusterCommissionNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestID@
requestIDSelector :: Selector '[] (Id NSNumber)
requestIDSelector = mkSelector "requestID"

-- | @Selector@ for @setRequestID:@
setRequestIDSelector :: Selector '[Id NSNumber] ()
setRequestIDSelector = mkSelector "setRequestID:"

-- | @Selector@ for @responseTimeoutSeconds@
responseTimeoutSecondsSelector :: Selector '[] (Id NSNumber)
responseTimeoutSecondsSelector = mkSelector "responseTimeoutSeconds"

-- | @Selector@ for @setResponseTimeoutSeconds:@
setResponseTimeoutSecondsSelector :: Selector '[Id NSNumber] ()
setResponseTimeoutSecondsSelector = mkSelector "setResponseTimeoutSeconds:"

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

