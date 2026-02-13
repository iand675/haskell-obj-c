{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestSimpleOptionalArgumentRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestSimpleOptionalArgumentRequestParams
  ( MTRTestClusterClusterTestSimpleOptionalArgumentRequestParams
  , IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams(..)
  , arg1
  , setArg1
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , serverSideProcessingTimeoutSelector
  , setArg1Selector
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

-- | @- arg1@
arg1 :: IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams => mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams setArg1Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams => mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams => mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestSimpleOptionalArgumentRequestParams mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestSimpleOptionalArgumentRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSNumber)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSNumber] ()
setArg1Selector = mkSelector "setArg1:"

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

