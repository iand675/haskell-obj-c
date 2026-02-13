{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEnumsRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestEnumsRequestParams
  ( MTRTestClusterClusterTestEnumsRequestParams
  , IsMTRTestClusterClusterTestEnumsRequestParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , arg2Selector
  , serverSideProcessingTimeoutSelector
  , setArg1Selector
  , setArg2Selector
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
arg1 :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestEnumsRequestParams =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestEnumsRequestParams value =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams setArg1Selector (toNSNumber value)

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
arg2 mtrTestClusterClusterTestEnumsRequestParams =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestEnumsRequestParams value =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams setArg2Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestEnumsRequestParams =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestEnumsRequestParams value =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestEnumsRequestParams =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestEnumsRequestParams value =
  sendMessage mtrTestClusterClusterTestEnumsRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSNumber)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSNumber] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector '[] (Id NSNumber)
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector '[Id NSNumber] ()
setArg2Selector = mkSelector "setArg2:"

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

