{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestSimpleArgumentRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestSimpleArgumentRequestParams
  ( MTRUnitTestingClusterTestSimpleArgumentRequestParams
  , IsMTRUnitTestingClusterTestSimpleArgumentRequestParams(..)
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
arg1 :: IsMTRUnitTestingClusterTestSimpleArgumentRequestParams mtrUnitTestingClusterTestSimpleArgumentRequestParams => mtrUnitTestingClusterTestSimpleArgumentRequestParams -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestSimpleArgumentRequestParams =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentRequestParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestSimpleArgumentRequestParams mtrUnitTestingClusterTestSimpleArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSimpleArgumentRequestParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestSimpleArgumentRequestParams value =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentRequestParams setArg1Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestSimpleArgumentRequestParams mtrUnitTestingClusterTestSimpleArgumentRequestParams => mtrUnitTestingClusterTestSimpleArgumentRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestSimpleArgumentRequestParams =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestSimpleArgumentRequestParams mtrUnitTestingClusterTestSimpleArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSimpleArgumentRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestSimpleArgumentRequestParams value =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestSimpleArgumentRequestParams mtrUnitTestingClusterTestSimpleArgumentRequestParams => mtrUnitTestingClusterTestSimpleArgumentRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestSimpleArgumentRequestParams =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestSimpleArgumentRequestParams mtrUnitTestingClusterTestSimpleArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSimpleArgumentRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestSimpleArgumentRequestParams value =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

