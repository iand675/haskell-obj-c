{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestEmitTestEventRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestEmitTestEventRequestParams
  ( MTRUnitTestingClusterTestEmitTestEventRequestParams
  , IsMTRUnitTestingClusterTestEmitTestEventRequestParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , arg2Selector
  , arg3Selector
  , serverSideProcessingTimeoutSelector
  , setArg1Selector
  , setArg2Selector
  , setArg3Selector
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
arg1 :: IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams => mtrUnitTestingClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestEmitTestEventRequestParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventRequestParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestEmitTestEventRequestParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams setArg1Selector (toNSNumber value)

-- | @- arg2@
arg2 :: IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams => mtrUnitTestingClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
arg2 mtrUnitTestingClusterTestEmitTestEventRequestParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventRequestParams -> value -> IO ()
setArg2 mtrUnitTestingClusterTestEmitTestEventRequestParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams setArg2Selector (toNSNumber value)

-- | @- arg3@
arg3 :: IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams => mtrUnitTestingClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
arg3 mtrUnitTestingClusterTestEmitTestEventRequestParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams arg3Selector

-- | @- setArg3:@
setArg3 :: (IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventRequestParams -> value -> IO ()
setArg3 mtrUnitTestingClusterTestEmitTestEventRequestParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams setArg3Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams => mtrUnitTestingClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestEmitTestEventRequestParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestEmitTestEventRequestParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams => mtrUnitTestingClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestEmitTestEventRequestParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestEmitTestEventRequestParams mtrUnitTestingClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestEmitTestEventRequestParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @arg3@
arg3Selector :: Selector '[] (Id NSNumber)
arg3Selector = mkSelector "arg3"

-- | @Selector@ for @setArg3:@
setArg3Selector :: Selector '[Id NSNumber] ()
setArg3Selector = mkSelector "setArg3:"

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

