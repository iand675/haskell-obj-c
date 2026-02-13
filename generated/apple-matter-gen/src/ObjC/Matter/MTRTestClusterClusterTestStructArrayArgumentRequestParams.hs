{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestStructArrayArgumentRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestStructArrayArgumentRequestParams
  ( MTRTestClusterClusterTestStructArrayArgumentRequestParams
  , IsMTRTestClusterClusterTestStructArrayArgumentRequestParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , arg4
  , setArg4
  , arg5
  , setArg5
  , arg6
  , setArg6
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , arg2Selector
  , arg3Selector
  , arg4Selector
  , arg5Selector
  , arg6Selector
  , serverSideProcessingTimeoutSelector
  , setArg1Selector
  , setArg2Selector
  , setArg3Selector
  , setArg4Selector
  , setArg5Selector
  , setArg6Selector
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
arg1 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg1 mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setArg1Selector (toNSArray value)

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg2 mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setArg2Selector (toNSArray value)

-- | @- arg3@
arg3 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg3 mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams arg3Selector

-- | @- setArg3:@
setArg3 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg3 mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setArg3Selector (toNSArray value)

-- | @- arg4@
arg4 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg4 mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams arg4Selector

-- | @- setArg4:@
setArg4 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg4 mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setArg4Selector (toNSArray value)

-- | @- arg5@
arg5 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
arg5 mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams arg5Selector

-- | @- setArg5:@
setArg5 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg5 mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setArg5Selector (toNSNumber value)

-- | @- arg6@
arg6 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
arg6 mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams arg6Selector

-- | @- setArg6:@
setArg6 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg6 mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setArg6Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestStructArrayArgumentRequestParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestStructArrayArgumentRequestParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSArray)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSArray] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector '[] (Id NSArray)
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector '[Id NSArray] ()
setArg2Selector = mkSelector "setArg2:"

-- | @Selector@ for @arg3@
arg3Selector :: Selector '[] (Id NSArray)
arg3Selector = mkSelector "arg3"

-- | @Selector@ for @setArg3:@
setArg3Selector :: Selector '[Id NSArray] ()
setArg3Selector = mkSelector "setArg3:"

-- | @Selector@ for @arg4@
arg4Selector :: Selector '[] (Id NSArray)
arg4Selector = mkSelector "arg4"

-- | @Selector@ for @setArg4:@
setArg4Selector :: Selector '[Id NSArray] ()
setArg4Selector = mkSelector "setArg4:"

-- | @Selector@ for @arg5@
arg5Selector :: Selector '[] (Id NSNumber)
arg5Selector = mkSelector "arg5"

-- | @Selector@ for @setArg5:@
setArg5Selector :: Selector '[Id NSNumber] ()
setArg5Selector = mkSelector "setArg5:"

-- | @Selector@ for @arg6@
arg6Selector :: Selector '[] (Id NSNumber)
arg6Selector = mkSelector "arg6"

-- | @Selector@ for @setArg6:@
setArg6Selector :: Selector '[Id NSNumber] ()
setArg6Selector = mkSelector "setArg6:"

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

