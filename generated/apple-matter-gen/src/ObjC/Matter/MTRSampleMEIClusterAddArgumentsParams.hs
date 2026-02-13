{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSampleMEIClusterAddArgumentsParams@.
module ObjC.Matter.MTRSampleMEIClusterAddArgumentsParams
  ( MTRSampleMEIClusterAddArgumentsParams
  , IsMTRSampleMEIClusterAddArgumentsParams(..)
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
arg1 :: IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams => mtrSampleMEIClusterAddArgumentsParams -> IO (Id NSNumber)
arg1 mtrSampleMEIClusterAddArgumentsParams =
  sendMessage mtrSampleMEIClusterAddArgumentsParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams, IsNSNumber value) => mtrSampleMEIClusterAddArgumentsParams -> value -> IO ()
setArg1 mtrSampleMEIClusterAddArgumentsParams value =
  sendMessage mtrSampleMEIClusterAddArgumentsParams setArg1Selector (toNSNumber value)

-- | @- arg2@
arg2 :: IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams => mtrSampleMEIClusterAddArgumentsParams -> IO (Id NSNumber)
arg2 mtrSampleMEIClusterAddArgumentsParams =
  sendMessage mtrSampleMEIClusterAddArgumentsParams arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams, IsNSNumber value) => mtrSampleMEIClusterAddArgumentsParams -> value -> IO ()
setArg2 mtrSampleMEIClusterAddArgumentsParams value =
  sendMessage mtrSampleMEIClusterAddArgumentsParams setArg2Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams => mtrSampleMEIClusterAddArgumentsParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrSampleMEIClusterAddArgumentsParams =
  sendMessage mtrSampleMEIClusterAddArgumentsParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams, IsNSNumber value) => mtrSampleMEIClusterAddArgumentsParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrSampleMEIClusterAddArgumentsParams value =
  sendMessage mtrSampleMEIClusterAddArgumentsParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams => mtrSampleMEIClusterAddArgumentsParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrSampleMEIClusterAddArgumentsParams =
  sendMessage mtrSampleMEIClusterAddArgumentsParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRSampleMEIClusterAddArgumentsParams mtrSampleMEIClusterAddArgumentsParams, IsNSNumber value) => mtrSampleMEIClusterAddArgumentsParams -> value -> IO ()
setServerSideProcessingTimeout mtrSampleMEIClusterAddArgumentsParams value =
  sendMessage mtrSampleMEIClusterAddArgumentsParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

