{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetDSTOffsetParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetDSTOffsetParams
  ( MTRTimeSynchronizationClusterSetDSTOffsetParams
  , IsMTRTimeSynchronizationClusterSetDSTOffsetParams(..)
  , dstOffset
  , setDstOffset
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dstOffsetSelector
  , serverSideProcessingTimeoutSelector
  , setDstOffsetSelector
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

-- | @- dstOffset@
dstOffset :: IsMTRTimeSynchronizationClusterSetDSTOffsetParams mtrTimeSynchronizationClusterSetDSTOffsetParams => mtrTimeSynchronizationClusterSetDSTOffsetParams -> IO (Id NSArray)
dstOffset mtrTimeSynchronizationClusterSetDSTOffsetParams =
  sendMessage mtrTimeSynchronizationClusterSetDSTOffsetParams dstOffsetSelector

-- | @- setDstOffset:@
setDstOffset :: (IsMTRTimeSynchronizationClusterSetDSTOffsetParams mtrTimeSynchronizationClusterSetDSTOffsetParams, IsNSArray value) => mtrTimeSynchronizationClusterSetDSTOffsetParams -> value -> IO ()
setDstOffset mtrTimeSynchronizationClusterSetDSTOffsetParams value =
  sendMessage mtrTimeSynchronizationClusterSetDSTOffsetParams setDstOffsetSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTimeSynchronizationClusterSetDSTOffsetParams mtrTimeSynchronizationClusterSetDSTOffsetParams => mtrTimeSynchronizationClusterSetDSTOffsetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTimeSynchronizationClusterSetDSTOffsetParams =
  sendMessage mtrTimeSynchronizationClusterSetDSTOffsetParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTimeSynchronizationClusterSetDSTOffsetParams mtrTimeSynchronizationClusterSetDSTOffsetParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetDSTOffsetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTimeSynchronizationClusterSetDSTOffsetParams value =
  sendMessage mtrTimeSynchronizationClusterSetDSTOffsetParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTimeSynchronizationClusterSetDSTOffsetParams mtrTimeSynchronizationClusterSetDSTOffsetParams => mtrTimeSynchronizationClusterSetDSTOffsetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTimeSynchronizationClusterSetDSTOffsetParams =
  sendMessage mtrTimeSynchronizationClusterSetDSTOffsetParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTimeSynchronizationClusterSetDSTOffsetParams mtrTimeSynchronizationClusterSetDSTOffsetParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetDSTOffsetParams -> value -> IO ()
setServerSideProcessingTimeout mtrTimeSynchronizationClusterSetDSTOffsetParams value =
  sendMessage mtrTimeSynchronizationClusterSetDSTOffsetParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dstOffset@
dstOffsetSelector :: Selector '[] (Id NSArray)
dstOffsetSelector = mkSelector "dstOffset"

-- | @Selector@ for @setDstOffset:@
setDstOffsetSelector :: Selector '[Id NSArray] ()
setDstOffsetSelector = mkSelector "setDstOffset:"

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

