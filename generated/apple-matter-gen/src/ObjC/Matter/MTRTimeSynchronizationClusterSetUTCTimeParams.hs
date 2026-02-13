{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetUTCTimeParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetUTCTimeParams
  ( MTRTimeSynchronizationClusterSetUTCTimeParams
  , IsMTRTimeSynchronizationClusterSetUTCTimeParams(..)
  , utcTime
  , setUtcTime
  , granularity
  , setGranularity
  , timeSource
  , setTimeSource
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , granularitySelector
  , serverSideProcessingTimeoutSelector
  , setGranularitySelector
  , setServerSideProcessingTimeoutSelector
  , setTimeSourceSelector
  , setTimedInvokeTimeoutMsSelector
  , setUtcTimeSelector
  , timeSourceSelector
  , timedInvokeTimeoutMsSelector
  , utcTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- utcTime@
utcTime :: IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams => mtrTimeSynchronizationClusterSetUTCTimeParams -> IO (Id NSNumber)
utcTime mtrTimeSynchronizationClusterSetUTCTimeParams =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams utcTimeSelector

-- | @- setUtcTime:@
setUtcTime :: (IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUTCTimeParams -> value -> IO ()
setUtcTime mtrTimeSynchronizationClusterSetUTCTimeParams value =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams setUtcTimeSelector (toNSNumber value)

-- | @- granularity@
granularity :: IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams => mtrTimeSynchronizationClusterSetUTCTimeParams -> IO (Id NSNumber)
granularity mtrTimeSynchronizationClusterSetUTCTimeParams =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams granularitySelector

-- | @- setGranularity:@
setGranularity :: (IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUTCTimeParams -> value -> IO ()
setGranularity mtrTimeSynchronizationClusterSetUTCTimeParams value =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams setGranularitySelector (toNSNumber value)

-- | @- timeSource@
timeSource :: IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams => mtrTimeSynchronizationClusterSetUTCTimeParams -> IO (Id NSNumber)
timeSource mtrTimeSynchronizationClusterSetUTCTimeParams =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams timeSourceSelector

-- | @- setTimeSource:@
setTimeSource :: (IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUTCTimeParams -> value -> IO ()
setTimeSource mtrTimeSynchronizationClusterSetUTCTimeParams value =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams setTimeSourceSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams => mtrTimeSynchronizationClusterSetUTCTimeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTimeSynchronizationClusterSetUTCTimeParams =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUTCTimeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTimeSynchronizationClusterSetUTCTimeParams value =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams => mtrTimeSynchronizationClusterSetUTCTimeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTimeSynchronizationClusterSetUTCTimeParams =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTimeSynchronizationClusterSetUTCTimeParams mtrTimeSynchronizationClusterSetUTCTimeParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetUTCTimeParams -> value -> IO ()
setServerSideProcessingTimeout mtrTimeSynchronizationClusterSetUTCTimeParams value =
  sendMessage mtrTimeSynchronizationClusterSetUTCTimeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @utcTime@
utcTimeSelector :: Selector '[] (Id NSNumber)
utcTimeSelector = mkSelector "utcTime"

-- | @Selector@ for @setUtcTime:@
setUtcTimeSelector :: Selector '[Id NSNumber] ()
setUtcTimeSelector = mkSelector "setUtcTime:"

-- | @Selector@ for @granularity@
granularitySelector :: Selector '[] (Id NSNumber)
granularitySelector = mkSelector "granularity"

-- | @Selector@ for @setGranularity:@
setGranularitySelector :: Selector '[Id NSNumber] ()
setGranularitySelector = mkSelector "setGranularity:"

-- | @Selector@ for @timeSource@
timeSourceSelector :: Selector '[] (Id NSNumber)
timeSourceSelector = mkSelector "timeSource"

-- | @Selector@ for @setTimeSource:@
setTimeSourceSelector :: Selector '[Id NSNumber] ()
setTimeSourceSelector = mkSelector "setTimeSource:"

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

