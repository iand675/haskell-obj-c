{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetTimeZoneParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetTimeZoneParams
  ( MTRTimeSynchronizationClusterSetTimeZoneParams
  , IsMTRTimeSynchronizationClusterSetTimeZoneParams(..)
  , timeZone
  , setTimeZone
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimeZoneSelector
  , setTimedInvokeTimeoutMsSelector
  , timeZoneSelector
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

-- | @- timeZone@
timeZone :: IsMTRTimeSynchronizationClusterSetTimeZoneParams mtrTimeSynchronizationClusterSetTimeZoneParams => mtrTimeSynchronizationClusterSetTimeZoneParams -> IO (Id NSArray)
timeZone mtrTimeSynchronizationClusterSetTimeZoneParams =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneParams timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: (IsMTRTimeSynchronizationClusterSetTimeZoneParams mtrTimeSynchronizationClusterSetTimeZoneParams, IsNSArray value) => mtrTimeSynchronizationClusterSetTimeZoneParams -> value -> IO ()
setTimeZone mtrTimeSynchronizationClusterSetTimeZoneParams value =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneParams setTimeZoneSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTimeSynchronizationClusterSetTimeZoneParams mtrTimeSynchronizationClusterSetTimeZoneParams => mtrTimeSynchronizationClusterSetTimeZoneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTimeSynchronizationClusterSetTimeZoneParams =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTimeSynchronizationClusterSetTimeZoneParams mtrTimeSynchronizationClusterSetTimeZoneParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetTimeZoneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTimeSynchronizationClusterSetTimeZoneParams value =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTimeSynchronizationClusterSetTimeZoneParams mtrTimeSynchronizationClusterSetTimeZoneParams => mtrTimeSynchronizationClusterSetTimeZoneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTimeSynchronizationClusterSetTimeZoneParams =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTimeSynchronizationClusterSetTimeZoneParams mtrTimeSynchronizationClusterSetTimeZoneParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetTimeZoneParams -> value -> IO ()
setServerSideProcessingTimeout mtrTimeSynchronizationClusterSetTimeZoneParams value =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSArray)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[Id NSArray] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

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

