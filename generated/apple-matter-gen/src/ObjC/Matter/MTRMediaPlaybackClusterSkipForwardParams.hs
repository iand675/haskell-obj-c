{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterSkipForwardParams@.
module ObjC.Matter.MTRMediaPlaybackClusterSkipForwardParams
  ( MTRMediaPlaybackClusterSkipForwardParams
  , IsMTRMediaPlaybackClusterSkipForwardParams(..)
  , deltaPositionMilliseconds
  , setDeltaPositionMilliseconds
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , deltaPositionMillisecondsSelector
  , serverSideProcessingTimeoutSelector
  , setDeltaPositionMillisecondsSelector
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

-- | @- deltaPositionMilliseconds@
deltaPositionMilliseconds :: IsMTRMediaPlaybackClusterSkipForwardParams mtrMediaPlaybackClusterSkipForwardParams => mtrMediaPlaybackClusterSkipForwardParams -> IO (Id NSNumber)
deltaPositionMilliseconds mtrMediaPlaybackClusterSkipForwardParams =
  sendMessage mtrMediaPlaybackClusterSkipForwardParams deltaPositionMillisecondsSelector

-- | @- setDeltaPositionMilliseconds:@
setDeltaPositionMilliseconds :: (IsMTRMediaPlaybackClusterSkipForwardParams mtrMediaPlaybackClusterSkipForwardParams, IsNSNumber value) => mtrMediaPlaybackClusterSkipForwardParams -> value -> IO ()
setDeltaPositionMilliseconds mtrMediaPlaybackClusterSkipForwardParams value =
  sendMessage mtrMediaPlaybackClusterSkipForwardParams setDeltaPositionMillisecondsSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMediaPlaybackClusterSkipForwardParams mtrMediaPlaybackClusterSkipForwardParams => mtrMediaPlaybackClusterSkipForwardParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMediaPlaybackClusterSkipForwardParams =
  sendMessage mtrMediaPlaybackClusterSkipForwardParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMediaPlaybackClusterSkipForwardParams mtrMediaPlaybackClusterSkipForwardParams, IsNSNumber value) => mtrMediaPlaybackClusterSkipForwardParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMediaPlaybackClusterSkipForwardParams value =
  sendMessage mtrMediaPlaybackClusterSkipForwardParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMediaPlaybackClusterSkipForwardParams mtrMediaPlaybackClusterSkipForwardParams => mtrMediaPlaybackClusterSkipForwardParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMediaPlaybackClusterSkipForwardParams =
  sendMessage mtrMediaPlaybackClusterSkipForwardParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMediaPlaybackClusterSkipForwardParams mtrMediaPlaybackClusterSkipForwardParams, IsNSNumber value) => mtrMediaPlaybackClusterSkipForwardParams -> value -> IO ()
setServerSideProcessingTimeout mtrMediaPlaybackClusterSkipForwardParams value =
  sendMessage mtrMediaPlaybackClusterSkipForwardParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deltaPositionMilliseconds@
deltaPositionMillisecondsSelector :: Selector '[] (Id NSNumber)
deltaPositionMillisecondsSelector = mkSelector "deltaPositionMilliseconds"

-- | @Selector@ for @setDeltaPositionMilliseconds:@
setDeltaPositionMillisecondsSelector :: Selector '[Id NSNumber] ()
setDeltaPositionMillisecondsSelector = mkSelector "setDeltaPositionMilliseconds:"

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

