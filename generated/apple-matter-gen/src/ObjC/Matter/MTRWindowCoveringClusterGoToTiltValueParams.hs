{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWindowCoveringClusterGoToTiltValueParams@.
module ObjC.Matter.MTRWindowCoveringClusterGoToTiltValueParams
  ( MTRWindowCoveringClusterGoToTiltValueParams
  , IsMTRWindowCoveringClusterGoToTiltValueParams(..)
  , tiltValue
  , setTiltValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTiltValueSelector
  , setTimedInvokeTimeoutMsSelector
  , tiltValueSelector
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

-- | @- tiltValue@
tiltValue :: IsMTRWindowCoveringClusterGoToTiltValueParams mtrWindowCoveringClusterGoToTiltValueParams => mtrWindowCoveringClusterGoToTiltValueParams -> IO (Id NSNumber)
tiltValue mtrWindowCoveringClusterGoToTiltValueParams =
  sendMessage mtrWindowCoveringClusterGoToTiltValueParams tiltValueSelector

-- | @- setTiltValue:@
setTiltValue :: (IsMTRWindowCoveringClusterGoToTiltValueParams mtrWindowCoveringClusterGoToTiltValueParams, IsNSNumber value) => mtrWindowCoveringClusterGoToTiltValueParams -> value -> IO ()
setTiltValue mtrWindowCoveringClusterGoToTiltValueParams value =
  sendMessage mtrWindowCoveringClusterGoToTiltValueParams setTiltValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWindowCoveringClusterGoToTiltValueParams mtrWindowCoveringClusterGoToTiltValueParams => mtrWindowCoveringClusterGoToTiltValueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWindowCoveringClusterGoToTiltValueParams =
  sendMessage mtrWindowCoveringClusterGoToTiltValueParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWindowCoveringClusterGoToTiltValueParams mtrWindowCoveringClusterGoToTiltValueParams, IsNSNumber value) => mtrWindowCoveringClusterGoToTiltValueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWindowCoveringClusterGoToTiltValueParams value =
  sendMessage mtrWindowCoveringClusterGoToTiltValueParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWindowCoveringClusterGoToTiltValueParams mtrWindowCoveringClusterGoToTiltValueParams => mtrWindowCoveringClusterGoToTiltValueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWindowCoveringClusterGoToTiltValueParams =
  sendMessage mtrWindowCoveringClusterGoToTiltValueParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWindowCoveringClusterGoToTiltValueParams mtrWindowCoveringClusterGoToTiltValueParams, IsNSNumber value) => mtrWindowCoveringClusterGoToTiltValueParams -> value -> IO ()
setServerSideProcessingTimeout mtrWindowCoveringClusterGoToTiltValueParams value =
  sendMessage mtrWindowCoveringClusterGoToTiltValueParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tiltValue@
tiltValueSelector :: Selector '[] (Id NSNumber)
tiltValueSelector = mkSelector "tiltValue"

-- | @Selector@ for @setTiltValue:@
setTiltValueSelector :: Selector '[Id NSNumber] ()
setTiltValueSelector = mkSelector "setTiltValue:"

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

