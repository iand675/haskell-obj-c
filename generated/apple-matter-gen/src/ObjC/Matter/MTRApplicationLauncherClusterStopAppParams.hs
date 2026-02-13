{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterStopAppParams@.
module ObjC.Matter.MTRApplicationLauncherClusterStopAppParams
  ( MTRApplicationLauncherClusterStopAppParams
  , IsMTRApplicationLauncherClusterStopAppParams(..)
  , application
  , setApplication
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , applicationSelector
  , serverSideProcessingTimeoutSelector
  , setApplicationSelector
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

-- | @- application@
application :: IsMTRApplicationLauncherClusterStopAppParams mtrApplicationLauncherClusterStopAppParams => mtrApplicationLauncherClusterStopAppParams -> IO (Id MTRApplicationLauncherClusterApplicationStruct)
application mtrApplicationLauncherClusterStopAppParams =
  sendMessage mtrApplicationLauncherClusterStopAppParams applicationSelector

-- | @- setApplication:@
setApplication :: (IsMTRApplicationLauncherClusterStopAppParams mtrApplicationLauncherClusterStopAppParams, IsMTRApplicationLauncherClusterApplicationStruct value) => mtrApplicationLauncherClusterStopAppParams -> value -> IO ()
setApplication mtrApplicationLauncherClusterStopAppParams value =
  sendMessage mtrApplicationLauncherClusterStopAppParams setApplicationSelector (toMTRApplicationLauncherClusterApplicationStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRApplicationLauncherClusterStopAppParams mtrApplicationLauncherClusterStopAppParams => mtrApplicationLauncherClusterStopAppParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrApplicationLauncherClusterStopAppParams =
  sendMessage mtrApplicationLauncherClusterStopAppParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRApplicationLauncherClusterStopAppParams mtrApplicationLauncherClusterStopAppParams, IsNSNumber value) => mtrApplicationLauncherClusterStopAppParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrApplicationLauncherClusterStopAppParams value =
  sendMessage mtrApplicationLauncherClusterStopAppParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRApplicationLauncherClusterStopAppParams mtrApplicationLauncherClusterStopAppParams => mtrApplicationLauncherClusterStopAppParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrApplicationLauncherClusterStopAppParams =
  sendMessage mtrApplicationLauncherClusterStopAppParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRApplicationLauncherClusterStopAppParams mtrApplicationLauncherClusterStopAppParams, IsNSNumber value) => mtrApplicationLauncherClusterStopAppParams -> value -> IO ()
setServerSideProcessingTimeout mtrApplicationLauncherClusterStopAppParams value =
  sendMessage mtrApplicationLauncherClusterStopAppParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @application@
applicationSelector :: Selector '[] (Id MTRApplicationLauncherClusterApplicationStruct)
applicationSelector = mkSelector "application"

-- | @Selector@ for @setApplication:@
setApplicationSelector :: Selector '[Id MTRApplicationLauncherClusterApplicationStruct] ()
setApplicationSelector = mkSelector "setApplication:"

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

