{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterHideAppParams@.
module ObjC.Matter.MTRApplicationLauncherClusterHideAppParams
  ( MTRApplicationLauncherClusterHideAppParams
  , IsMTRApplicationLauncherClusterHideAppParams(..)
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
application :: IsMTRApplicationLauncherClusterHideAppParams mtrApplicationLauncherClusterHideAppParams => mtrApplicationLauncherClusterHideAppParams -> IO (Id MTRApplicationLauncherClusterApplicationStruct)
application mtrApplicationLauncherClusterHideAppParams =
  sendMessage mtrApplicationLauncherClusterHideAppParams applicationSelector

-- | @- setApplication:@
setApplication :: (IsMTRApplicationLauncherClusterHideAppParams mtrApplicationLauncherClusterHideAppParams, IsMTRApplicationLauncherClusterApplicationStruct value) => mtrApplicationLauncherClusterHideAppParams -> value -> IO ()
setApplication mtrApplicationLauncherClusterHideAppParams value =
  sendMessage mtrApplicationLauncherClusterHideAppParams setApplicationSelector (toMTRApplicationLauncherClusterApplicationStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRApplicationLauncherClusterHideAppParams mtrApplicationLauncherClusterHideAppParams => mtrApplicationLauncherClusterHideAppParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrApplicationLauncherClusterHideAppParams =
  sendMessage mtrApplicationLauncherClusterHideAppParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRApplicationLauncherClusterHideAppParams mtrApplicationLauncherClusterHideAppParams, IsNSNumber value) => mtrApplicationLauncherClusterHideAppParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrApplicationLauncherClusterHideAppParams value =
  sendMessage mtrApplicationLauncherClusterHideAppParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRApplicationLauncherClusterHideAppParams mtrApplicationLauncherClusterHideAppParams => mtrApplicationLauncherClusterHideAppParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrApplicationLauncherClusterHideAppParams =
  sendMessage mtrApplicationLauncherClusterHideAppParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRApplicationLauncherClusterHideAppParams mtrApplicationLauncherClusterHideAppParams, IsNSNumber value) => mtrApplicationLauncherClusterHideAppParams -> value -> IO ()
setServerSideProcessingTimeout mtrApplicationLauncherClusterHideAppParams value =
  sendMessage mtrApplicationLauncherClusterHideAppParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

