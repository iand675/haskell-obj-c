{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSetStreamPrioritiesParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSetStreamPrioritiesParams
  ( MTRCameraAVStreamManagementClusterSetStreamPrioritiesParams
  , IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams(..)
  , streamPriorities
  , setStreamPriorities
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setStreamPrioritiesSelector
  , setTimedInvokeTimeoutMsSelector
  , streamPrioritiesSelector
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

-- | @- streamPriorities@
streamPriorities :: IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams => mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams -> IO (Id NSArray)
streamPriorities mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams =
  sendMessage mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams streamPrioritiesSelector

-- | @- setStreamPriorities:@
setStreamPriorities :: (IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams, IsNSArray value) => mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams -> value -> IO ()
setStreamPriorities mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams value =
  sendMessage mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams setStreamPrioritiesSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams => mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams =
  sendMessage mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams value =
  sendMessage mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams => mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams =
  sendMessage mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams value =
  sendMessage mtrCameraAVStreamManagementClusterSetStreamPrioritiesParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamPriorities@
streamPrioritiesSelector :: Selector '[] (Id NSArray)
streamPrioritiesSelector = mkSelector "streamPriorities"

-- | @Selector@ for @setStreamPriorities:@
setStreamPrioritiesSelector :: Selector '[Id NSArray] ()
setStreamPrioritiesSelector = mkSelector "setStreamPriorities:"

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

