{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams(..)
  , snapshotStreamID
  , setSnapshotStreamID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setSnapshotStreamIDSelector
  , setTimedInvokeTimeoutMsSelector
  , snapshotStreamIDSelector
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

-- | @- snapshotStreamID@
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams snapshotStreamIDSelector

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams setSnapshotStreamIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamDeallocateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector '[] (Id NSNumber)
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector '[Id NSNumber] ()
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

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

