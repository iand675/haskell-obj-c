{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamModifyParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamModifyParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamModifyParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams(..)
  , snapshotStreamID
  , setSnapshotStreamID
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , osdEnabledSelector
  , serverSideProcessingTimeoutSelector
  , setOsdEnabledSelector
  , setServerSideProcessingTimeoutSelector
  , setSnapshotStreamIDSelector
  , setTimedInvokeTimeoutMsSelector
  , setWatermarkEnabledSelector
  , snapshotStreamIDSelector
  , timedInvokeTimeoutMsSelector
  , watermarkEnabledSelector


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
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams snapshotStreamIDSelector

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams setSnapshotStreamIDSelector (toNSNumber value)

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams watermarkEnabledSelector

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams setWatermarkEnabledSelector (toNSNumber value)

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams osdEnabledSelector

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams setOsdEnabledSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamModifyParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector '[] (Id NSNumber)
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector '[Id NSNumber] ()
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

-- | @Selector@ for @watermarkEnabled@
watermarkEnabledSelector :: Selector '[] (Id NSNumber)
watermarkEnabledSelector = mkSelector "watermarkEnabled"

-- | @Selector@ for @setWatermarkEnabled:@
setWatermarkEnabledSelector :: Selector '[Id NSNumber] ()
setWatermarkEnabledSelector = mkSelector "setWatermarkEnabled:"

-- | @Selector@ for @osdEnabled@
osdEnabledSelector :: Selector '[] (Id NSNumber)
osdEnabledSelector = mkSelector "osdEnabled"

-- | @Selector@ for @setOsdEnabled:@
setOsdEnabledSelector :: Selector '[Id NSNumber] ()
setOsdEnabledSelector = mkSelector "setOsdEnabled:"

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

