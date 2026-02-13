{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamModifyParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamModifyParams
  ( MTRCameraAVStreamManagementClusterVideoStreamModifyParams
  , IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams(..)
  , videoStreamID
  , setVideoStreamID
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
  , setTimedInvokeTimeoutMsSelector
  , setVideoStreamIDSelector
  , setWatermarkEnabledSelector
  , timedInvokeTimeoutMsSelector
  , videoStreamIDSelector
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

-- | @- videoStreamID@
videoStreamID :: IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVStreamManagementClusterVideoStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> value -> IO ()
setVideoStreamID mtrCameraAVStreamManagementClusterVideoStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams setVideoStreamIDSelector (toNSNumber value)

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams watermarkEnabledSelector

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams setWatermarkEnabledSelector (toNSNumber value)

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterVideoStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams osdEnabledSelector

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterVideoStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams setOsdEnabledSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamModifyParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams mtrCameraAVStreamManagementClusterVideoStreamModifyParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamModifyParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamModifyParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamModifyParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

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

