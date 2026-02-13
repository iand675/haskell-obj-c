{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams
  ( MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams(..)
  , videoStreamID
  , setVideoStreamID
  , deltaX
  , setDeltaX
  , deltaY
  , setDeltaY
  , zoomDelta
  , setZoomDelta
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , deltaXSelector
  , deltaYSelector
  , serverSideProcessingTimeoutSelector
  , setDeltaXSelector
  , setDeltaYSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setVideoStreamIDSelector
  , setZoomDeltaSelector
  , timedInvokeTimeoutMsSelector
  , videoStreamIDSelector
  , zoomDeltaSelector


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
videoStreamID :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setVideoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams setVideoStreamIDSelector (toNSNumber value)

-- | @- deltaX@
deltaX :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
deltaX mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams deltaXSelector

-- | @- setDeltaX:@
setDeltaX :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setDeltaX mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams setDeltaXSelector (toNSNumber value)

-- | @- deltaY@
deltaY :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
deltaY mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams deltaYSelector

-- | @- setDeltaY:@
setDeltaY :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setDeltaY mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams setDeltaYSelector (toNSNumber value)

-- | @- zoomDelta@
zoomDelta :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
zoomDelta mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams zoomDeltaSelector

-- | @- setZoomDelta:@
setZoomDelta :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setZoomDelta mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams setZoomDeltaSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @deltaX@
deltaXSelector :: Selector '[] (Id NSNumber)
deltaXSelector = mkSelector "deltaX"

-- | @Selector@ for @setDeltaX:@
setDeltaXSelector :: Selector '[Id NSNumber] ()
setDeltaXSelector = mkSelector "setDeltaX:"

-- | @Selector@ for @deltaY@
deltaYSelector :: Selector '[] (Id NSNumber)
deltaYSelector = mkSelector "deltaY"

-- | @Selector@ for @setDeltaY:@
setDeltaYSelector :: Selector '[Id NSNumber] ()
setDeltaYSelector = mkSelector "setDeltaY:"

-- | @Selector@ for @zoomDelta@
zoomDeltaSelector :: Selector '[] (Id NSNumber)
zoomDeltaSelector = mkSelector "zoomDelta"

-- | @Selector@ for @setZoomDelta:@
setZoomDeltaSelector :: Selector '[Id NSNumber] ()
setZoomDeltaSelector = mkSelector "setZoomDelta:"

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

