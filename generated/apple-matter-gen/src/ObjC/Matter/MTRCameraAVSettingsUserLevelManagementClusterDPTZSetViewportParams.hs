{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams
  ( MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams(..)
  , videoStreamID
  , setVideoStreamID
  , viewport
  , setViewport
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setVideoStreamIDSelector
  , setViewportSelector
  , timedInvokeTimeoutMsSelector
  , videoStreamIDSelector
  , viewportSelector


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
videoStreamID :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setVideoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams setVideoStreamIDSelector (toNSNumber value)

-- | @- viewport@
viewport :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id MTRDataTypeViewportStruct)
viewport mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams viewportSelector

-- | @- setViewport:@
setViewport :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsMTRDataTypeViewportStruct value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setViewport mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams setViewportSelector (toMTRDataTypeViewportStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @viewport@
viewportSelector :: Selector '[] (Id MTRDataTypeViewportStruct)
viewportSelector = mkSelector "viewport"

-- | @Selector@ for @setViewport:@
setViewportSelector :: Selector '[Id MTRDataTypeViewportStruct] ()
setViewportSelector = mkSelector "setViewport:"

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

