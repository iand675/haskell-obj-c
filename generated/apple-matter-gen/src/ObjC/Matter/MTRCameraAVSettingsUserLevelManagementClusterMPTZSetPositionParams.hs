{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams(..)
  , pan
  , setPan
  , tilt
  , setTilt
  , zoom
  , setZoom
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , panSelector
  , serverSideProcessingTimeoutSelector
  , setPanSelector
  , setServerSideProcessingTimeoutSelector
  , setTiltSelector
  , setTimedInvokeTimeoutMsSelector
  , setZoomSelector
  , tiltSelector
  , timedInvokeTimeoutMsSelector
  , zoomSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pan@
pan :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
pan mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams panSelector

-- | @- setPan:@
setPan :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setPan mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams setPanSelector (toNSNumber value)

-- | @- tilt@
tilt :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
tilt mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams tiltSelector

-- | @- setTilt:@
setTilt :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setTilt mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams setTiltSelector (toNSNumber value)

-- | @- zoom@
zoom :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
zoom mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams zoomSelector

-- | @- setZoom:@
setZoom :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setZoom mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams setZoomSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pan@
panSelector :: Selector '[] (Id NSNumber)
panSelector = mkSelector "pan"

-- | @Selector@ for @setPan:@
setPanSelector :: Selector '[Id NSNumber] ()
setPanSelector = mkSelector "setPan:"

-- | @Selector@ for @tilt@
tiltSelector :: Selector '[] (Id NSNumber)
tiltSelector = mkSelector "tilt"

-- | @Selector@ for @setTilt:@
setTiltSelector :: Selector '[Id NSNumber] ()
setTiltSelector = mkSelector "setTilt:"

-- | @Selector@ for @zoom@
zoomSelector :: Selector '[] (Id NSNumber)
zoomSelector = mkSelector "zoom"

-- | @Selector@ for @setZoom:@
setZoomSelector :: Selector '[Id NSNumber] ()
setZoomSelector = mkSelector "setZoom:"

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

