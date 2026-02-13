{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams(..)
  , imageCodec
  , setImageCodec
  , maxFrameRate
  , setMaxFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , quality
  , setQuality
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , imageCodecSelector
  , maxFrameRateSelector
  , maxResolutionSelector
  , minResolutionSelector
  , osdEnabledSelector
  , qualitySelector
  , serverSideProcessingTimeoutSelector
  , setImageCodecSelector
  , setMaxFrameRateSelector
  , setMaxResolutionSelector
  , setMinResolutionSelector
  , setOsdEnabledSelector
  , setQualitySelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setWatermarkEnabledSelector
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

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams imageCodecSelector

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setImageCodecSelector (toNSNumber value)

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams maxFrameRateSelector

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setMaxFrameRateSelector (toNSNumber value)

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams minResolutionSelector

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setMinResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams maxResolutionSelector

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setMaxResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- quality@
quality :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
quality mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams qualitySelector

-- | @- setQuality:@
setQuality :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setQuality mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setQualitySelector (toNSNumber value)

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams watermarkEnabledSelector

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setWatermarkEnabledSelector (toNSNumber value)

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams osdEnabledSelector

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setOsdEnabledSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector '[] (Id NSNumber)
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector '[Id NSNumber] ()
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector '[] (Id NSNumber)
maxFrameRateSelector = mkSelector "maxFrameRate"

-- | @Selector@ for @setMaxFrameRate:@
setMaxFrameRateSelector :: Selector '[Id NSNumber] ()
setMaxFrameRateSelector = mkSelector "setMaxFrameRate:"

-- | @Selector@ for @minResolution@
minResolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolutionSelector = mkSelector "minResolution"

-- | @Selector@ for @setMinResolution:@
setMinResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setMinResolutionSelector = mkSelector "setMinResolution:"

-- | @Selector@ for @maxResolution@
maxResolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolutionSelector = mkSelector "maxResolution"

-- | @Selector@ for @setMaxResolution:@
setMaxResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setMaxResolutionSelector = mkSelector "setMaxResolution:"

-- | @Selector@ for @quality@
qualitySelector :: Selector '[] (Id NSNumber)
qualitySelector = mkSelector "quality"

-- | @Selector@ for @setQuality:@
setQualitySelector :: Selector '[Id NSNumber] ()
setQualitySelector = mkSelector "setQuality:"

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

