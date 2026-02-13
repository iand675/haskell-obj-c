{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamAllocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamAllocateParams
  ( MTRCameraAVStreamManagementClusterVideoStreamAllocateParams
  , IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams(..)
  , streamUsage
  , setStreamUsage
  , videoCodec
  , setVideoCodec
  , minFrameRate
  , setMinFrameRate
  , maxFrameRate
  , setMaxFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , minBitRate
  , setMinBitRate
  , maxBitRate
  , setMaxBitRate
  , keyFrameInterval
  , setKeyFrameInterval
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , keyFrameIntervalSelector
  , maxBitRateSelector
  , maxFrameRateSelector
  , maxResolutionSelector
  , minBitRateSelector
  , minFrameRateSelector
  , minResolutionSelector
  , osdEnabledSelector
  , serverSideProcessingTimeoutSelector
  , setKeyFrameIntervalSelector
  , setMaxBitRateSelector
  , setMaxFrameRateSelector
  , setMaxResolutionSelector
  , setMinBitRateSelector
  , setMinFrameRateSelector
  , setMinResolutionSelector
  , setOsdEnabledSelector
  , setServerSideProcessingTimeoutSelector
  , setStreamUsageSelector
  , setTimedInvokeTimeoutMsSelector
  , setVideoCodecSelector
  , setWatermarkEnabledSelector
  , streamUsageSelector
  , timedInvokeTimeoutMsSelector
  , videoCodecSelector
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

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setStreamUsageSelector (toNSNumber value)

-- | @- videoCodec@
videoCodec :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
videoCodec mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams videoCodecSelector

-- | @- setVideoCodec:@
setVideoCodec :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setVideoCodec mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setVideoCodecSelector (toNSNumber value)

-- | @- minFrameRate@
minFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
minFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams minFrameRateSelector

-- | @- setMinFrameRate:@
setMinFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMinFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setMinFrameRateSelector (toNSNumber value)

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams maxFrameRateSelector

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setMaxFrameRateSelector (toNSNumber value)

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams minResolutionSelector

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setMinResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams maxResolutionSelector

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setMaxResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- minBitRate@
minBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
minBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams minBitRateSelector

-- | @- setMinBitRate:@
setMinBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMinBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setMinBitRateSelector (toNSNumber value)

-- | @- maxBitRate@
maxBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
maxBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams maxBitRateSelector

-- | @- setMaxBitRate:@
setMaxBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMaxBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setMaxBitRateSelector (toNSNumber value)

-- | @- keyFrameInterval@
keyFrameInterval :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
keyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams keyFrameIntervalSelector

-- | @- setKeyFrameInterval:@
setKeyFrameInterval :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setKeyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setKeyFrameIntervalSelector (toNSNumber value)

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams watermarkEnabledSelector

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setWatermarkEnabledSelector (toNSNumber value)

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams osdEnabledSelector

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setOsdEnabledSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector '[] (Id NSNumber)
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector '[Id NSNumber] ()
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @videoCodec@
videoCodecSelector :: Selector '[] (Id NSNumber)
videoCodecSelector = mkSelector "videoCodec"

-- | @Selector@ for @setVideoCodec:@
setVideoCodecSelector :: Selector '[Id NSNumber] ()
setVideoCodecSelector = mkSelector "setVideoCodec:"

-- | @Selector@ for @minFrameRate@
minFrameRateSelector :: Selector '[] (Id NSNumber)
minFrameRateSelector = mkSelector "minFrameRate"

-- | @Selector@ for @setMinFrameRate:@
setMinFrameRateSelector :: Selector '[Id NSNumber] ()
setMinFrameRateSelector = mkSelector "setMinFrameRate:"

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

-- | @Selector@ for @minBitRate@
minBitRateSelector :: Selector '[] (Id NSNumber)
minBitRateSelector = mkSelector "minBitRate"

-- | @Selector@ for @setMinBitRate:@
setMinBitRateSelector :: Selector '[Id NSNumber] ()
setMinBitRateSelector = mkSelector "setMinBitRate:"

-- | @Selector@ for @maxBitRate@
maxBitRateSelector :: Selector '[] (Id NSNumber)
maxBitRateSelector = mkSelector "maxBitRate"

-- | @Selector@ for @setMaxBitRate:@
setMaxBitRateSelector :: Selector '[Id NSNumber] ()
setMaxBitRateSelector = mkSelector "setMaxBitRate:"

-- | @Selector@ for @keyFrameInterval@
keyFrameIntervalSelector :: Selector '[] (Id NSNumber)
keyFrameIntervalSelector = mkSelector "keyFrameInterval"

-- | @Selector@ for @setKeyFrameInterval:@
setKeyFrameIntervalSelector :: Selector '[Id NSNumber] ()
setKeyFrameIntervalSelector = mkSelector "setKeyFrameInterval:"

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

