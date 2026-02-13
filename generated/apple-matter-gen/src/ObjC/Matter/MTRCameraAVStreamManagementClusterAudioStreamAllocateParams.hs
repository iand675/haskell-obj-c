{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioStreamAllocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioStreamAllocateParams
  ( MTRCameraAVStreamManagementClusterAudioStreamAllocateParams
  , IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams(..)
  , streamUsage
  , setStreamUsage
  , audioCodec
  , setAudioCodec
  , channelCount
  , setChannelCount
  , sampleRate
  , setSampleRate
  , bitRate
  , setBitRate
  , bitDepth
  , setBitDepth
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , audioCodecSelector
  , bitDepthSelector
  , bitRateSelector
  , channelCountSelector
  , sampleRateSelector
  , serverSideProcessingTimeoutSelector
  , setAudioCodecSelector
  , setBitDepthSelector
  , setBitRateSelector
  , setChannelCountSelector
  , setSampleRateSelector
  , setServerSideProcessingTimeoutSelector
  , setStreamUsageSelector
  , setTimedInvokeTimeoutMsSelector
  , streamUsageSelector
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

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setStreamUsageSelector (toNSNumber value)

-- | @- audioCodec@
audioCodec :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
audioCodec mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams audioCodecSelector

-- | @- setAudioCodec:@
setAudioCodec :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setAudioCodec mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setAudioCodecSelector (toNSNumber value)

-- | @- channelCount@
channelCount :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
channelCount mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams channelCountSelector

-- | @- setChannelCount:@
setChannelCount :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setChannelCount mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setChannelCountSelector (toNSNumber value)

-- | @- sampleRate@
sampleRate :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
sampleRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams sampleRateSelector

-- | @- setSampleRate:@
setSampleRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setSampleRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setSampleRateSelector (toNSNumber value)

-- | @- bitRate@
bitRate :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
bitRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams bitRateSelector

-- | @- setBitRate:@
setBitRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setBitRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setBitRateSelector (toNSNumber value)

-- | @- bitDepth@
bitDepth :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
bitDepth mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams bitDepthSelector

-- | @- setBitDepth:@
setBitDepth :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setBitDepth mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setBitDepthSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterAudioStreamAllocateParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterAudioStreamAllocateParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector '[] (Id NSNumber)
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector '[Id NSNumber] ()
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @audioCodec@
audioCodecSelector :: Selector '[] (Id NSNumber)
audioCodecSelector = mkSelector "audioCodec"

-- | @Selector@ for @setAudioCodec:@
setAudioCodecSelector :: Selector '[Id NSNumber] ()
setAudioCodecSelector = mkSelector "setAudioCodec:"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] (Id NSNumber)
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @setChannelCount:@
setChannelCountSelector :: Selector '[Id NSNumber] ()
setChannelCountSelector = mkSelector "setChannelCount:"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector '[] (Id NSNumber)
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @setSampleRate:@
setSampleRateSelector :: Selector '[Id NSNumber] ()
setSampleRateSelector = mkSelector "setSampleRate:"

-- | @Selector@ for @bitRate@
bitRateSelector :: Selector '[] (Id NSNumber)
bitRateSelector = mkSelector "bitRate"

-- | @Selector@ for @setBitRate:@
setBitRateSelector :: Selector '[Id NSNumber] ()
setBitRateSelector = mkSelector "setBitRate:"

-- | @Selector@ for @bitDepth@
bitDepthSelector :: Selector '[] (Id NSNumber)
bitDepthSelector = mkSelector "bitDepth"

-- | @Selector@ for @setBitDepth:@
setBitDepthSelector :: Selector '[Id NSNumber] ()
setBitDepthSelector = mkSelector "setBitDepth:"

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

