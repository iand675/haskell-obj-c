{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioPlayerNode
--
-- Play buffers or segments of audio files.
--
-- AVAudioPlayerNode supports scheduling the playback of @AVAudioBuffer@ instances,		or segments of audio files opened via @AVAudioFile@. Buffers and segments may be		scheduled at specific points in time, or to play immediately following preceding segments.
--
-- FORMATS
--
-- Normally, you will want to configure the node's output format with the same number of		channels as are in the files and buffers to be played. Otherwise, channels will be dropped		or added as required. It is usually better to use an @AVAudioMixerNode@ to		do this.
--
-- Similarly, when playing file segments, the node will sample rate convert if necessary, but		it is often preferable to configure the node's output sample rate to match that of the file(s)		and use a mixer to perform the rate conversion.
--
-- When playing buffers, there is an implicit assumption that the buffers are at the same		sample rate as the node's output format.
--
-- TIMELINES
--
-- The usual @AVAudioNode@ sample times (as observed by @lastRenderTime@)		have an arbitrary zero point. AVAudioPlayerNode superimposes a second "player timeline" on		top of this, to reflect when the player was started, and intervals during which it was		paused. The methods @nodeTimeForPlayerTime:@ and @playerTimeForNodeTime:@		convert between the two.
--
-- This class' @stop@ method unschedules all previously scheduled buffers and		file segments, and returns the player timeline to sample time 0.
--
-- TIMESTAMPS
--
-- The "schedule" methods all take an @AVAudioTime@ "when" parameter. This is		interpreted as follows:
--
-- 1. nil:			- if there have been previous commands, the new one is played immediately following the				last one.			- otherwise, if the node is playing, the event is played in the very near future.			- otherwise, the command is played at sample time 0.		2. sample time:			- relative to the node's start time (which begins at 0 when the node is started).		3. host time:			- ignored unless the sample time is invalid when the engine is rendering to an audio 			  device.			- ignored in manual rendering mode.
--
-- ERRORS
--
-- The "schedule" methods can fail if:
--
-- 1. a buffer's channel count does not match that of the node's output format.		2. a file can't be accessed.		3. an AVAudioTime specifies neither a valid sample time or host time.		4. a segment's start frame or frame count is negative.
--
-- BUFFER/FILE COMPLETION HANDLERS
--
-- The buffer or file completion handlers (see scheduling methods) are a means to schedule 		more data if available on the player node. See @AVAudioPlayerNodeCompletionCallbackType@ 		for details on the different buffer/file completion callback types.
--
-- Note that a player should not be stopped from within a completion handler callback because		it can deadlock while trying to unschedule previously scheduled buffers.
--
-- OFFLINE RENDERING
--
-- When a player node is used with the engine operating in the manual rendering mode, the		buffer/file completion handlers, @lastRenderTime@ and the latencies (@latency@ and		@outputPresentationLatency@) can be used to track how much data the player has rendered and		how much more data is left to render.
--
-- Generated bindings for @AVAudioPlayerNode@.
module ObjC.AVFAudio.AVAudioPlayerNode
  ( AVAudioPlayerNode
  , IsAVAudioPlayerNode(..)
  , init_
  , scheduleBuffer_completionHandler
  , scheduleBuffer_completionCallbackType_completionHandler
  , scheduleBuffer_atTime_options_completionHandler
  , scheduleBuffer_atTime_options_completionCallbackType_completionHandler
  , scheduleFile_atTime_completionHandler
  , scheduleFile_atTime_completionCallbackType_completionHandler
  , scheduleSegment_startingFrame_frameCount_atTime_completionHandler
  , scheduleSegment_startingFrame_frameCount_atTime_completionCallbackType_completionHandler
  , stop
  , prepareWithFrameCount
  , play
  , playAtTime
  , pause
  , nodeTimeForPlayerTime
  , playerTimeForNodeTime
  , playing
  , initSelector
  , scheduleBuffer_completionHandlerSelector
  , scheduleBuffer_completionCallbackType_completionHandlerSelector
  , scheduleBuffer_atTime_options_completionHandlerSelector
  , scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector
  , scheduleFile_atTime_completionHandlerSelector
  , scheduleFile_atTime_completionCallbackType_completionHandlerSelector
  , scheduleSegment_startingFrame_frameCount_atTime_completionHandlerSelector
  , scheduleSegment_startingFrame_frameCount_atTime_completionCallbackType_completionHandlerSelector
  , stopSelector
  , prepareWithFrameCountSelector
  , playSelector
  , playAtTimeSelector
  , pauseSelector
  , nodeTimeForPlayerTimeSelector
  , playerTimeForNodeTimeSelector
  , playingSelector

  -- * Enum types
  , AVAudioPlayerNodeBufferOptions(AVAudioPlayerNodeBufferOptions)
  , pattern AVAudioPlayerNodeBufferLoops
  , pattern AVAudioPlayerNodeBufferInterrupts
  , pattern AVAudioPlayerNodeBufferInterruptsAtLoop
  , AVAudioPlayerNodeCompletionCallbackType(AVAudioPlayerNodeCompletionCallbackType)
  , pattern AVAudioPlayerNodeCompletionDataConsumed
  , pattern AVAudioPlayerNodeCompletionDataRendered
  , pattern AVAudioPlayerNodeCompletionDataPlayedBack

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioPlayerNode avAudioPlayerNode => avAudioPlayerNode -> IO (Id AVAudioPlayerNode)
init_ avAudioPlayerNode  =
  sendMsg avAudioPlayerNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | scheduleBuffer:completionHandler:
--
-- Schedule playing samples from an AVAudioBuffer.
--
-- @buffer@ — the buffer to play
--
-- @completionHandler@ — called after the buffer has been consumed by the player or the player is stopped. may be nil.
--
-- Schedules the buffer to be played following any previously scheduled commands.
--
-- It is possible for the completionHandler to be called before rendering begins		or before the buffer is played completely.
--
-- ObjC selector: @- scheduleBuffer:completionHandler:@
scheduleBuffer_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioPCMBuffer buffer) => avAudioPlayerNode -> buffer -> Ptr () -> IO ()
scheduleBuffer_completionHandler avAudioPlayerNode  buffer completionHandler =
withObjCPtr buffer $ \raw_buffer ->
    sendMsg avAudioPlayerNode (mkSelector "scheduleBuffer:completionHandler:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleBuffer:completionCallbackType:completionHandler:
--
-- Schedule playing samples from an AVAudioBuffer.
--
-- @buffer@ — the buffer to play
--
-- @callbackType@ — option to specify when the completion handler must be called
--
-- @completionHandler@ — called after the buffer has been consumed by the player or has finished playing back or 		the player is stopped. may be nil.
--
-- Schedules the buffer to be played following any previously scheduled commands.
--
-- ObjC selector: @- scheduleBuffer:completionCallbackType:completionHandler:@
scheduleBuffer_completionCallbackType_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioPCMBuffer buffer) => avAudioPlayerNode -> buffer -> AVAudioPlayerNodeCompletionCallbackType -> Ptr () -> IO ()
scheduleBuffer_completionCallbackType_completionHandler avAudioPlayerNode  buffer callbackType completionHandler =
withObjCPtr buffer $ \raw_buffer ->
    sendMsg avAudioPlayerNode (mkSelector "scheduleBuffer:completionCallbackType:completionHandler:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argCLong (coerce callbackType), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleBuffer:atTime:options:completionHandler:
--
-- Schedule playing samples from an AVAudioBuffer.
--
-- @buffer@ — the buffer to play
--
-- @when@ — the time at which to play the buffer. see the discussion of timestamps, above.
--
-- @options@ — options for looping, interrupting other buffers, etc.
--
-- @completionHandler@ — called after the buffer has been consumed by the player or the player is stopped. may be nil.
--
-- It is possible for the completionHandler to be called before rendering begins		or before the buffer is played completely.
--
-- ObjC selector: @- scheduleBuffer:atTime:options:completionHandler:@
scheduleBuffer_atTime_options_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioPCMBuffer buffer, IsAVAudioTime when) => avAudioPlayerNode -> buffer -> when -> AVAudioPlayerNodeBufferOptions -> Ptr () -> IO ()
scheduleBuffer_atTime_options_completionHandler avAudioPlayerNode  buffer when options completionHandler =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr when $ \raw_when ->
      sendMsg avAudioPlayerNode (mkSelector "scheduleBuffer:atTime:options:completionHandler:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_when :: Ptr ()), argCULong (coerce options), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleBuffer:atTime:options:completionCallbackType:completionHandler:
--
-- Schedule playing samples from an AVAudioBuffer.
--
-- @buffer@ — the buffer to play
--
-- @when@ — the time at which to play the buffer. see the discussion of timestamps, above.
--
-- @options@ — options for looping, interrupting other buffers, etc.
--
-- @callbackType@ — option to specify when the completion handler must be called
--
-- @completionHandler@ — called after the buffer has been consumed by the player or has finished playing back or 		the player is stopped. may be nil.
--
-- ObjC selector: @- scheduleBuffer:atTime:options:completionCallbackType:completionHandler:@
scheduleBuffer_atTime_options_completionCallbackType_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioPCMBuffer buffer, IsAVAudioTime when) => avAudioPlayerNode -> buffer -> when -> AVAudioPlayerNodeBufferOptions -> AVAudioPlayerNodeCompletionCallbackType -> Ptr () -> IO ()
scheduleBuffer_atTime_options_completionCallbackType_completionHandler avAudioPlayerNode  buffer when options callbackType completionHandler =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr when $ \raw_when ->
      sendMsg avAudioPlayerNode (mkSelector "scheduleBuffer:atTime:options:completionCallbackType:completionHandler:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_when :: Ptr ()), argCULong (coerce options), argCLong (coerce callbackType), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleFile:atTime:completionHandler:
--
-- Schedule playing of an entire audio file.
--
-- @file@ — the file to play
--
-- @when@ — the time at which to play the file. see the discussion of timestamps, above.
--
-- @completionHandler@ — called after the file has been consumed by the player or the player is stopped. may be nil.
--
-- It is possible for the completionHandler to be called before rendering begins		or before the file is played completely.
--
-- ObjC selector: @- scheduleFile:atTime:completionHandler:@
scheduleFile_atTime_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioFile file, IsAVAudioTime when) => avAudioPlayerNode -> file -> when -> Ptr () -> IO ()
scheduleFile_atTime_completionHandler avAudioPlayerNode  file when completionHandler =
withObjCPtr file $ \raw_file ->
  withObjCPtr when $ \raw_when ->
      sendMsg avAudioPlayerNode (mkSelector "scheduleFile:atTime:completionHandler:") retVoid [argPtr (castPtr raw_file :: Ptr ()), argPtr (castPtr raw_when :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleFile:atTime:completionCallbackType:completionHandler:
--
-- Schedule playing of an entire audio file.
--
-- @file@ — the file to play
--
-- @when@ — the time at which to play the file. see the discussion of timestamps, above.
--
-- @callbackType@ — option to specify when the completion handler must be called
--
-- @completionHandler@ — called after the file has been consumed by the player or has finished playing back or 		the player is stopped. may be nil.
--
-- ObjC selector: @- scheduleFile:atTime:completionCallbackType:completionHandler:@
scheduleFile_atTime_completionCallbackType_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioFile file, IsAVAudioTime when) => avAudioPlayerNode -> file -> when -> AVAudioPlayerNodeCompletionCallbackType -> Ptr () -> IO ()
scheduleFile_atTime_completionCallbackType_completionHandler avAudioPlayerNode  file when callbackType completionHandler =
withObjCPtr file $ \raw_file ->
  withObjCPtr when $ \raw_when ->
      sendMsg avAudioPlayerNode (mkSelector "scheduleFile:atTime:completionCallbackType:completionHandler:") retVoid [argPtr (castPtr raw_file :: Ptr ()), argPtr (castPtr raw_when :: Ptr ()), argCLong (coerce callbackType), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleSegment:startingFrame:frameCount:atTime:completionHandler:
--
-- Schedule playing a segment of an audio file.
--
-- @file@ — the file to play
--
-- @startFrame@ — the starting frame position in the stream
--
-- @numberFrames@ — the number of frames to play
--
-- @when@ — the time at which to play the region. see the discussion of timestamps, above.
--
-- @completionHandler@ — called after the segment has been consumed by the player or the player is stopped. may be nil.
--
-- It is possible for the completionHandler to be called before rendering begins		or before the segment is played completely.
--
-- ObjC selector: @- scheduleSegment:startingFrame:frameCount:atTime:completionHandler:@
scheduleSegment_startingFrame_frameCount_atTime_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioFile file, IsAVAudioTime when) => avAudioPlayerNode -> file -> CLong -> CUInt -> when -> Ptr () -> IO ()
scheduleSegment_startingFrame_frameCount_atTime_completionHandler avAudioPlayerNode  file startFrame numberFrames when completionHandler =
withObjCPtr file $ \raw_file ->
  withObjCPtr when $ \raw_when ->
      sendMsg avAudioPlayerNode (mkSelector "scheduleSegment:startingFrame:frameCount:atTime:completionHandler:") retVoid [argPtr (castPtr raw_file :: Ptr ()), argCLong (fromIntegral startFrame), argCUInt (fromIntegral numberFrames), argPtr (castPtr raw_when :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | scheduleSegment:startingFrame:frameCount:atTime:completionCallbackType:completionHandler:
--
-- Schedule playing a segment of an audio file.
--
-- @file@ — the file to play
--
-- @startFrame@ — the starting frame position in the stream
--
-- @numberFrames@ — the number of frames to play
--
-- @when@ — the time at which to play the region. see the discussion of timestamps, above.
--
-- @callbackType@ — option to specify when the completion handler must be called
--
-- @completionHandler@ — called after the segment has been consumed by the player or has finished playing back or 		the player is stopped. may be nil.
--
-- ObjC selector: @- scheduleSegment:startingFrame:frameCount:atTime:completionCallbackType:completionHandler:@
scheduleSegment_startingFrame_frameCount_atTime_completionCallbackType_completionHandler :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioFile file, IsAVAudioTime when) => avAudioPlayerNode -> file -> CLong -> CUInt -> when -> AVAudioPlayerNodeCompletionCallbackType -> Ptr () -> IO ()
scheduleSegment_startingFrame_frameCount_atTime_completionCallbackType_completionHandler avAudioPlayerNode  file startFrame numberFrames when callbackType completionHandler =
withObjCPtr file $ \raw_file ->
  withObjCPtr when $ \raw_when ->
      sendMsg avAudioPlayerNode (mkSelector "scheduleSegment:startingFrame:frameCount:atTime:completionCallbackType:completionHandler:") retVoid [argPtr (castPtr raw_file :: Ptr ()), argCLong (fromIntegral startFrame), argCUInt (fromIntegral numberFrames), argPtr (castPtr raw_when :: Ptr ()), argCLong (coerce callbackType), argPtr (castPtr completionHandler :: Ptr ())]

-- | stop
--
-- Clear all of the node's previously scheduled events and stop playback.
--
-- All of the node's previously scheduled events are cleared, including any that are in the		middle of playing. The node's sample time (and therefore the times to which new events are 		to be scheduled) is reset to 0, and will not proceed until the node is started again (via		play or playAtTime).
--
-- Note that pausing or stopping all the players connected to an engine does not pause or stop		the engine or the underlying hardware. The engine must be explicitly paused or stopped for		the hardware to stop.
--
-- ObjC selector: @- stop@
stop :: IsAVAudioPlayerNode avAudioPlayerNode => avAudioPlayerNode -> IO ()
stop avAudioPlayerNode  =
  sendMsg avAudioPlayerNode (mkSelector "stop") retVoid []

-- | prepareWithFrameCount:
--
-- Prepares previously scheduled file regions or buffers for playback.
--
-- @frameCount@ — The number of sample frames of data to be prepared before returning.
--
-- ObjC selector: @- prepareWithFrameCount:@
prepareWithFrameCount :: IsAVAudioPlayerNode avAudioPlayerNode => avAudioPlayerNode -> CUInt -> IO ()
prepareWithFrameCount avAudioPlayerNode  frameCount =
  sendMsg avAudioPlayerNode (mkSelector "prepareWithFrameCount:") retVoid [argCUInt (fromIntegral frameCount)]

-- | play
--
-- Start or resume playback immediately.
--
-- equivalent to playAtTime:nil
--
-- ObjC selector: @- play@
play :: IsAVAudioPlayerNode avAudioPlayerNode => avAudioPlayerNode -> IO ()
play avAudioPlayerNode  =
  sendMsg avAudioPlayerNode (mkSelector "play") retVoid []

-- | playAtTime:
--
-- Start or resume playback at a specific time.
--
-- @when@ — the node time at which to start or resume playback. nil signifies "now".
--
-- This node is initially paused. Requests to play buffers or file segments are enqueued, and		any necessary decoding begins immediately. Playback does not begin, however, until the player		has started playing, via this method.
--
-- Note that providing an AVAudioTime which is past (before lastRenderTime) will cause the 		player to begin playback immediately.
--
-- E.g. To start a player X seconds in future:// start engine and playerNSError *nsErr = nil;[_engine startAndReturnError:&nsErr];if (!nsErr) {	const float kStartDelayTime = 0.5; // sec	AVAudioFormat *outputFormat = [_player outputFormatForBus:0];	AVAudioFramePosition startSampleTime = _player.lastRenderTime.sampleTime + kStartDelayTime * outputFormat.sampleRate;	AVAudioTime *startTime = [AVAudioTime timeWithSampleTime:startSampleTime atRate:outputFormat.sampleRate];	[_player playAtTime:startTime];}
--
-- ObjC selector: @- playAtTime:@
playAtTime :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioTime when) => avAudioPlayerNode -> when -> IO ()
playAtTime avAudioPlayerNode  when =
withObjCPtr when $ \raw_when ->
    sendMsg avAudioPlayerNode (mkSelector "playAtTime:") retVoid [argPtr (castPtr raw_when :: Ptr ())]

-- | pause
--
-- Pause playback.
--
-- The player's sample time does not advance while the node is paused.
--
-- Note that pausing or stopping all the players connected to an engine does not pause or stop		the engine or the underlying hardware. The engine must be explicitly paused or stopped for		the hardware to stop.
--
-- ObjC selector: @- pause@
pause :: IsAVAudioPlayerNode avAudioPlayerNode => avAudioPlayerNode -> IO ()
pause avAudioPlayerNode  =
  sendMsg avAudioPlayerNode (mkSelector "pause") retVoid []

-- | nodeTimeForPlayerTime:
--
-- Convert from player time to node time.
--
-- @playerTime@ — a time relative to the player's start time
--
-- Returns: a node time
--
-- This method and its inverse @playerTimeForNodeTime:@ are discussed in the		introduction to this class.
--
-- If the player is not playing when this method is called, nil is returned.
--
-- ObjC selector: @- nodeTimeForPlayerTime:@
nodeTimeForPlayerTime :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioTime playerTime) => avAudioPlayerNode -> playerTime -> IO (Id AVAudioTime)
nodeTimeForPlayerTime avAudioPlayerNode  playerTime =
withObjCPtr playerTime $ \raw_playerTime ->
    sendMsg avAudioPlayerNode (mkSelector "nodeTimeForPlayerTime:") (retPtr retVoid) [argPtr (castPtr raw_playerTime :: Ptr ())] >>= retainedObject . castPtr

-- | playerTimeForNodeTime:
--
-- Convert from node time to player time.
--
-- @nodeTime@ — a node time
--
-- Returns: a time relative to the player's start time
--
-- This method and its inverse @nodeTimeForPlayerTime:@ are discussed in the		introduction to this class.
--
-- If the player is not playing when this method is called, nil is returned.
--
-- ObjC selector: @- playerTimeForNodeTime:@
playerTimeForNodeTime :: (IsAVAudioPlayerNode avAudioPlayerNode, IsAVAudioTime nodeTime) => avAudioPlayerNode -> nodeTime -> IO (Id AVAudioTime)
playerTimeForNodeTime avAudioPlayerNode  nodeTime =
withObjCPtr nodeTime $ \raw_nodeTime ->
    sendMsg avAudioPlayerNode (mkSelector "playerTimeForNodeTime:") (retPtr retVoid) [argPtr (castPtr raw_nodeTime :: Ptr ())] >>= retainedObject . castPtr

-- | playing
--
-- Indicates whether or not the player is playing.
--
-- ObjC selector: @- playing@
playing :: IsAVAudioPlayerNode avAudioPlayerNode => avAudioPlayerNode -> IO Bool
playing avAudioPlayerNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioPlayerNode (mkSelector "playing") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @scheduleBuffer:completionHandler:@
scheduleBuffer_completionHandlerSelector :: Selector
scheduleBuffer_completionHandlerSelector = mkSelector "scheduleBuffer:completionHandler:"

-- | @Selector@ for @scheduleBuffer:completionCallbackType:completionHandler:@
scheduleBuffer_completionCallbackType_completionHandlerSelector :: Selector
scheduleBuffer_completionCallbackType_completionHandlerSelector = mkSelector "scheduleBuffer:completionCallbackType:completionHandler:"

-- | @Selector@ for @scheduleBuffer:atTime:options:completionHandler:@
scheduleBuffer_atTime_options_completionHandlerSelector :: Selector
scheduleBuffer_atTime_options_completionHandlerSelector = mkSelector "scheduleBuffer:atTime:options:completionHandler:"

-- | @Selector@ for @scheduleBuffer:atTime:options:completionCallbackType:completionHandler:@
scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector :: Selector
scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector = mkSelector "scheduleBuffer:atTime:options:completionCallbackType:completionHandler:"

-- | @Selector@ for @scheduleFile:atTime:completionHandler:@
scheduleFile_atTime_completionHandlerSelector :: Selector
scheduleFile_atTime_completionHandlerSelector = mkSelector "scheduleFile:atTime:completionHandler:"

-- | @Selector@ for @scheduleFile:atTime:completionCallbackType:completionHandler:@
scheduleFile_atTime_completionCallbackType_completionHandlerSelector :: Selector
scheduleFile_atTime_completionCallbackType_completionHandlerSelector = mkSelector "scheduleFile:atTime:completionCallbackType:completionHandler:"

-- | @Selector@ for @scheduleSegment:startingFrame:frameCount:atTime:completionHandler:@
scheduleSegment_startingFrame_frameCount_atTime_completionHandlerSelector :: Selector
scheduleSegment_startingFrame_frameCount_atTime_completionHandlerSelector = mkSelector "scheduleSegment:startingFrame:frameCount:atTime:completionHandler:"

-- | @Selector@ for @scheduleSegment:startingFrame:frameCount:atTime:completionCallbackType:completionHandler:@
scheduleSegment_startingFrame_frameCount_atTime_completionCallbackType_completionHandlerSelector :: Selector
scheduleSegment_startingFrame_frameCount_atTime_completionCallbackType_completionHandlerSelector = mkSelector "scheduleSegment:startingFrame:frameCount:atTime:completionCallbackType:completionHandler:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @prepareWithFrameCount:@
prepareWithFrameCountSelector :: Selector
prepareWithFrameCountSelector = mkSelector "prepareWithFrameCount:"

-- | @Selector@ for @play@
playSelector :: Selector
playSelector = mkSelector "play"

-- | @Selector@ for @playAtTime:@
playAtTimeSelector :: Selector
playAtTimeSelector = mkSelector "playAtTime:"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @nodeTimeForPlayerTime:@
nodeTimeForPlayerTimeSelector :: Selector
nodeTimeForPlayerTimeSelector = mkSelector "nodeTimeForPlayerTime:"

-- | @Selector@ for @playerTimeForNodeTime:@
playerTimeForNodeTimeSelector :: Selector
playerTimeForNodeTimeSelector = mkSelector "playerTimeForNodeTime:"

-- | @Selector@ for @playing@
playingSelector :: Selector
playingSelector = mkSelector "playing"

