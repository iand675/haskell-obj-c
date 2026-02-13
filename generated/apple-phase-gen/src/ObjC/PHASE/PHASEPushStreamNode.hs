{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEPushStreamNode
--
-- An object for addessing an instance of a stream in an executing sound event
--
-- Generated bindings for @PHASEPushStreamNode@.
module ObjC.PHASE.PHASEPushStreamNode
  ( PHASEPushStreamNode
  , IsPHASEPushStreamNode(..)
  , init_
  , new
  , scheduleBuffer
  , scheduleBuffer_completionCallbackType_completionHandler
  , scheduleBuffer_atTime_options
  , scheduleBuffer_atTime_options_completionCallbackType_completionHandler
  , gainMetaParameter
  , rateMetaParameter
  , mixer
  , format
  , formatSelector
  , gainMetaParameterSelector
  , initSelector
  , mixerSelector
  , newSelector
  , rateMetaParameterSelector
  , scheduleBufferSelector
  , scheduleBuffer_atTime_optionsSelector
  , scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector
  , scheduleBuffer_completionCallbackType_completionHandlerSelector

  -- * Enum types
  , PHASEPushStreamBufferOptions(PHASEPushStreamBufferOptions)
  , pattern PHASEPushStreamBufferDefault
  , pattern PHASEPushStreamBufferLoops
  , pattern PHASEPushStreamBufferInterrupts
  , pattern PHASEPushStreamBufferInterruptsAtLoop
  , PHASEPushStreamCompletionCallbackCondition(PHASEPushStreamCompletionCallbackCondition)
  , pattern PHASEPushStreamCompletionDataRendered

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASEPushStreamNode)
init_ phasePushStreamNode =
  sendOwnedMessage phasePushStreamNode initSelector

-- | @+ new@
new :: IO (Id PHASEPushStreamNode)
new  =
  do
    cls' <- getRequiredClass "PHASEPushStreamNode"
    sendOwnedClassMessage cls' newSelector

-- | scheduleBuffer
--
-- Schedule a buffer for playback.
--
-- @buffer@ — The buffer with PCM audio data.
--
-- Schedules the buffer to be played following any previously scheduled buffer(s).        The buffer format must be same as format specified during player instantiation
--
-- ObjC selector: @- scheduleBuffer:@
scheduleBuffer :: (IsPHASEPushStreamNode phasePushStreamNode, IsAVAudioPCMBuffer buffer) => phasePushStreamNode -> buffer -> IO ()
scheduleBuffer phasePushStreamNode buffer =
  sendMessage phasePushStreamNode scheduleBufferSelector (toAVAudioPCMBuffer buffer)

-- | scheduleBuffer:completionCallbackType:completionHandler:
--
-- Schedule a buffer for playback.
--
-- Schedules the buffer to be played following any previously scheduled buffer(s).        The buffer format must be same as format specified during player instantiation
--
-- @buffer@ — The buffer with PCM audio data.
--
-- @completionCallbackType@ — Option to specify when the completion handler must be called.
--
-- @completionHandler@ — The completionHandler to be called as per the specified completion callback type        or when the player is stopped, at which point the buffer can be recycled.
--
-- ObjC selector: @- scheduleBuffer:completionCallbackType:completionHandler:@
scheduleBuffer_completionCallbackType_completionHandler :: (IsPHASEPushStreamNode phasePushStreamNode, IsAVAudioPCMBuffer buffer) => phasePushStreamNode -> buffer -> PHASEPushStreamCompletionCallbackCondition -> Ptr () -> IO ()
scheduleBuffer_completionCallbackType_completionHandler phasePushStreamNode buffer completionCallbackType completionHandler =
  sendMessage phasePushStreamNode scheduleBuffer_completionCallbackType_completionHandlerSelector (toAVAudioPCMBuffer buffer) completionCallbackType completionHandler

-- | scheduleBuffer:atTime:options:
--
-- Schedule a buffer for playback at a given time.
--
-- The buffer format must be same as format specified during player instantiation
--
-- @buffer@ — The buffer with PCM audio data.
--
-- @when@ — The time at which to play the buffer.
--
-- @options@ — Options for looping, interrupting other buffers, etc.
--
-- ObjC selector: @- scheduleBuffer:atTime:options:@
scheduleBuffer_atTime_options :: (IsPHASEPushStreamNode phasePushStreamNode, IsAVAudioPCMBuffer buffer, IsAVAudioTime when) => phasePushStreamNode -> buffer -> when -> PHASEPushStreamBufferOptions -> IO ()
scheduleBuffer_atTime_options phasePushStreamNode buffer when options =
  sendMessage phasePushStreamNode scheduleBuffer_atTime_optionsSelector (toAVAudioPCMBuffer buffer) (toAVAudioTime when) options

-- | scheduleBuffer:atTime:options:completionCallbackType:completionHandler:
--
-- Schedule a buffer for playback at a given time.
--
-- The buffer format must be same as format specified during player instantiation
--
-- @buffer@ — The buffer with PCM audio data.
--
-- @when@ — The time at which to play the buffer.
--
-- @options@ — Options for looping, interrupting other buffers, etc.
--
-- @completionCallbackType@ — Option to specify when the completion handler must be called.
--
-- @completionHandler@ — The completionHandler to be called as per the callback type specified or when        the player is stopped, at which point the buffer can be recycled.
--
-- ObjC selector: @- scheduleBuffer:atTime:options:completionCallbackType:completionHandler:@
scheduleBuffer_atTime_options_completionCallbackType_completionHandler :: (IsPHASEPushStreamNode phasePushStreamNode, IsAVAudioPCMBuffer buffer, IsAVAudioTime when) => phasePushStreamNode -> buffer -> when -> PHASEPushStreamBufferOptions -> PHASEPushStreamCompletionCallbackCondition -> Ptr () -> IO ()
scheduleBuffer_atTime_options_completionCallbackType_completionHandler phasePushStreamNode buffer when options completionCallbackType completionHandler =
  sendMessage phasePushStreamNode scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector (toAVAudioPCMBuffer buffer) (toAVAudioTime when) options completionCallbackType completionHandler

-- | gainMetaParameter
--
-- If specified during construction, the metaparameter for controlling gain will be available here
--
-- ObjC selector: @- gainMetaParameter@
gainMetaParameter :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASENumberMetaParameter)
gainMetaParameter phasePushStreamNode =
  sendMessage phasePushStreamNode gainMetaParameterSelector

-- | rateMetaParameter
--
-- If specified during construction, the metaparameter for controlling rate/pitch will be available here
--
-- ObjC selector: @- rateMetaParameter@
rateMetaParameter :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASENumberMetaParameter)
rateMetaParameter phasePushStreamNode =
  sendMessage phasePushStreamNode rateMetaParameterSelector

-- | mixer
--
-- The readonly property that returns the PHASEMixer this stream was created with and assigned to.
--
-- ObjC selector: @- mixer@
mixer :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASEMixer)
mixer phasePushStreamNode =
  sendMessage phasePushStreamNode mixerSelector

-- | format
--
-- The readonly property that returns the AVAudioFormat that this stream was initialized with.
--
-- ObjC selector: @- format@
format :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id AVAudioFormat)
format phasePushStreamNode =
  sendMessage phasePushStreamNode formatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEPushStreamNode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEPushStreamNode)
newSelector = mkSelector "new"

-- | @Selector@ for @scheduleBuffer:@
scheduleBufferSelector :: Selector '[Id AVAudioPCMBuffer] ()
scheduleBufferSelector = mkSelector "scheduleBuffer:"

-- | @Selector@ for @scheduleBuffer:completionCallbackType:completionHandler:@
scheduleBuffer_completionCallbackType_completionHandlerSelector :: Selector '[Id AVAudioPCMBuffer, PHASEPushStreamCompletionCallbackCondition, Ptr ()] ()
scheduleBuffer_completionCallbackType_completionHandlerSelector = mkSelector "scheduleBuffer:completionCallbackType:completionHandler:"

-- | @Selector@ for @scheduleBuffer:atTime:options:@
scheduleBuffer_atTime_optionsSelector :: Selector '[Id AVAudioPCMBuffer, Id AVAudioTime, PHASEPushStreamBufferOptions] ()
scheduleBuffer_atTime_optionsSelector = mkSelector "scheduleBuffer:atTime:options:"

-- | @Selector@ for @scheduleBuffer:atTime:options:completionCallbackType:completionHandler:@
scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector :: Selector '[Id AVAudioPCMBuffer, Id AVAudioTime, PHASEPushStreamBufferOptions, PHASEPushStreamCompletionCallbackCondition, Ptr ()] ()
scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector = mkSelector "scheduleBuffer:atTime:options:completionCallbackType:completionHandler:"

-- | @Selector@ for @gainMetaParameter@
gainMetaParameterSelector :: Selector '[] (Id PHASENumberMetaParameter)
gainMetaParameterSelector = mkSelector "gainMetaParameter"

-- | @Selector@ for @rateMetaParameter@
rateMetaParameterSelector :: Selector '[] (Id PHASENumberMetaParameter)
rateMetaParameterSelector = mkSelector "rateMetaParameter"

-- | @Selector@ for @mixer@
mixerSelector :: Selector '[] (Id PHASEMixer)
mixerSelector = mkSelector "mixer"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id AVAudioFormat)
formatSelector = mkSelector "format"

