{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , scheduleBufferSelector
  , scheduleBuffer_completionCallbackType_completionHandlerSelector
  , scheduleBuffer_atTime_optionsSelector
  , scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector
  , gainMetaParameterSelector
  , rateMetaParameterSelector
  , mixerSelector
  , formatSelector

  -- * Enum types
  , PHASEPushStreamBufferOptions(PHASEPushStreamBufferOptions)
  , pattern PHASEPushStreamBufferDefault
  , pattern PHASEPushStreamBufferLoops
  , pattern PHASEPushStreamBufferInterrupts
  , pattern PHASEPushStreamBufferInterruptsAtLoop
  , PHASEPushStreamCompletionCallbackCondition(PHASEPushStreamCompletionCallbackCondition)
  , pattern PHASEPushStreamCompletionDataRendered

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

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASEPushStreamNode)
init_ phasePushStreamNode  =
  sendMsg phasePushStreamNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEPushStreamNode)
new  =
  do
    cls' <- getRequiredClass "PHASEPushStreamNode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
scheduleBuffer phasePushStreamNode  buffer =
withObjCPtr buffer $ \raw_buffer ->
    sendMsg phasePushStreamNode (mkSelector "scheduleBuffer:") retVoid [argPtr (castPtr raw_buffer :: Ptr ())]

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
scheduleBuffer_completionCallbackType_completionHandler phasePushStreamNode  buffer completionCallbackType completionHandler =
withObjCPtr buffer $ \raw_buffer ->
    sendMsg phasePushStreamNode (mkSelector "scheduleBuffer:completionCallbackType:completionHandler:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argCLong (coerce completionCallbackType), argPtr (castPtr completionHandler :: Ptr ())]

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
scheduleBuffer_atTime_options phasePushStreamNode  buffer when options =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr when $ \raw_when ->
      sendMsg phasePushStreamNode (mkSelector "scheduleBuffer:atTime:options:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_when :: Ptr ()), argCULong (coerce options)]

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
scheduleBuffer_atTime_options_completionCallbackType_completionHandler phasePushStreamNode  buffer when options completionCallbackType completionHandler =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr when $ \raw_when ->
      sendMsg phasePushStreamNode (mkSelector "scheduleBuffer:atTime:options:completionCallbackType:completionHandler:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_when :: Ptr ()), argCULong (coerce options), argCLong (coerce completionCallbackType), argPtr (castPtr completionHandler :: Ptr ())]

-- | gainMetaParameter
--
-- If specified during construction, the metaparameter for controlling gain will be available here
--
-- ObjC selector: @- gainMetaParameter@
gainMetaParameter :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASENumberMetaParameter)
gainMetaParameter phasePushStreamNode  =
  sendMsg phasePushStreamNode (mkSelector "gainMetaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rateMetaParameter
--
-- If specified during construction, the metaparameter for controlling rate/pitch will be available here
--
-- ObjC selector: @- rateMetaParameter@
rateMetaParameter :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASENumberMetaParameter)
rateMetaParameter phasePushStreamNode  =
  sendMsg phasePushStreamNode (mkSelector "rateMetaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mixer
--
-- The readonly property that returns the PHASEMixer this stream was created with and assigned to.
--
-- ObjC selector: @- mixer@
mixer :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id PHASEMixer)
mixer phasePushStreamNode  =
  sendMsg phasePushStreamNode (mkSelector "mixer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | format
--
-- The readonly property that returns the AVAudioFormat that this stream was initialized with.
--
-- ObjC selector: @- format@
format :: IsPHASEPushStreamNode phasePushStreamNode => phasePushStreamNode -> IO (Id AVAudioFormat)
format phasePushStreamNode  =
  sendMsg phasePushStreamNode (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @scheduleBuffer:@
scheduleBufferSelector :: Selector
scheduleBufferSelector = mkSelector "scheduleBuffer:"

-- | @Selector@ for @scheduleBuffer:completionCallbackType:completionHandler:@
scheduleBuffer_completionCallbackType_completionHandlerSelector :: Selector
scheduleBuffer_completionCallbackType_completionHandlerSelector = mkSelector "scheduleBuffer:completionCallbackType:completionHandler:"

-- | @Selector@ for @scheduleBuffer:atTime:options:@
scheduleBuffer_atTime_optionsSelector :: Selector
scheduleBuffer_atTime_optionsSelector = mkSelector "scheduleBuffer:atTime:options:"

-- | @Selector@ for @scheduleBuffer:atTime:options:completionCallbackType:completionHandler:@
scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector :: Selector
scheduleBuffer_atTime_options_completionCallbackType_completionHandlerSelector = mkSelector "scheduleBuffer:atTime:options:completionCallbackType:completionHandler:"

-- | @Selector@ for @gainMetaParameter@
gainMetaParameterSelector :: Selector
gainMetaParameterSelector = mkSelector "gainMetaParameter"

-- | @Selector@ for @rateMetaParameter@
rateMetaParameterSelector :: Selector
rateMetaParameterSelector = mkSelector "rateMetaParameter"

-- | @Selector@ for @mixer@
mixerSelector :: Selector
mixerSelector = mkSelector "mixer"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

