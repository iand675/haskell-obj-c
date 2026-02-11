{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioInputNode
--
-- A node that performs audio input in the engine.
--
-- When the engine is rendering to/from an audio device, this node connects to the system's 		audio input.		When the engine is operating in manual rendering mode, this node can be used to supply		the input data to the engine.
--
-- This node has one element.		The format of the input scope reflects:			- the audio hardware sample rate and channel count, when connected to the hardware			- the format of the PCM audio data that the node will supply to the engine, in the			  manual rendering mode (see @setManualRenderingInputPCMFormat:inputBlock:@)
--
-- When rendering from an audio device, the input node does not support format conversion.		Hence the format of the output scope must be same as that of the input, as well as the		formats for all the nodes connected in the input node chain.
--
-- In the manual rendering mode, the format of the output scope is initially the same as that		of the input, but you may set it to a different format, in which case the node will convert.
--
-- Generated bindings for @AVAudioInputNode@.
module ObjC.AVFAudio.AVAudioInputNode
  ( AVAudioInputNode
  , IsAVAudioInputNode(..)
  , init_
  , setManualRenderingInputPCMFormat_inputBlock
  , setMutedSpeechActivityEventListener
  , voiceProcessingBypassed
  , setVoiceProcessingBypassed
  , voiceProcessingAGCEnabled
  , setVoiceProcessingAGCEnabled
  , voiceProcessingInputMuted
  , setVoiceProcessingInputMuted
  , initSelector
  , setManualRenderingInputPCMFormat_inputBlockSelector
  , setMutedSpeechActivityEventListenerSelector
  , voiceProcessingBypassedSelector
  , setVoiceProcessingBypassedSelector
  , voiceProcessingAGCEnabledSelector
  , setVoiceProcessingAGCEnabledSelector
  , voiceProcessingInputMutedSelector
  , setVoiceProcessingInputMutedSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> IO (Id AVAudioInputNode)
init_ avAudioInputNode  =
  sendMsg avAudioInputNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | setManualRenderingInputPCMFormat:inputBlock:
--
-- Supply the data through the input node to the engine operating in the manual rendering mode.
--
-- @format@ — The format of the PCM audio data the block will supply to the engine
--
-- @block@ — The block the engine will call on the input node to get the audio to send to the output,		when operating in the manual rendering mode. See @AVAudioIONodeInputBlock@ for more details
--
-- Returns: YES for success
--
-- This block must be set if the input node is being used when the engine is operating in 		manual rendering mode.		Switching the engine to render to/from an audio device invalidates any previously set block, 		and makes this method ineffective.
--
-- ObjC selector: @- setManualRenderingInputPCMFormat:inputBlock:@
setManualRenderingInputPCMFormat_inputBlock :: (IsAVAudioInputNode avAudioInputNode, IsAVAudioFormat format) => avAudioInputNode -> format -> Ptr () -> IO Bool
setManualRenderingInputPCMFormat_inputBlock avAudioInputNode  format block =
withObjCPtr format $ \raw_format ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioInputNode (mkSelector "setManualRenderingInputPCMFormat:inputBlock:") retCULong [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | setMutedSpeechActivityEventListener
--
-- Register a listener to be notified when speech activity event occurs while the input is muted.
--
-- @listenerBlock@ — The block the engine will call when speech activity event occurs while the input is muted.		Passing nil will remove an already set block.
--
-- Returns: YES for success
--
-- Continuous presence of or lack of speech activity during mute will not cause redundant notification.		In order to use this API, it's expected to implement the mute via the voiceProcessingInputMuted.
--
-- ObjC selector: @- setMutedSpeechActivityEventListener:@
setMutedSpeechActivityEventListener :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> Ptr () -> IO Bool
setMutedSpeechActivityEventListener avAudioInputNode  listenerBlock =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioInputNode (mkSelector "setMutedSpeechActivityEventListener:") retCULong [argPtr (castPtr listenerBlock :: Ptr ())]

-- | voiceProcessingBypassed
--
-- Bypass all processing for microphone uplink done by the voice processing unit.
--
-- Querying this property when voice processing is disabled will return false.
--
-- ObjC selector: @- voiceProcessingBypassed@
voiceProcessingBypassed :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> IO Bool
voiceProcessingBypassed avAudioInputNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioInputNode (mkSelector "voiceProcessingBypassed") retCULong []

-- | voiceProcessingBypassed
--
-- Bypass all processing for microphone uplink done by the voice processing unit.
--
-- Querying this property when voice processing is disabled will return false.
--
-- ObjC selector: @- setVoiceProcessingBypassed:@
setVoiceProcessingBypassed :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> Bool -> IO ()
setVoiceProcessingBypassed avAudioInputNode  value =
  sendMsg avAudioInputNode (mkSelector "setVoiceProcessingBypassed:") retVoid [argCULong (if value then 1 else 0)]

-- | voiceProcessingAGCEnabled
--
-- Enable automatic gain control on the processed microphone uplink.        signal. Enabled by default.
--
-- Querying this property when voice processing is disabled will return false.
--
-- ObjC selector: @- voiceProcessingAGCEnabled@
voiceProcessingAGCEnabled :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> IO Bool
voiceProcessingAGCEnabled avAudioInputNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioInputNode (mkSelector "voiceProcessingAGCEnabled") retCULong []

-- | voiceProcessingAGCEnabled
--
-- Enable automatic gain control on the processed microphone uplink.        signal. Enabled by default.
--
-- Querying this property when voice processing is disabled will return false.
--
-- ObjC selector: @- setVoiceProcessingAGCEnabled:@
setVoiceProcessingAGCEnabled :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> Bool -> IO ()
setVoiceProcessingAGCEnabled avAudioInputNode  value =
  sendMsg avAudioInputNode (mkSelector "setVoiceProcessingAGCEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | voiceProcessingInputMuted
--
-- Mutes the input of the voice processing unit.
--
-- Querying this property when voice processing is disabled will return false.
--
-- ObjC selector: @- voiceProcessingInputMuted@
voiceProcessingInputMuted :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> IO Bool
voiceProcessingInputMuted avAudioInputNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioInputNode (mkSelector "voiceProcessingInputMuted") retCULong []

-- | voiceProcessingInputMuted
--
-- Mutes the input of the voice processing unit.
--
-- Querying this property when voice processing is disabled will return false.
--
-- ObjC selector: @- setVoiceProcessingInputMuted:@
setVoiceProcessingInputMuted :: IsAVAudioInputNode avAudioInputNode => avAudioInputNode -> Bool -> IO ()
setVoiceProcessingInputMuted avAudioInputNode  value =
  sendMsg avAudioInputNode (mkSelector "setVoiceProcessingInputMuted:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setManualRenderingInputPCMFormat:inputBlock:@
setManualRenderingInputPCMFormat_inputBlockSelector :: Selector
setManualRenderingInputPCMFormat_inputBlockSelector = mkSelector "setManualRenderingInputPCMFormat:inputBlock:"

-- | @Selector@ for @setMutedSpeechActivityEventListener:@
setMutedSpeechActivityEventListenerSelector :: Selector
setMutedSpeechActivityEventListenerSelector = mkSelector "setMutedSpeechActivityEventListener:"

-- | @Selector@ for @voiceProcessingBypassed@
voiceProcessingBypassedSelector :: Selector
voiceProcessingBypassedSelector = mkSelector "voiceProcessingBypassed"

-- | @Selector@ for @setVoiceProcessingBypassed:@
setVoiceProcessingBypassedSelector :: Selector
setVoiceProcessingBypassedSelector = mkSelector "setVoiceProcessingBypassed:"

-- | @Selector@ for @voiceProcessingAGCEnabled@
voiceProcessingAGCEnabledSelector :: Selector
voiceProcessingAGCEnabledSelector = mkSelector "voiceProcessingAGCEnabled"

-- | @Selector@ for @setVoiceProcessingAGCEnabled:@
setVoiceProcessingAGCEnabledSelector :: Selector
setVoiceProcessingAGCEnabledSelector = mkSelector "setVoiceProcessingAGCEnabled:"

-- | @Selector@ for @voiceProcessingInputMuted@
voiceProcessingInputMutedSelector :: Selector
voiceProcessingInputMutedSelector = mkSelector "voiceProcessingInputMuted"

-- | @Selector@ for @setVoiceProcessingInputMuted:@
setVoiceProcessingInputMutedSelector :: Selector
setVoiceProcessingInputMutedSelector = mkSelector "setVoiceProcessingInputMuted:"

