{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioIONode
--
-- Base class for a node that performs audio input or output in the engine.
--
-- When the engine is configured to render to/from an audio device, on macOS, AVAudioInputNode 		and AVAudioOutputNode communicate with the system's default input and output devices. 		On iOS, they communicate with the devices appropriate to the app's AVAudioSession category 		and other configuration, also considering the user's actions such as 		connecting/disconnecting external devices.
--
-- In the manual rendering mode, the AVAudioInputNode and AVAudioOutputNode perform the input		and output in the engine, in response to client's request.
--
-- Generated bindings for @AVAudioIONode@.
module ObjC.AVFAudio.AVAudioIONode
  ( AVAudioIONode
  , IsAVAudioIONode(..)
  , setVoiceProcessingEnabled_error
  , presentationLatency
  , audioUnit
  , voiceProcessingEnabled
  , audioUnitSelector
  , presentationLatencySelector
  , setVoiceProcessingEnabled_errorSelector
  , voiceProcessingEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setVoiceProcessingEnabled:error:
--
-- Enable or disable voice processing on the IO node.
--
-- @enabled@ — Whether voice processing is to be enabled.
--
-- @outError@ — On exit, if the IO node cannot enable or diable voice processing, a description of the error
--
-- Returns: YES for success
--
-- If enabled, the input node does signal processing on the incoming audio (taking out any        of the audio that is played from the device at a given time from the incoming audio).        Disabling this mode on either of the IO nodes automatically disabled it on the other IO node.
--
-- Voice processing requires both input and output nodes to be in the voice processing mode.        Enabling this mode on either of the IO nodes automatically enables it on the other IO node.        Voice processing is only supported when the engine is rendering to the audio device and not        in the manual rendering mode.        Voice processing can only be be enabled or disabled when the engine is in a stopped state.
--
-- The output format of the input node and the input format of the output node have to be        the same and they can only be changed when the engine is in a stopped state.
--
-- ObjC selector: @- setVoiceProcessingEnabled:error:@
setVoiceProcessingEnabled_error :: (IsAVAudioIONode avAudioIONode, IsNSError outError) => avAudioIONode -> Bool -> outError -> IO Bool
setVoiceProcessingEnabled_error avAudioIONode enabled outError =
  sendMessage avAudioIONode setVoiceProcessingEnabled_errorSelector enabled (toNSError outError)

-- | presentationLatency
--
-- The presentation or hardware latency, applicable when the engine is rendering to/from an		audio device.
--
-- This corresponds to kAudioDevicePropertyLatency and kAudioStreamPropertyLatency.		See <CoreAudio/AudioHardwareBase.h>.
--
-- ObjC selector: @- presentationLatency@
presentationLatency :: IsAVAudioIONode avAudioIONode => avAudioIONode -> IO CDouble
presentationLatency avAudioIONode =
  sendMessage avAudioIONode presentationLatencySelector

-- | audioUnit
--
-- The node's underlying AudioUnit, if any.
--
-- This is only necessary for certain advanced usages.
--
-- ObjC selector: @- audioUnit@
audioUnit :: IsAVAudioIONode avAudioIONode => avAudioIONode -> IO (Ptr ())
audioUnit avAudioIONode =
  sendMessage avAudioIONode audioUnitSelector

-- | voiceProcessingEnabled
--
-- Indicates whether voice processing is enabled.
--
-- ObjC selector: @- voiceProcessingEnabled@
voiceProcessingEnabled :: IsAVAudioIONode avAudioIONode => avAudioIONode -> IO Bool
voiceProcessingEnabled avAudioIONode =
  sendMessage avAudioIONode voiceProcessingEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setVoiceProcessingEnabled:error:@
setVoiceProcessingEnabled_errorSelector :: Selector '[Bool, Id NSError] Bool
setVoiceProcessingEnabled_errorSelector = mkSelector "setVoiceProcessingEnabled:error:"

-- | @Selector@ for @presentationLatency@
presentationLatencySelector :: Selector '[] CDouble
presentationLatencySelector = mkSelector "presentationLatency"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector '[] (Ptr ())
audioUnitSelector = mkSelector "audioUnit"

-- | @Selector@ for @voiceProcessingEnabled@
voiceProcessingEnabledSelector :: Selector '[] Bool
voiceProcessingEnabledSelector = mkSelector "voiceProcessingEnabled"

