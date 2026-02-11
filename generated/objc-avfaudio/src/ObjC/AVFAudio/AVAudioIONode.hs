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
  , setVoiceProcessingEnabled_errorSelector
  , presentationLatencySelector
  , audioUnitSelector
  , voiceProcessingEnabledSelector


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
setVoiceProcessingEnabled_error avAudioIONode  enabled outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioIONode (mkSelector "setVoiceProcessingEnabled:error:") retCULong [argCULong (if enabled then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | presentationLatency
--
-- The presentation or hardware latency, applicable when the engine is rendering to/from an		audio device.
--
-- This corresponds to kAudioDevicePropertyLatency and kAudioStreamPropertyLatency.		See <CoreAudio/AudioHardwareBase.h>.
--
-- ObjC selector: @- presentationLatency@
presentationLatency :: IsAVAudioIONode avAudioIONode => avAudioIONode -> IO CDouble
presentationLatency avAudioIONode  =
  sendMsg avAudioIONode (mkSelector "presentationLatency") retCDouble []

-- | audioUnit
--
-- The node's underlying AudioUnit, if any.
--
-- This is only necessary for certain advanced usages.
--
-- ObjC selector: @- audioUnit@
audioUnit :: IsAVAudioIONode avAudioIONode => avAudioIONode -> IO (Ptr ())
audioUnit avAudioIONode  =
  fmap castPtr $ sendMsg avAudioIONode (mkSelector "audioUnit") (retPtr retVoid) []

-- | voiceProcessingEnabled
--
-- Indicates whether voice processing is enabled.
--
-- ObjC selector: @- voiceProcessingEnabled@
voiceProcessingEnabled :: IsAVAudioIONode avAudioIONode => avAudioIONode -> IO Bool
voiceProcessingEnabled avAudioIONode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioIONode (mkSelector "voiceProcessingEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setVoiceProcessingEnabled:error:@
setVoiceProcessingEnabled_errorSelector :: Selector
setVoiceProcessingEnabled_errorSelector = mkSelector "setVoiceProcessingEnabled:error:"

-- | @Selector@ for @presentationLatency@
presentationLatencySelector :: Selector
presentationLatencySelector = mkSelector "presentationLatency"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector
audioUnitSelector = mkSelector "audioUnit"

-- | @Selector@ for @voiceProcessingEnabled@
voiceProcessingEnabledSelector :: Selector
voiceProcessingEnabledSelector = mkSelector "voiceProcessingEnabled"

