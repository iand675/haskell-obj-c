{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioSourceNode
--
-- AVAudioSourceNode wraps a client provided block to supply audio.
--
-- With AVAudioSourceNode the client can supply audio data for rendering through an        AVAudioSourceNodeRenderBlock block.        This is similar to setting the input callback on an Audio Unit with the        kAudioUnitProperty_SetRenderCallback property.
--
-- Generated bindings for @AVAudioSourceNode@.
module ObjC.AVFAudio.AVAudioSourceNode
  ( AVAudioSourceNode
  , IsAVAudioSourceNode(..)
  , init_
  , initWithRenderBlock
  , initWithFormat_renderBlock
  , initSelector
  , initWithFormat_renderBlockSelector
  , initWithRenderBlockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioSourceNode avAudioSourceNode => avAudioSourceNode -> IO (Id AVAudioSourceNode)
init_ avAudioSourceNode =
  sendOwnedMessage avAudioSourceNode initSelector

-- | initWithRenderBlock:
--
-- Create a node with a render block.
--
-- @block@ — The block to supply audio data to the output.
--
-- The block can be called on realtime or non-realtime threads depending on the engine’s        operating mode and it is the client's responsibility to handle it in a thread-safe manner.
--
-- The audio format for the output bus will be set from the connection format when connecting        to another node.
--
-- The audio format for the block will be set to the node's output format. If node is        reconnected with a different output format, the audio format for the block will also change.
--
-- ObjC selector: @- initWithRenderBlock:@
initWithRenderBlock :: IsAVAudioSourceNode avAudioSourceNode => avAudioSourceNode -> Ptr () -> IO (Id AVAudioSourceNode)
initWithRenderBlock avAudioSourceNode block =
  sendOwnedMessage avAudioSourceNode initWithRenderBlockSelector block

-- | initWithFormat:renderBlock:
--
-- Create a node with a render block.
--
-- @format@ — The format of the PCM audio data that will be supplied by the block.
--
-- @block@ — The block to supply audio data to the output.
--
-- The block can be called on realtime or non-realtime threads depending on the engine’s        operating mode and it is the client's responsibility to handle it in a thread-safe manner.
--
-- The audio format for the output bus will be set from the connection format when connecting        to another node.
--
-- AVAudioSourceNode supports different audio formats for the block and output, but only        Linear PCM conversions are supported (sample rate, bit depth, interleaving).
--
-- ObjC selector: @- initWithFormat:renderBlock:@
initWithFormat_renderBlock :: (IsAVAudioSourceNode avAudioSourceNode, IsAVAudioFormat format) => avAudioSourceNode -> format -> Ptr () -> IO (Id AVAudioSourceNode)
initWithFormat_renderBlock avAudioSourceNode format block =
  sendOwnedMessage avAudioSourceNode initWithFormat_renderBlockSelector (toAVAudioFormat format) block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioSourceNode)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRenderBlock:@
initWithRenderBlockSelector :: Selector '[Ptr ()] (Id AVAudioSourceNode)
initWithRenderBlockSelector = mkSelector "initWithRenderBlock:"

-- | @Selector@ for @initWithFormat:renderBlock:@
initWithFormat_renderBlockSelector :: Selector '[Id AVAudioFormat, Ptr ()] (Id AVAudioSourceNode)
initWithFormat_renderBlockSelector = mkSelector "initWithFormat:renderBlock:"

