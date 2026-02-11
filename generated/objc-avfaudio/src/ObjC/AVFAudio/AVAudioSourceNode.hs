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
  , initWithRenderBlockSelector
  , initWithFormat_renderBlockSelector


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

-- | @- init@
init_ :: IsAVAudioSourceNode avAudioSourceNode => avAudioSourceNode -> IO (Id AVAudioSourceNode)
init_ avAudioSourceNode  =
  sendMsg avAudioSourceNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithRenderBlock avAudioSourceNode  block =
  sendMsg avAudioSourceNode (mkSelector "initWithRenderBlock:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ())] >>= ownedObject . castPtr

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
initWithFormat_renderBlock avAudioSourceNode  format block =
withObjCPtr format $ \raw_format ->
    sendMsg avAudioSourceNode (mkSelector "initWithFormat:renderBlock:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr block :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRenderBlock:@
initWithRenderBlockSelector :: Selector
initWithRenderBlockSelector = mkSelector "initWithRenderBlock:"

-- | @Selector@ for @initWithFormat:renderBlock:@
initWithFormat_renderBlockSelector :: Selector
initWithFormat_renderBlockSelector = mkSelector "initWithFormat:renderBlock:"

