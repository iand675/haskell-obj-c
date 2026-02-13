{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioSinkNode
--
-- AVAudioSinkNode wraps a client provided block to receive input audio on the audio IO thread.
--
-- AVAudioSinkNode is restricted to be used in the input chain and does not support format        conversion. Hence when connecting to an AVAudioSinkNode node, the format for the connection        should be the output scope format of the input node (essentialy the format should match the input hardware 		sample rate).
--
-- The voice processing IO unit is an exception to the above as it supports sample rate conversion.         The input scope format (HW format) and output scope format (client format) of the input node can differ         in that case.
--
-- This node is only supported when the engine is rendering to the audio device and not in        manual rendering mode.
--
-- AVAudioSinkNode does not have an output bus and therefore it does not support tapping.
--
-- Generated bindings for @AVAudioSinkNode@.
module ObjC.AVFAudio.AVAudioSinkNode
  ( AVAudioSinkNode
  , IsAVAudioSinkNode(..)
  , init_
  , initWithReceiverBlock
  , initSelector
  , initWithReceiverBlockSelector


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
init_ :: IsAVAudioSinkNode avAudioSinkNode => avAudioSinkNode -> IO (Id AVAudioSinkNode)
init_ avAudioSinkNode =
  sendOwnedMessage avAudioSinkNode initSelector

-- | initWithReceiverBlock:
--
-- Create a node with a receiver block.
--
-- @block@ — The block that receives audio data from the input.
--
-- The receiver block is called when the input data is available.
--
-- The block will be called on the realtime thread and it is the client's responsibility to        handle it in a thread-safe manner and to not make any blocking calls.
--
-- The audio format for the input bus will be set from the connection format when connecting        to another node.
--
-- The audio format for the data received by the block will be set to the node's input format.
--
-- ObjC selector: @- initWithReceiverBlock:@
initWithReceiverBlock :: IsAVAudioSinkNode avAudioSinkNode => avAudioSinkNode -> Ptr () -> IO (Id AVAudioSinkNode)
initWithReceiverBlock avAudioSinkNode block =
  sendOwnedMessage avAudioSinkNode initWithReceiverBlockSelector block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioSinkNode)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithReceiverBlock:@
initWithReceiverBlockSelector :: Selector '[Ptr ()] (Id AVAudioSinkNode)
initWithReceiverBlockSelector = mkSelector "initWithReceiverBlock:"

