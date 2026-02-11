{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioOutputNode
--
-- A node that performs audio output in the engine.
--
-- When the engine is rendering to/from an audio device, this node connects to the system's 		audio output.		When the engine is operating in manual rendering mode, this node performs output in		response to client's requests.
--
-- This node has one element.		The format of the output scope reflects:			- the audio hardware sample rate and channel count, when connected to the hardware			- the engine's manual rendering mode output format (see 			  @AVAudioEngine(manualRenderingFormat)@), in the manual rendering mode
--
-- The format of the input scope is initially the same as that of the		output, but you may set it to a different format, in which case the node will convert.
--
-- Generated bindings for @AVAudioOutputNode@.
module ObjC.AVFAudio.AVAudioOutputNode
  ( AVAudioOutputNode
  , IsAVAudioOutputNode(..)
  , init_
  , initSelector


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
init_ :: IsAVAudioOutputNode avAudioOutputNode => avAudioOutputNode -> IO (Id AVAudioOutputNode)
init_ avAudioOutputNode  =
  sendMsg avAudioOutputNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

