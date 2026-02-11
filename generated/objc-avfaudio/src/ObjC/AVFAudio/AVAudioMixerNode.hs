{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioMixerNode
--
-- A node that mixes its inputs to a single output.
--
-- Mixers may have any number of inputs.
--
-- The mixer accepts input at any sample rate and efficiently combines sample rate		conversions. It also accepts any channel count and will correctly upmix or downmix		to the output channel count.
--
-- Generated bindings for @AVAudioMixerNode@.
module ObjC.AVFAudio.AVAudioMixerNode
  ( AVAudioMixerNode
  , IsAVAudioMixerNode(..)
  , init_
  , outputVolume
  , setOutputVolume
  , nextAvailableInputBus
  , initSelector
  , outputVolumeSelector
  , setOutputVolumeSelector
  , nextAvailableInputBusSelector


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
init_ :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> IO (Id AVAudioMixerNode)
init_ avAudioMixerNode  =
  sendMsg avAudioMixerNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- outputVolume@
outputVolume :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> IO CFloat
outputVolume avAudioMixerNode  =
  sendMsg avAudioMixerNode (mkSelector "outputVolume") retCFloat []

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- setOutputVolume:@
setOutputVolume :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> CFloat -> IO ()
setOutputVolume avAudioMixerNode  value =
  sendMsg avAudioMixerNode (mkSelector "setOutputVolume:") retVoid [argCFloat (fromIntegral value)]

-- | nextAvailableInputBus
--
-- Find an unused input bus.
--
-- This will find and return the first input bus to which no other node is connected.
--
-- ObjC selector: @- nextAvailableInputBus@
nextAvailableInputBus :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> IO CULong
nextAvailableInputBus avAudioMixerNode  =
  sendMsg avAudioMixerNode (mkSelector "nextAvailableInputBus") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @outputVolume@
outputVolumeSelector :: Selector
outputVolumeSelector = mkSelector "outputVolume"

-- | @Selector@ for @setOutputVolume:@
setOutputVolumeSelector :: Selector
setOutputVolumeSelector = mkSelector "setOutputVolume:"

-- | @Selector@ for @nextAvailableInputBus@
nextAvailableInputBusSelector :: Selector
nextAvailableInputBusSelector = mkSelector "nextAvailableInputBus"

