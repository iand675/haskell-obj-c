{-# LANGUAGE DataKinds #-}
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
  , nextAvailableInputBusSelector
  , outputVolumeSelector
  , setOutputVolumeSelector


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
init_ :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> IO (Id AVAudioMixerNode)
init_ avAudioMixerNode =
  sendOwnedMessage avAudioMixerNode initSelector

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- outputVolume@
outputVolume :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> IO CFloat
outputVolume avAudioMixerNode =
  sendMessage avAudioMixerNode outputVolumeSelector

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- setOutputVolume:@
setOutputVolume :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> CFloat -> IO ()
setOutputVolume avAudioMixerNode value =
  sendMessage avAudioMixerNode setOutputVolumeSelector value

-- | nextAvailableInputBus
--
-- Find an unused input bus.
--
-- This will find and return the first input bus to which no other node is connected.
--
-- ObjC selector: @- nextAvailableInputBus@
nextAvailableInputBus :: IsAVAudioMixerNode avAudioMixerNode => avAudioMixerNode -> IO CULong
nextAvailableInputBus avAudioMixerNode =
  sendMessage avAudioMixerNode nextAvailableInputBusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioMixerNode)
initSelector = mkSelector "init"

-- | @Selector@ for @outputVolume@
outputVolumeSelector :: Selector '[] CFloat
outputVolumeSelector = mkSelector "outputVolume"

-- | @Selector@ for @setOutputVolume:@
setOutputVolumeSelector :: Selector '[CFloat] ()
setOutputVolumeSelector = mkSelector "setOutputVolume:"

-- | @Selector@ for @nextAvailableInputBus@
nextAvailableInputBusSelector :: Selector '[] CULong
nextAvailableInputBusSelector = mkSelector "nextAvailableInputBus"

