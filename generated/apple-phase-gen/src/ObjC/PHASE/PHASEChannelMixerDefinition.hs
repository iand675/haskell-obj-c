{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEChannelMixerDefinition
--
-- Channel mixer definition.
--
-- Channel mixers render audio without spatialization or environmental effects.        Use channel mixers for regular stem-based content that needs be rendered directly to the output device, such as stereo music        or center channel narrative dialogue.
--
-- Generated bindings for @PHASEChannelMixerDefinition@.
module ObjC.PHASE.PHASEChannelMixerDefinition
  ( PHASEChannelMixerDefinition
  , IsPHASEChannelMixerDefinition(..)
  , init_
  , new
  , initWithChannelLayout_identifier
  , initWithChannelLayout
  , inputChannelLayout
  , initSelector
  , initWithChannelLayoutSelector
  , initWithChannelLayout_identifierSelector
  , inputChannelLayoutSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEChannelMixerDefinition phaseChannelMixerDefinition => phaseChannelMixerDefinition -> IO (Id PHASEChannelMixerDefinition)
init_ phaseChannelMixerDefinition =
  sendOwnedMessage phaseChannelMixerDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEChannelMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEChannelMixerDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithChannelLayout:identifier
--
-- Create a new PHASEChannelMixerDefinition
--
-- Note: Any connected sampler must match this channel layout.
--
-- @layout@ — The input channel layout.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASEChannelMixerDefinition object
--
-- ObjC selector: @- initWithChannelLayout:identifier:@
initWithChannelLayout_identifier :: (IsPHASEChannelMixerDefinition phaseChannelMixerDefinition, IsAVAudioChannelLayout layout, IsNSString identifier) => phaseChannelMixerDefinition -> layout -> identifier -> IO (Id PHASEChannelMixerDefinition)
initWithChannelLayout_identifier phaseChannelMixerDefinition layout identifier =
  sendOwnedMessage phaseChannelMixerDefinition initWithChannelLayout_identifierSelector (toAVAudioChannelLayout layout) (toNSString identifier)

-- | initWithChannelLayout
--
-- Create a new PHASEChannelMixerDefinition
--
-- Note: Any connected sampler must match this channel layout.
--
-- @layout@ — The input channel layout. Any connected sampler must match this channel layout.
--
-- Returns: A new PHASEChannelMixerDefinition object
--
-- ObjC selector: @- initWithChannelLayout:@
initWithChannelLayout :: (IsPHASEChannelMixerDefinition phaseChannelMixerDefinition, IsAVAudioChannelLayout layout) => phaseChannelMixerDefinition -> layout -> IO (Id PHASEChannelMixerDefinition)
initWithChannelLayout phaseChannelMixerDefinition layout =
  sendOwnedMessage phaseChannelMixerDefinition initWithChannelLayoutSelector (toAVAudioChannelLayout layout)

-- | inputChannelLayout
--
-- A readonly value of the input channel layout this mixer was initialized with.
--
-- ObjC selector: @- inputChannelLayout@
inputChannelLayout :: IsPHASEChannelMixerDefinition phaseChannelMixerDefinition => phaseChannelMixerDefinition -> IO (Id AVAudioChannelLayout)
inputChannelLayout phaseChannelMixerDefinition =
  sendMessage phaseChannelMixerDefinition inputChannelLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEChannelMixerDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEChannelMixerDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithChannelLayout:identifier:@
initWithChannelLayout_identifierSelector :: Selector '[Id AVAudioChannelLayout, Id NSString] (Id PHASEChannelMixerDefinition)
initWithChannelLayout_identifierSelector = mkSelector "initWithChannelLayout:identifier:"

-- | @Selector@ for @initWithChannelLayout:@
initWithChannelLayoutSelector :: Selector '[Id AVAudioChannelLayout] (Id PHASEChannelMixerDefinition)
initWithChannelLayoutSelector = mkSelector "initWithChannelLayout:"

-- | @Selector@ for @inputChannelLayout@
inputChannelLayoutSelector :: Selector '[] (Id AVAudioChannelLayout)
inputChannelLayoutSelector = mkSelector "inputChannelLayout"

