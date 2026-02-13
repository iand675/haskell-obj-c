{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEAmbientMixerDefinition
--
-- Ambient mixer definition.
--
-- Ambient mixers render audio with spatialization but without environmental effects.        Use ambient mixers for content that isn't being simulated in the environment,        but should still sound like it's coming from somewhere out in space.
--
-- Note: Ambient mixers do not support distance modeling or directivity modeling.        Clients can however set the orientation at initialization time.
--
-- Generated bindings for @PHASEAmbientMixerDefinition@.
module ObjC.PHASE.PHASEAmbientMixerDefinition
  ( PHASEAmbientMixerDefinition
  , IsPHASEAmbientMixerDefinition(..)
  , init_
  , new
  , inputChannelLayout
  , initSelector
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
init_ :: IsPHASEAmbientMixerDefinition phaseAmbientMixerDefinition => phaseAmbientMixerDefinition -> IO (Id PHASEAmbientMixerDefinition)
init_ phaseAmbientMixerDefinition =
  sendOwnedMessage phaseAmbientMixerDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEAmbientMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEAmbientMixerDefinition"
    sendOwnedClassMessage cls' newSelector

-- | inputChannelLayout
--
-- A readonly value of the input channel layout this mixer was initialized with.
--
-- ObjC selector: @- inputChannelLayout@
inputChannelLayout :: IsPHASEAmbientMixerDefinition phaseAmbientMixerDefinition => phaseAmbientMixerDefinition -> IO (Id AVAudioChannelLayout)
inputChannelLayout phaseAmbientMixerDefinition =
  sendMessage phaseAmbientMixerDefinition inputChannelLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEAmbientMixerDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEAmbientMixerDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @inputChannelLayout@
inputChannelLayoutSelector :: Selector '[] (Id AVAudioChannelLayout)
inputChannelLayoutSelector = mkSelector "inputChannelLayout"

