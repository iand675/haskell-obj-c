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
  , newSelector
  , inputChannelLayoutSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEAmbientMixerDefinition phaseAmbientMixerDefinition => phaseAmbientMixerDefinition -> IO (Id PHASEAmbientMixerDefinition)
init_ phaseAmbientMixerDefinition  =
  sendMsg phaseAmbientMixerDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEAmbientMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEAmbientMixerDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | inputChannelLayout
--
-- A readonly value of the input channel layout this mixer was initialized with.
--
-- ObjC selector: @- inputChannelLayout@
inputChannelLayout :: IsPHASEAmbientMixerDefinition phaseAmbientMixerDefinition => phaseAmbientMixerDefinition -> IO (Id AVAudioChannelLayout)
inputChannelLayout phaseAmbientMixerDefinition  =
  sendMsg phaseAmbientMixerDefinition (mkSelector "inputChannelLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @inputChannelLayout@
inputChannelLayoutSelector :: Selector
inputChannelLayoutSelector = mkSelector "inputChannelLayout"

