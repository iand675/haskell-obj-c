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
  , newSelector
  , initWithChannelLayout_identifierSelector
  , initWithChannelLayoutSelector
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
init_ :: IsPHASEChannelMixerDefinition phaseChannelMixerDefinition => phaseChannelMixerDefinition -> IO (Id PHASEChannelMixerDefinition)
init_ phaseChannelMixerDefinition  =
  sendMsg phaseChannelMixerDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEChannelMixerDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEChannelMixerDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithChannelLayout_identifier phaseChannelMixerDefinition  layout identifier =
withObjCPtr layout $ \raw_layout ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg phaseChannelMixerDefinition (mkSelector "initWithChannelLayout:identifier:") (retPtr retVoid) [argPtr (castPtr raw_layout :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithChannelLayout phaseChannelMixerDefinition  layout =
withObjCPtr layout $ \raw_layout ->
    sendMsg phaseChannelMixerDefinition (mkSelector "initWithChannelLayout:") (retPtr retVoid) [argPtr (castPtr raw_layout :: Ptr ())] >>= ownedObject . castPtr

-- | inputChannelLayout
--
-- A readonly value of the input channel layout this mixer was initialized with.
--
-- ObjC selector: @- inputChannelLayout@
inputChannelLayout :: IsPHASEChannelMixerDefinition phaseChannelMixerDefinition => phaseChannelMixerDefinition -> IO (Id AVAudioChannelLayout)
inputChannelLayout phaseChannelMixerDefinition  =
  sendMsg phaseChannelMixerDefinition (mkSelector "inputChannelLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithChannelLayout:identifier:@
initWithChannelLayout_identifierSelector :: Selector
initWithChannelLayout_identifierSelector = mkSelector "initWithChannelLayout:identifier:"

-- | @Selector@ for @initWithChannelLayout:@
initWithChannelLayoutSelector :: Selector
initWithChannelLayoutSelector = mkSelector "initWithChannelLayout:"

-- | @Selector@ for @inputChannelLayout@
inputChannelLayoutSelector :: Selector
inputChannelLayoutSelector = mkSelector "inputChannelLayout"

