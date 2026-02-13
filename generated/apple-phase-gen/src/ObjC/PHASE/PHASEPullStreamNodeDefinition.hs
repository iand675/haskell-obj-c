{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEPullStreamNodeDefinition
--
-- An object for defining a pull stream sound event node when building a sound event.
--
-- Generated bindings for @PHASEPullStreamNodeDefinition@.
module ObjC.PHASE.PHASEPullStreamNodeDefinition
  ( PHASEPullStreamNodeDefinition
  , IsPHASEPullStreamNodeDefinition(..)
  , init_
  , new
  , initWithMixerDefinition_format_identifier
  , initWithMixerDefinition_format
  , format
  , normalize
  , setNormalize
  , formatSelector
  , initSelector
  , initWithMixerDefinition_formatSelector
  , initWithMixerDefinition_format_identifierSelector
  , newSelector
  , normalizeSelector
  , setNormalizeSelector


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
init_ :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> IO (Id PHASEPullStreamNodeDefinition)
init_ phasePullStreamNodeDefinition =
  sendOwnedMessage phasePullStreamNodeDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEPullStreamNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEPullStreamNodeDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithMixerDefinition:format:identifier
--
-- Create a pull stream node definition
--
-- @mixerDefinition@ — The mixer definition this stream will be assigned to
--
-- @format@ — The AVAudioFormat object that will define the attributes of the audio this node will accept.        Only Core Audio's standard deinterleaved 32-bit floating-point formats are supported.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASEPullStreamNodeDefinition object
--
-- ObjC selector: @- initWithMixerDefinition:format:identifier:@
initWithMixerDefinition_format_identifier :: (IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition, IsPHASEMixerDefinition mixerDefinition, IsAVAudioFormat format, IsNSString identifier) => phasePullStreamNodeDefinition -> mixerDefinition -> format -> identifier -> IO (Id PHASEPullStreamNodeDefinition)
initWithMixerDefinition_format_identifier phasePullStreamNodeDefinition mixerDefinition format identifier =
  sendOwnedMessage phasePullStreamNodeDefinition initWithMixerDefinition_format_identifierSelector (toPHASEMixerDefinition mixerDefinition) (toAVAudioFormat format) (toNSString identifier)

-- | initWithMixerDefinition:format
--
-- Create a pull stream node definition
--
-- @mixerDefinition@ — The mixer definition this stream will be assigned to
--
-- @format@ — The AVAudioFormat object that will define the attributes of the audio this node will accept.        Only Core Audio's standard deinterleaved 32-bit floating-point formats are supported.
--
-- Returns: A new PHASEPullStreamNodeDefinition object
--
-- ObjC selector: @- initWithMixerDefinition:format:@
initWithMixerDefinition_format :: (IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition, IsPHASEMixerDefinition mixerDefinition, IsAVAudioFormat format) => phasePullStreamNodeDefinition -> mixerDefinition -> format -> IO (Id PHASEPullStreamNodeDefinition)
initWithMixerDefinition_format phasePullStreamNodeDefinition mixerDefinition format =
  sendOwnedMessage phasePullStreamNodeDefinition initWithMixerDefinition_formatSelector (toPHASEMixerDefinition mixerDefinition) (toAVAudioFormat format)

-- | format
--
-- The readonly property that returns the AVAudioFormat that this stream was initialized with
--
-- ObjC selector: @- format@
format :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> IO (Id AVAudioFormat)
format phasePullStreamNodeDefinition =
  sendMessage phasePullStreamNodeDefinition formatSelector

-- | normalize
--
-- Determines whether or not the engine should normalize the stream. The default value is NO.
--
-- In general, clients are advised to normalize the input. Normalization is required to properly calibrate the output level.        If you set this value to NO, it's advised that you do custom normalization of the audio data prior to passing the buffers to PHASE.
--
-- ObjC selector: @- normalize@
normalize :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> IO Bool
normalize phasePullStreamNodeDefinition =
  sendMessage phasePullStreamNodeDefinition normalizeSelector

-- | normalize
--
-- Determines whether or not the engine should normalize the stream. The default value is NO.
--
-- In general, clients are advised to normalize the input. Normalization is required to properly calibrate the output level.        If you set this value to NO, it's advised that you do custom normalization of the audio data prior to passing the buffers to PHASE.
--
-- ObjC selector: @- setNormalize:@
setNormalize :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> Bool -> IO ()
setNormalize phasePullStreamNodeDefinition value =
  sendMessage phasePullStreamNodeDefinition setNormalizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEPullStreamNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEPullStreamNodeDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithMixerDefinition:format:identifier:@
initWithMixerDefinition_format_identifierSelector :: Selector '[Id PHASEMixerDefinition, Id AVAudioFormat, Id NSString] (Id PHASEPullStreamNodeDefinition)
initWithMixerDefinition_format_identifierSelector = mkSelector "initWithMixerDefinition:format:identifier:"

-- | @Selector@ for @initWithMixerDefinition:format:@
initWithMixerDefinition_formatSelector :: Selector '[Id PHASEMixerDefinition, Id AVAudioFormat] (Id PHASEPullStreamNodeDefinition)
initWithMixerDefinition_formatSelector = mkSelector "initWithMixerDefinition:format:"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id AVAudioFormat)
formatSelector = mkSelector "format"

-- | @Selector@ for @normalize@
normalizeSelector :: Selector '[] Bool
normalizeSelector = mkSelector "normalize"

-- | @Selector@ for @setNormalize:@
setNormalizeSelector :: Selector '[Bool] ()
setNormalizeSelector = mkSelector "setNormalize:"

