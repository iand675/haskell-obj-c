{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESamplerNodeDefinition
--
-- Sampler node definition.
--
-- Sampler nodes play back registered sound assets.
--
-- Generated bindings for @PHASESamplerNodeDefinition@.
module ObjC.PHASE.PHASESamplerNodeDefinition
  ( PHASESamplerNodeDefinition
  , IsPHASESamplerNodeDefinition(..)
  , init_
  , new
  , initWithSoundAssetIdentifier_mixerDefinition_identifier
  , initWithSoundAssetIdentifier_mixerDefinition
  , assetIdentifier
  , cullOption
  , setCullOption
  , playbackMode
  , setPlaybackMode
  , assetIdentifierSelector
  , cullOptionSelector
  , initSelector
  , initWithSoundAssetIdentifier_mixerDefinitionSelector
  , initWithSoundAssetIdentifier_mixerDefinition_identifierSelector
  , newSelector
  , playbackModeSelector
  , setCullOptionSelector
  , setPlaybackModeSelector

  -- * Enum types
  , PHASECullOption(PHASECullOption)
  , pattern PHASECullOptionTerminate
  , pattern PHASECullOptionSleepWakeAtZero
  , pattern PHASECullOptionSleepWakeAtRandomOffset
  , pattern PHASECullOptionSleepWakeAtRealtimeOffset
  , pattern PHASECullOptionDoNotCull
  , PHASEPlaybackMode(PHASEPlaybackMode)
  , pattern PHASEPlaybackModeOneShot
  , pattern PHASEPlaybackModeLooping

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO (Id PHASESamplerNodeDefinition)
init_ phaseSamplerNodeDefinition =
  sendOwnedMessage phaseSamplerNodeDefinition initSelector

-- | @+ new@
new :: IO (Id PHASESamplerNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESamplerNodeDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithSoundAssetIdentifier:mixerDefinition:identifier
--
-- Create a sampler node definition
--
-- @soundAssetIdentifier@ — The identifier of the registered sound asset this sampler will play
--
-- @mixerDefinition@ — The mixer definition this sampler will be assigned to
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASESamplerNodeDefinition object
--
-- ObjC selector: @- initWithSoundAssetIdentifier:mixerDefinition:identifier:@
initWithSoundAssetIdentifier_mixerDefinition_identifier :: (IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition, IsNSString soundAssetIdentifier, IsPHASEMixerDefinition mixerDefinition, IsNSString identifier) => phaseSamplerNodeDefinition -> soundAssetIdentifier -> mixerDefinition -> identifier -> IO (Id PHASESamplerNodeDefinition)
initWithSoundAssetIdentifier_mixerDefinition_identifier phaseSamplerNodeDefinition soundAssetIdentifier mixerDefinition identifier =
  sendOwnedMessage phaseSamplerNodeDefinition initWithSoundAssetIdentifier_mixerDefinition_identifierSelector (toNSString soundAssetIdentifier) (toPHASEMixerDefinition mixerDefinition) (toNSString identifier)

-- | initWithSoundAssetIdentifier:mixerDefinition
--
-- Create a sampler node definition
--
-- @soundAssetIdentifier@ — The identifier of the registered sound asset this sampler will play
--
-- @mixerDefinition@ — The mixer definition this sampler will be assigned to
--
-- Returns: A new PHASESamplerNodeDefinition object
--
-- ObjC selector: @- initWithSoundAssetIdentifier:mixerDefinition:@
initWithSoundAssetIdentifier_mixerDefinition :: (IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition, IsNSString soundAssetIdentifier, IsPHASEMixerDefinition mixerDefinition) => phaseSamplerNodeDefinition -> soundAssetIdentifier -> mixerDefinition -> IO (Id PHASESamplerNodeDefinition)
initWithSoundAssetIdentifier_mixerDefinition phaseSamplerNodeDefinition soundAssetIdentifier mixerDefinition =
  sendOwnedMessage phaseSamplerNodeDefinition initWithSoundAssetIdentifier_mixerDefinitionSelector (toNSString soundAssetIdentifier) (toPHASEMixerDefinition mixerDefinition)

-- | assetIdentifier
--
-- The identifier that uniquely represents the registered sound asset this sampler will play.
--
-- ObjC selector: @- assetIdentifier@
assetIdentifier :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO (Id NSString)
assetIdentifier phaseSamplerNodeDefinition =
  sendMessage phaseSamplerNodeDefinition assetIdentifierSelector

-- | cullOption
--
-- The cull option for the sampler.
--
-- The default value is PHASECullOptionTerminate.
--
-- ObjC selector: @- cullOption@
cullOption :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO PHASECullOption
cullOption phaseSamplerNodeDefinition =
  sendMessage phaseSamplerNodeDefinition cullOptionSelector

-- | cullOption
--
-- The cull option for the sampler.
--
-- The default value is PHASECullOptionTerminate.
--
-- ObjC selector: @- setCullOption:@
setCullOption :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> PHASECullOption -> IO ()
setCullOption phaseSamplerNodeDefinition value =
  sendMessage phaseSamplerNodeDefinition setCullOptionSelector value

-- | playbackMode
--
-- The playback mode for the sampler.
--
-- If the playback mode is set to PHASEPlaybackModeOneShot, you need to make sure the the audio data in the registered sound asset associated with this sampler begins and ends at zero crossings. Otherwise, you'll hear a click when beginning playback and / or ending playback.    If the playback mode is set to PHASEPlaybackModeLooping, you need to make sure the audio data in the registered sound asset associated with this sampler loops smoothly from the end sample to the start sample. Please verify this during authoring. Failing to do so will result in audible clicks at loop boundaries.    The default value is PHASEPlaybackModeOneShot.
--
-- ObjC selector: @- playbackMode@
playbackMode :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO PHASEPlaybackMode
playbackMode phaseSamplerNodeDefinition =
  sendMessage phaseSamplerNodeDefinition playbackModeSelector

-- | playbackMode
--
-- The playback mode for the sampler.
--
-- If the playback mode is set to PHASEPlaybackModeOneShot, you need to make sure the the audio data in the registered sound asset associated with this sampler begins and ends at zero crossings. Otherwise, you'll hear a click when beginning playback and / or ending playback.    If the playback mode is set to PHASEPlaybackModeLooping, you need to make sure the audio data in the registered sound asset associated with this sampler loops smoothly from the end sample to the start sample. Please verify this during authoring. Failing to do so will result in audible clicks at loop boundaries.    The default value is PHASEPlaybackModeOneShot.
--
-- ObjC selector: @- setPlaybackMode:@
setPlaybackMode :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> PHASEPlaybackMode -> IO ()
setPlaybackMode phaseSamplerNodeDefinition value =
  sendMessage phaseSamplerNodeDefinition setPlaybackModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESamplerNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESamplerNodeDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSoundAssetIdentifier:mixerDefinition:identifier:@
initWithSoundAssetIdentifier_mixerDefinition_identifierSelector :: Selector '[Id NSString, Id PHASEMixerDefinition, Id NSString] (Id PHASESamplerNodeDefinition)
initWithSoundAssetIdentifier_mixerDefinition_identifierSelector = mkSelector "initWithSoundAssetIdentifier:mixerDefinition:identifier:"

-- | @Selector@ for @initWithSoundAssetIdentifier:mixerDefinition:@
initWithSoundAssetIdentifier_mixerDefinitionSelector :: Selector '[Id NSString, Id PHASEMixerDefinition] (Id PHASESamplerNodeDefinition)
initWithSoundAssetIdentifier_mixerDefinitionSelector = mkSelector "initWithSoundAssetIdentifier:mixerDefinition:"

-- | @Selector@ for @assetIdentifier@
assetIdentifierSelector :: Selector '[] (Id NSString)
assetIdentifierSelector = mkSelector "assetIdentifier"

-- | @Selector@ for @cullOption@
cullOptionSelector :: Selector '[] PHASECullOption
cullOptionSelector = mkSelector "cullOption"

-- | @Selector@ for @setCullOption:@
setCullOptionSelector :: Selector '[PHASECullOption] ()
setCullOptionSelector = mkSelector "setCullOption:"

-- | @Selector@ for @playbackMode@
playbackModeSelector :: Selector '[] PHASEPlaybackMode
playbackModeSelector = mkSelector "playbackMode"

-- | @Selector@ for @setPlaybackMode:@
setPlaybackModeSelector :: Selector '[PHASEPlaybackMode] ()
setPlaybackModeSelector = mkSelector "setPlaybackMode:"

