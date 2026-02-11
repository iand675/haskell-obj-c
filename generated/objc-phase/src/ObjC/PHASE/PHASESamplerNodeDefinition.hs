{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithSoundAssetIdentifier_mixerDefinition_identifierSelector
  , initWithSoundAssetIdentifier_mixerDefinitionSelector
  , assetIdentifierSelector
  , cullOptionSelector
  , setCullOptionSelector
  , playbackModeSelector
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
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO (Id PHASESamplerNodeDefinition)
init_ phaseSamplerNodeDefinition  =
  sendMsg phaseSamplerNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESamplerNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESamplerNodeDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithSoundAssetIdentifier_mixerDefinition_identifier phaseSamplerNodeDefinition  soundAssetIdentifier mixerDefinition identifier =
withObjCPtr soundAssetIdentifier $ \raw_soundAssetIdentifier ->
  withObjCPtr mixerDefinition $ \raw_mixerDefinition ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg phaseSamplerNodeDefinition (mkSelector "initWithSoundAssetIdentifier:mixerDefinition:identifier:") (retPtr retVoid) [argPtr (castPtr raw_soundAssetIdentifier :: Ptr ()), argPtr (castPtr raw_mixerDefinition :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithSoundAssetIdentifier_mixerDefinition phaseSamplerNodeDefinition  soundAssetIdentifier mixerDefinition =
withObjCPtr soundAssetIdentifier $ \raw_soundAssetIdentifier ->
  withObjCPtr mixerDefinition $ \raw_mixerDefinition ->
      sendMsg phaseSamplerNodeDefinition (mkSelector "initWithSoundAssetIdentifier:mixerDefinition:") (retPtr retVoid) [argPtr (castPtr raw_soundAssetIdentifier :: Ptr ()), argPtr (castPtr raw_mixerDefinition :: Ptr ())] >>= ownedObject . castPtr

-- | assetIdentifier
--
-- The identifier that uniquely represents the registered sound asset this sampler will play.
--
-- ObjC selector: @- assetIdentifier@
assetIdentifier :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO (Id NSString)
assetIdentifier phaseSamplerNodeDefinition  =
  sendMsg phaseSamplerNodeDefinition (mkSelector "assetIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cullOption
--
-- The cull option for the sampler.
--
-- The default value is PHASECullOptionTerminate.
--
-- ObjC selector: @- cullOption@
cullOption :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO PHASECullOption
cullOption phaseSamplerNodeDefinition  =
  fmap (coerce :: CLong -> PHASECullOption) $ sendMsg phaseSamplerNodeDefinition (mkSelector "cullOption") retCLong []

-- | cullOption
--
-- The cull option for the sampler.
--
-- The default value is PHASECullOptionTerminate.
--
-- ObjC selector: @- setCullOption:@
setCullOption :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> PHASECullOption -> IO ()
setCullOption phaseSamplerNodeDefinition  value =
  sendMsg phaseSamplerNodeDefinition (mkSelector "setCullOption:") retVoid [argCLong (coerce value)]

-- | playbackMode
--
-- The playback mode for the sampler.
--
-- If the playback mode is set to PHASEPlaybackModeOneShot, you need to make sure the the audio data in the registered sound asset associated with this sampler begins and ends at zero crossings. Otherwise, you'll hear a click when beginning playback and / or ending playback.    If the playback mode is set to PHASEPlaybackModeLooping, you need to make sure the audio data in the registered sound asset associated with this sampler loops smoothly from the end sample to the start sample. Please verify this during authoring. Failing to do so will result in audible clicks at loop boundaries.    The default value is PHASEPlaybackModeOneShot.
--
-- ObjC selector: @- playbackMode@
playbackMode :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> IO PHASEPlaybackMode
playbackMode phaseSamplerNodeDefinition  =
  fmap (coerce :: CLong -> PHASEPlaybackMode) $ sendMsg phaseSamplerNodeDefinition (mkSelector "playbackMode") retCLong []

-- | playbackMode
--
-- The playback mode for the sampler.
--
-- If the playback mode is set to PHASEPlaybackModeOneShot, you need to make sure the the audio data in the registered sound asset associated with this sampler begins and ends at zero crossings. Otherwise, you'll hear a click when beginning playback and / or ending playback.    If the playback mode is set to PHASEPlaybackModeLooping, you need to make sure the audio data in the registered sound asset associated with this sampler loops smoothly from the end sample to the start sample. Please verify this during authoring. Failing to do so will result in audible clicks at loop boundaries.    The default value is PHASEPlaybackModeOneShot.
--
-- ObjC selector: @- setPlaybackMode:@
setPlaybackMode :: IsPHASESamplerNodeDefinition phaseSamplerNodeDefinition => phaseSamplerNodeDefinition -> PHASEPlaybackMode -> IO ()
setPlaybackMode phaseSamplerNodeDefinition  value =
  sendMsg phaseSamplerNodeDefinition (mkSelector "setPlaybackMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSoundAssetIdentifier:mixerDefinition:identifier:@
initWithSoundAssetIdentifier_mixerDefinition_identifierSelector :: Selector
initWithSoundAssetIdentifier_mixerDefinition_identifierSelector = mkSelector "initWithSoundAssetIdentifier:mixerDefinition:identifier:"

-- | @Selector@ for @initWithSoundAssetIdentifier:mixerDefinition:@
initWithSoundAssetIdentifier_mixerDefinitionSelector :: Selector
initWithSoundAssetIdentifier_mixerDefinitionSelector = mkSelector "initWithSoundAssetIdentifier:mixerDefinition:"

-- | @Selector@ for @assetIdentifier@
assetIdentifierSelector :: Selector
assetIdentifierSelector = mkSelector "assetIdentifier"

-- | @Selector@ for @cullOption@
cullOptionSelector :: Selector
cullOptionSelector = mkSelector "cullOption"

-- | @Selector@ for @setCullOption:@
setCullOptionSelector :: Selector
setCullOptionSelector = mkSelector "setCullOption:"

-- | @Selector@ for @playbackMode@
playbackModeSelector :: Selector
playbackModeSelector = mkSelector "playbackMode"

-- | @Selector@ for @setPlaybackMode:@
setPlaybackModeSelector :: Selector
setPlaybackModeSelector = mkSelector "setPlaybackMode:"

