{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEAssetRegistry
--
-- Asset registry
--
-- Generated bindings for @PHASEAssetRegistry@.
module ObjC.PHASE.PHASEAssetRegistry
  ( PHASEAssetRegistry
  , IsPHASEAssetRegistry(..)
  , init_
  , new
  , registerGlobalMetaParameter_error
  , registerSoundEventAssetWithRootNode_identifier_error
  , registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_error
  , registerSoundAssetWithData_identifier_format_normalizationMode_error
  , unregisterAssetWithIdentifier_completion
  , assetForIdentifier
  , globalMetaParameters
  , assetForIdentifierSelector
  , globalMetaParametersSelector
  , initSelector
  , newSelector
  , registerGlobalMetaParameter_errorSelector
  , registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_errorSelector
  , registerSoundAssetWithData_identifier_format_normalizationMode_errorSelector
  , registerSoundEventAssetWithRootNode_identifier_errorSelector
  , unregisterAssetWithIdentifier_completionSelector

  -- * Enum types
  , PHASEAssetType(PHASEAssetType)
  , pattern PHASEAssetTypeResident
  , pattern PHASEAssetTypeStreamed
  , PHASENormalizationMode(PHASENormalizationMode)
  , pattern PHASENormalizationModeNone
  , pattern PHASENormalizationModeDynamic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEAssetRegistry phaseAssetRegistry => phaseAssetRegistry -> IO (Id PHASEAssetRegistry)
init_ phaseAssetRegistry =
  sendOwnedMessage phaseAssetRegistry initSelector

-- | @+ new@
new :: IO (Id PHASEAssetRegistry)
new  =
  do
    cls' <- getRequiredClass "PHASEAssetRegistry"
    sendOwnedClassMessage cls' newSelector

-- | registerGlobalMetaParameter:error
--
-- Register a global metaparameter with the asset registry.
--
-- Note: This function is synchronous and thread-safe.        Clients can safely run this function to register multiple global metaparameters from multiple threads, if required.
--
-- @metaParameterDefinition@ — The metaparameter object to register.
--
-- @error@ — The error object in case of an error.
--
-- Returns: A PHASEGlobalMetaParameterAsset object.
--
-- ObjC selector: @- registerGlobalMetaParameter:error:@
registerGlobalMetaParameter_error :: (IsPHASEAssetRegistry phaseAssetRegistry, IsPHASEMetaParameterDefinition metaParameterDefinition, IsNSError error_) => phaseAssetRegistry -> metaParameterDefinition -> error_ -> IO (Id PHASEGlobalMetaParameterAsset)
registerGlobalMetaParameter_error phaseAssetRegistry metaParameterDefinition error_ =
  sendMessage phaseAssetRegistry registerGlobalMetaParameter_errorSelector (toPHASEMetaParameterDefinition metaParameterDefinition) (toNSError error_)

-- | registerSoundEventAssetWithRootNode:identifier:error
--
-- Register a sound event asset with the asset registry.
--
-- Note: This function is synchronous and thread-safe.        Clients can safely run this function to register multiple sound event assets from multiple threads, if required.
--
-- @rootNode@ — The root node of the sound event asset to register.
--
-- @identifier@ — An identifier that uniquely represents this sound event asset. Nil generates an automatic identifier.
--
-- @error@ — The error object in case of an error
--
-- Returns: A PHASESoundEventNodeAsset object
--
-- ObjC selector: @- registerSoundEventAssetWithRootNode:identifier:error:@
registerSoundEventAssetWithRootNode_identifier_error :: (IsPHASEAssetRegistry phaseAssetRegistry, IsPHASESoundEventNodeDefinition rootNode, IsNSString identifier, IsNSError error_) => phaseAssetRegistry -> rootNode -> identifier -> error_ -> IO (Id PHASESoundEventNodeAsset)
registerSoundEventAssetWithRootNode_identifier_error phaseAssetRegistry rootNode identifier error_ =
  sendMessage phaseAssetRegistry registerSoundEventAssetWithRootNode_identifier_errorSelector (toPHASESoundEventNodeDefinition rootNode) (toNSString identifier) (toNSError error_)

-- | registerSoundAssetAtURL:identifier:assetType:channelLayout:normalizationMode:error
--
-- Register an audio file as a sound asset in the system.
--
-- Note: This function is synchronous and thread-safe.        Clients can safely run this function to register multiple sound assets from multiple threads, if required.
--
-- @url@ — The URL of the audio file.
--
-- @identifier@ — An identifier that uniquely represents this sound event asset. Nil generates an automatic identifier.
--
-- @assetType@ — The asset type for this sound asset.
--
-- @channelLayout@ — The audio channel layout for this sound asset.        If a valid channel layout definition is read from the file being registered, this will override it.        If nil is passed as a value for this property, the file must either be mono or stereo, or already contain a vaild channel layout definition.        This channel layout must have the same channel count as the audio file being loaded.
--
-- @normalizationMode@ — The normalization mode.
--
-- @error@ — The error object in case of an error
--
-- Returns: A PHASESoundAsset object
--
-- ObjC selector: @- registerSoundAssetAtURL:identifier:assetType:channelLayout:normalizationMode:error:@
registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_error :: (IsPHASEAssetRegistry phaseAssetRegistry, IsNSURL url, IsNSString identifier, IsAVAudioChannelLayout channelLayout, IsNSError error_) => phaseAssetRegistry -> url -> identifier -> PHASEAssetType -> channelLayout -> PHASENormalizationMode -> error_ -> IO (Id PHASESoundAsset)
registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_error phaseAssetRegistry url identifier assetType channelLayout normalizationMode error_ =
  sendMessage phaseAssetRegistry registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_errorSelector (toNSURL url) (toNSString identifier) assetType (toAVAudioChannelLayout channelLayout) normalizationMode (toNSError error_)

-- | registerSoundAssetWithData:identifier:format:normalizationMode:error
--
-- Register audio data as a sound asset in the system.
--
-- Note: This function is synchronous and thread-safe.        Clients can safely run this function to register multiple sound assets from multiple threads, if required.
--
-- @data@ — A buffer containing the audio data to register as a sound asset.        Audio data must either be a single PCM buffer of interleaved channels or multiple deinterleaved PCM buffers per channel packed back to back.
--
-- @identifier@ — The identifier to assign to this sound asset. Nil generates an automatic identifier.
--
-- @format@ — The AVAudioFormat object that describes the audio data in the buffer.
--
-- @normalizationMode@ — The normalization mode.
--
-- @error@ — The error object in case of an error.
--
-- Returns: A PHASESoundAsset object.
--
-- ObjC selector: @- registerSoundAssetWithData:identifier:format:normalizationMode:error:@
registerSoundAssetWithData_identifier_format_normalizationMode_error :: (IsPHASEAssetRegistry phaseAssetRegistry, IsNSData data_, IsNSString identifier, IsAVAudioFormat format, IsNSError error_) => phaseAssetRegistry -> data_ -> identifier -> format -> PHASENormalizationMode -> error_ -> IO (Id PHASESoundAsset)
registerSoundAssetWithData_identifier_format_normalizationMode_error phaseAssetRegistry data_ identifier format normalizationMode error_ =
  sendMessage phaseAssetRegistry registerSoundAssetWithData_identifier_format_normalizationMode_errorSelector (toNSData data_) (toNSString identifier) (toAVAudioFormat format) normalizationMode (toNSError error_)

-- | unregisterAssetWithIdentifier:completion:
--
-- Unregister and unload an asset.
--
-- @identifier@ — The identifier of the PHASEAsset object to unregister
--
-- @handler@ — An optional completion block that will be called when the asset has been unregistered.        Once you receive this callback, it's safe to deallocate external resources, if applicable.
--
-- ObjC selector: @- unregisterAssetWithIdentifier:completion:@
unregisterAssetWithIdentifier_completion :: (IsPHASEAssetRegistry phaseAssetRegistry, IsNSString identifier) => phaseAssetRegistry -> identifier -> Ptr () -> IO ()
unregisterAssetWithIdentifier_completion phaseAssetRegistry identifier handler =
  sendMessage phaseAssetRegistry unregisterAssetWithIdentifier_completionSelector (toNSString identifier) handler

-- | assetForIdentifier
--
-- Finds an asset in the asset registry, given an identifier.
--
-- @identifier@ — The identifier of this asset
--
-- Returns: A PHASEAsset object, or nil if one could not be found.
--
-- ObjC selector: @- assetForIdentifier:@
assetForIdentifier :: (IsPHASEAssetRegistry phaseAssetRegistry, IsNSString identifier) => phaseAssetRegistry -> identifier -> IO (Id PHASEAsset)
assetForIdentifier phaseAssetRegistry identifier =
  sendMessage phaseAssetRegistry assetForIdentifierSelector (toNSString identifier)

-- | globalMetaParameters
--
-- A dictionary of global metaparameters
--
-- ObjC selector: @- globalMetaParameters@
globalMetaParameters :: IsPHASEAssetRegistry phaseAssetRegistry => phaseAssetRegistry -> IO (Id NSDictionary)
globalMetaParameters phaseAssetRegistry =
  sendMessage phaseAssetRegistry globalMetaParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEAssetRegistry)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEAssetRegistry)
newSelector = mkSelector "new"

-- | @Selector@ for @registerGlobalMetaParameter:error:@
registerGlobalMetaParameter_errorSelector :: Selector '[Id PHASEMetaParameterDefinition, Id NSError] (Id PHASEGlobalMetaParameterAsset)
registerGlobalMetaParameter_errorSelector = mkSelector "registerGlobalMetaParameter:error:"

-- | @Selector@ for @registerSoundEventAssetWithRootNode:identifier:error:@
registerSoundEventAssetWithRootNode_identifier_errorSelector :: Selector '[Id PHASESoundEventNodeDefinition, Id NSString, Id NSError] (Id PHASESoundEventNodeAsset)
registerSoundEventAssetWithRootNode_identifier_errorSelector = mkSelector "registerSoundEventAssetWithRootNode:identifier:error:"

-- | @Selector@ for @registerSoundAssetAtURL:identifier:assetType:channelLayout:normalizationMode:error:@
registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_errorSelector :: Selector '[Id NSURL, Id NSString, PHASEAssetType, Id AVAudioChannelLayout, PHASENormalizationMode, Id NSError] (Id PHASESoundAsset)
registerSoundAssetAtURL_identifier_assetType_channelLayout_normalizationMode_errorSelector = mkSelector "registerSoundAssetAtURL:identifier:assetType:channelLayout:normalizationMode:error:"

-- | @Selector@ for @registerSoundAssetWithData:identifier:format:normalizationMode:error:@
registerSoundAssetWithData_identifier_format_normalizationMode_errorSelector :: Selector '[Id NSData, Id NSString, Id AVAudioFormat, PHASENormalizationMode, Id NSError] (Id PHASESoundAsset)
registerSoundAssetWithData_identifier_format_normalizationMode_errorSelector = mkSelector "registerSoundAssetWithData:identifier:format:normalizationMode:error:"

-- | @Selector@ for @unregisterAssetWithIdentifier:completion:@
unregisterAssetWithIdentifier_completionSelector :: Selector '[Id NSString, Ptr ()] ()
unregisterAssetWithIdentifier_completionSelector = mkSelector "unregisterAssetWithIdentifier:completion:"

-- | @Selector@ for @assetForIdentifier:@
assetForIdentifierSelector :: Selector '[Id NSString] (Id PHASEAsset)
assetForIdentifierSelector = mkSelector "assetForIdentifier:"

-- | @Selector@ for @globalMetaParameters@
globalMetaParametersSelector :: Selector '[] (Id NSDictionary)
globalMetaParametersSelector = mkSelector "globalMetaParameters"

