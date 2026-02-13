{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetReaderAudioMixOutput
--
-- AVAssetReaderAudioMixOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading audio samples that result from mixing the audio from one or more AVAssetTracks of an AVAssetReader's AVAsset.
--
-- Clients can read the audio data mixed from one or more asset tracks by adding an instance of AVAssetReaderAudioMixOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method.
--
-- Generated bindings for @AVAssetReaderAudioMixOutput@.
module ObjC.AVFoundation.AVAssetReaderAudioMixOutput
  ( AVAssetReaderAudioMixOutput
  , IsAVAssetReaderAudioMixOutput(..)
  , init_
  , new
  , assetReaderAudioMixOutputWithAudioTracks_audioSettings
  , initWithAudioTracks_audioSettings
  , audioTracks
  , audioSettings
  , audioMix
  , setAudioMix
  , audioTimePitchAlgorithm
  , setAudioTimePitchAlgorithm
  , assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector
  , audioMixSelector
  , audioSettingsSelector
  , audioTimePitchAlgorithmSelector
  , audioTracksSelector
  , initSelector
  , initWithAudioTracks_audioSettingsSelector
  , newSelector
  , setAudioMixSelector
  , setAudioTimePitchAlgorithmSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id AVAssetReaderAudioMixOutput)
init_ avAssetReaderAudioMixOutput =
  sendOwnedMessage avAssetReaderAudioMixOutput initSelector

-- | @+ new@
new :: IO (Id AVAssetReaderAudioMixOutput)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderAudioMixOutput"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderAudioMixOutputWithAudioTracks:audioSettings:
--
-- Returns an instance of AVAssetReaderAudioMixOutput for reading mixed audio from the specified audio tracks, with optional audio settings.
--
-- @tracks@ — An NSArray of AVAssetTrack objects from which the created object should read sample buffers to be mixed.
--
-- @audioSettings@ — An NSDictionary of audio settings to be used for audio output.
--
-- Returns: An instance of AVAssetReaderAudioMixOutput.
--
-- Each track must be one of the tracks owned by the target AVAssetReader's asset and must be of media type AVMediaTypeAudio.
--
-- For non-nil values of audioSettings, the audio settings dictionary must contain values for keys in AVAudioSettings.h (linear PCM only). Initialization will fail if the audio settings cannot be used with the specified tracks. AVSampleRateConverterAudioQualityKey is not supported.
--
-- A value of nil for audioSettings configures the output to return samples in a convenient uncompressed format, with sample rate and other properties determined according to the properties of the specified audio tracks as well as other considerations that may vary according to device capabilities, operating system version, and other factors. Therefore if you wish to perform any processing on the output, you must examine the CMAudioFormatDescription of the CMSampleBuffers that are provided in order to ensure that your processing is appropriately configured for the output format.
--
-- ObjC selector: @+ assetReaderAudioMixOutputWithAudioTracks:audioSettings:@
assetReaderAudioMixOutputWithAudioTracks_audioSettings :: (IsNSArray audioTracks, IsNSDictionary audioSettings) => audioTracks -> audioSettings -> IO (Id AVAssetReaderAudioMixOutput)
assetReaderAudioMixOutputWithAudioTracks_audioSettings audioTracks audioSettings =
  do
    cls' <- getRequiredClass "AVAssetReaderAudioMixOutput"
    sendClassMessage cls' assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector (toNSArray audioTracks) (toNSDictionary audioSettings)

-- | initWithAudioTracks:audioSettings:
--
-- Creates an instance of AVAssetReaderAudioMixOutput for reading mixed audio from the specified audio tracks, with optional audio settings.
--
-- @tracks@ — An NSArray of AVAssetTrack objects from which the created object should read sample buffers to be mixed.
--
-- @audioSettings@ — An NSDictionary of audio settings to be used for audio output.
--
-- Returns: An instance of AVAssetReaderAudioMixOutput.
--
-- Each track must be one of the tracks owned by the target AVAssetReader's asset and must be of media type AVMediaTypeAudio.
--
-- For non-nil values of audioSettings, the audio settings dictionary must contain values for keys in AVAudioSettings.h (linear PCM only). Initialization will fail if the audio settings cannot be used with the specified tracks. AVSampleRateConverterAudioQualityKey is not supported.
--
-- A value of nil for audioSettings configures the output to return samples in a convenient uncompressed format, with sample rate and other properties determined according to the properties of the specified audio tracks as well as other considerations that may vary according to device capabilities, operating system version, and other factors. Therefore if you wish to perform any processing on the output, you must examine the CMAudioFormatDescription of the CMSampleBuffers that are provided in order to ensure that your processing is appropriately configured for the output format.
--
-- This method throws an exception for any of the following reasons:		- an audio track does not have media type AVMediaTypeAudio		- an audio track belongs to a different AVAsset		- the audio settings contains an AVSampleRateConverterAudioQualityKey		- the output would be compressed
--
-- ObjC selector: @- initWithAudioTracks:audioSettings:@
initWithAudioTracks_audioSettings :: (IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput, IsNSArray audioTracks, IsNSDictionary audioSettings) => avAssetReaderAudioMixOutput -> audioTracks -> audioSettings -> IO (Id AVAssetReaderAudioMixOutput)
initWithAudioTracks_audioSettings avAssetReaderAudioMixOutput audioTracks audioSettings =
  sendOwnedMessage avAssetReaderAudioMixOutput initWithAudioTracks_audioSettingsSelector (toNSArray audioTracks) (toNSDictionary audioSettings)

-- | audioTracks
--
-- The tracks from which the receiver reads mixed audio.
--
-- The value of this property is an NSArray of AVAssetTracks owned by the target AVAssetReader's asset.
--
-- ObjC selector: @- audioTracks@
audioTracks :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id NSArray)
audioTracks avAssetReaderAudioMixOutput =
  sendMessage avAssetReaderAudioMixOutput audioTracksSelector

-- | audioSettings
--
-- The audio settings used by the receiver.
--
-- The value of this property is an NSDictionary that contains values for keys from AVAudioSettings.h (linear PCM only).  A value of nil indicates that the receiver will return audio samples in a convenient uncompressed format, with sample rate and other properties determined according to the properties of the receiver's audio tracks.
--
-- ObjC selector: @- audioSettings@
audioSettings :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id NSDictionary)
audioSettings avAssetReaderAudioMixOutput =
  sendMessage avAssetReaderAudioMixOutput audioSettingsSelector

-- | audioMix
--
-- The audio mix used by the receiver.
--
-- The value of this property is an AVAudioMix that can be used to specify how the volume of audio samples read from each source track will change over the timeline of the source asset.
--
-- This property throws an exception for any of the following reasons:		- an audio mix is set after reading has started (the asset reader has progressed beyond AVAssetReaderStatusUnknown)		- setting an audio mix containing a track that was not used to create the receiver		- an audio mix is set containing an invalid audio time pitch algorithm
--
-- ObjC selector: @- audioMix@
audioMix :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id AVAudioMix)
audioMix avAssetReaderAudioMixOutput =
  sendMessage avAssetReaderAudioMixOutput audioMixSelector

-- | audioMix
--
-- The audio mix used by the receiver.
--
-- The value of this property is an AVAudioMix that can be used to specify how the volume of audio samples read from each source track will change over the timeline of the source asset.
--
-- This property throws an exception for any of the following reasons:		- an audio mix is set after reading has started (the asset reader has progressed beyond AVAssetReaderStatusUnknown)		- setting an audio mix containing a track that was not used to create the receiver		- an audio mix is set containing an invalid audio time pitch algorithm
--
-- ObjC selector: @- setAudioMix:@
setAudioMix :: (IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput, IsAVAudioMix value) => avAssetReaderAudioMixOutput -> value -> IO ()
setAudioMix avAssetReaderAudioMixOutput value =
  sendMessage avAssetReaderAudioMixOutput setAudioMixSelector (toAVAudioMix value)

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchAlgorithmSpectral, are defined in AVAudioProcessingSettings.h.  An NSInvalidArgumentException will be raised if this property is set to a value other than the constants defined in that file.
--
-- The default value is AVAudioTimePitchAlgorithmSpectral.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id NSString)
audioTimePitchAlgorithm avAssetReaderAudioMixOutput =
  sendMessage avAssetReaderAudioMixOutput audioTimePitchAlgorithmSelector

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchAlgorithmSpectral, are defined in AVAudioProcessingSettings.h.  An NSInvalidArgumentException will be raised if this property is set to a value other than the constants defined in that file.
--
-- The default value is AVAudioTimePitchAlgorithmSpectral.
--
-- ObjC selector: @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput, IsNSString value) => avAssetReaderAudioMixOutput -> value -> IO ()
setAudioTimePitchAlgorithm avAssetReaderAudioMixOutput value =
  sendMessage avAssetReaderAudioMixOutput setAudioTimePitchAlgorithmSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReaderAudioMixOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReaderAudioMixOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderAudioMixOutputWithAudioTracks:audioSettings:@
assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector :: Selector '[Id NSArray, Id NSDictionary] (Id AVAssetReaderAudioMixOutput)
assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector = mkSelector "assetReaderAudioMixOutputWithAudioTracks:audioSettings:"

-- | @Selector@ for @initWithAudioTracks:audioSettings:@
initWithAudioTracks_audioSettingsSelector :: Selector '[Id NSArray, Id NSDictionary] (Id AVAssetReaderAudioMixOutput)
initWithAudioTracks_audioSettingsSelector = mkSelector "initWithAudioTracks:audioSettings:"

-- | @Selector@ for @audioTracks@
audioTracksSelector :: Selector '[] (Id NSArray)
audioTracksSelector = mkSelector "audioTracks"

-- | @Selector@ for @audioSettings@
audioSettingsSelector :: Selector '[] (Id NSDictionary)
audioSettingsSelector = mkSelector "audioSettings"

-- | @Selector@ for @audioMix@
audioMixSelector :: Selector '[] (Id AVAudioMix)
audioMixSelector = mkSelector "audioMix"

-- | @Selector@ for @setAudioMix:@
setAudioMixSelector :: Selector '[Id AVAudioMix] ()
setAudioMixSelector = mkSelector "setAudioMix:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector '[] (Id NSString)
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector '[Id NSString] ()
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

