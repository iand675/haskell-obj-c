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
  , initSelector
  , newSelector
  , assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector
  , initWithAudioTracks_audioSettingsSelector
  , audioTracksSelector
  , audioSettingsSelector
  , audioMixSelector
  , setAudioMixSelector
  , audioTimePitchAlgorithmSelector
  , setAudioTimePitchAlgorithmSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id AVAssetReaderAudioMixOutput)
init_ avAssetReaderAudioMixOutput  =
  sendMsg avAssetReaderAudioMixOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetReaderAudioMixOutput)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderAudioMixOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr audioTracks $ \raw_audioTracks ->
      withObjCPtr audioSettings $ \raw_audioSettings ->
        sendClassMsg cls' (mkSelector "assetReaderAudioMixOutputWithAudioTracks:audioSettings:") (retPtr retVoid) [argPtr (castPtr raw_audioTracks :: Ptr ()), argPtr (castPtr raw_audioSettings :: Ptr ())] >>= retainedObject . castPtr

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
initWithAudioTracks_audioSettings avAssetReaderAudioMixOutput  audioTracks audioSettings =
withObjCPtr audioTracks $ \raw_audioTracks ->
  withObjCPtr audioSettings $ \raw_audioSettings ->
      sendMsg avAssetReaderAudioMixOutput (mkSelector "initWithAudioTracks:audioSettings:") (retPtr retVoid) [argPtr (castPtr raw_audioTracks :: Ptr ()), argPtr (castPtr raw_audioSettings :: Ptr ())] >>= ownedObject . castPtr

-- | audioTracks
--
-- The tracks from which the receiver reads mixed audio.
--
-- The value of this property is an NSArray of AVAssetTracks owned by the target AVAssetReader's asset.
--
-- ObjC selector: @- audioTracks@
audioTracks :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id NSArray)
audioTracks avAssetReaderAudioMixOutput  =
  sendMsg avAssetReaderAudioMixOutput (mkSelector "audioTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioSettings
--
-- The audio settings used by the receiver.
--
-- The value of this property is an NSDictionary that contains values for keys from AVAudioSettings.h (linear PCM only).  A value of nil indicates that the receiver will return audio samples in a convenient uncompressed format, with sample rate and other properties determined according to the properties of the receiver's audio tracks.
--
-- ObjC selector: @- audioSettings@
audioSettings :: IsAVAssetReaderAudioMixOutput avAssetReaderAudioMixOutput => avAssetReaderAudioMixOutput -> IO (Id NSDictionary)
audioSettings avAssetReaderAudioMixOutput  =
  sendMsg avAssetReaderAudioMixOutput (mkSelector "audioSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

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
audioMix avAssetReaderAudioMixOutput  =
  sendMsg avAssetReaderAudioMixOutput (mkSelector "audioMix") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setAudioMix avAssetReaderAudioMixOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetReaderAudioMixOutput (mkSelector "setAudioMix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
audioTimePitchAlgorithm avAssetReaderAudioMixOutput  =
  sendMsg avAssetReaderAudioMixOutput (mkSelector "audioTimePitchAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setAudioTimePitchAlgorithm avAssetReaderAudioMixOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetReaderAudioMixOutput (mkSelector "setAudioTimePitchAlgorithm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderAudioMixOutputWithAudioTracks:audioSettings:@
assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector :: Selector
assetReaderAudioMixOutputWithAudioTracks_audioSettingsSelector = mkSelector "assetReaderAudioMixOutputWithAudioTracks:audioSettings:"

-- | @Selector@ for @initWithAudioTracks:audioSettings:@
initWithAudioTracks_audioSettingsSelector :: Selector
initWithAudioTracks_audioSettingsSelector = mkSelector "initWithAudioTracks:audioSettings:"

-- | @Selector@ for @audioTracks@
audioTracksSelector :: Selector
audioTracksSelector = mkSelector "audioTracks"

-- | @Selector@ for @audioSettings@
audioSettingsSelector :: Selector
audioSettingsSelector = mkSelector "audioSettings"

-- | @Selector@ for @audioMix@
audioMixSelector :: Selector
audioMixSelector = mkSelector "audioMix"

-- | @Selector@ for @setAudioMix:@
setAudioMixSelector :: Selector
setAudioMixSelector = mkSelector "setAudioMix:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

