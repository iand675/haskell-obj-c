{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetReaderTrackOutput
--
-- AVAssetReaderTrackOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading media data from a single AVAssetTrack of an AVAssetReader's AVAsset.
--
-- Clients can read the media data of an asset track by adding an instance of AVAssetReaderTrackOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method. The track's media samples can either be read in the format in which they are stored in the asset, or they can be converted to a different format.
--
-- Generated bindings for @AVAssetReaderTrackOutput@.
module ObjC.AVFoundation.AVAssetReaderTrackOutput
  ( AVAssetReaderTrackOutput
  , IsAVAssetReaderTrackOutput(..)
  , init_
  , new
  , assetReaderTrackOutputWithTrack_outputSettings
  , initWithTrack_outputSettings
  , track
  , outputSettings
  , audioTimePitchAlgorithm
  , setAudioTimePitchAlgorithm
  , assetReaderTrackOutputWithTrack_outputSettingsSelector
  , audioTimePitchAlgorithmSelector
  , initSelector
  , initWithTrack_outputSettingsSelector
  , newSelector
  , outputSettingsSelector
  , setAudioTimePitchAlgorithmSelector
  , trackSelector


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
init_ :: IsAVAssetReaderTrackOutput avAssetReaderTrackOutput => avAssetReaderTrackOutput -> IO (Id AVAssetReaderTrackOutput)
init_ avAssetReaderTrackOutput =
  sendOwnedMessage avAssetReaderTrackOutput initSelector

-- | @+ new@
new :: IO (Id AVAssetReaderTrackOutput)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderTrackOutput"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderTrackOutputWithTrack:outputSettings:
--
-- Returns an instance of AVAssetReaderTrackOutput for reading from the specified track and supplying media data according to the specified output settings.
--
-- @track@ — The AVAssetTrack from which the resulting AVAssetReaderTrackOutput should read sample buffers.
--
-- @outputSettings@ — An NSDictionary of output settings to be used for sample output.  See AVAudioSettings.h for available output settings for audio tracks or AVVideoSettings.h for available output settings for video tracks and also for more information about how to construct an output settings dictionary.
--
-- Returns: An instance of AVAssetReaderTrackOutput.
--
-- The track must be one of the tracks contained by the target AVAssetReader's asset.
--
-- A value of nil for outputSettings configures the output to vend samples in their original format as stored by the specified track.  Initialization will fail if the output settings cannot be used with the specified track.
--
-- AVAssetReaderTrackOutput can only produce uncompressed output.  For audio output settings, this means that AVFormatIDKey must be kAudioFormatLinearPCM.  For video output settings, this means that the dictionary must follow the rules for uncompressed video output, as laid out in AVVideoSettings.h.  AVAssetReaderTrackOutput does not support the AVAudioSettings.h key AVSampleRateConverterAudioQualityKey or the following AVVideoSettings.h keys:
--
-- AVVideoCleanApertureKey		AVVideoPixelAspectRatioKey		AVVideoScalingModeKey
--
-- When constructing video output settings the choice of pixel format will affect the performance and quality of the decompression. For optimal performance when decompressing video the requested pixel format should be one that the decoder supports natively to avoid unnecessary conversions. Below are some recommendations:
--
-- For H.264 use kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange, or kCVPixelFormatType_420YpCbCr8BiPlanarFullRange if the video is known to be full range.  For JPEG on iOS, use kCVPixelFormatType_420YpCbCr8BiPlanarFullRange.
--
-- For other codecs on OSX, kCVPixelFormatType_422YpCbCr8 is the preferred pixel format for video and is generally the most performant when decoding. If you need to work in the RGB domain then kCVPixelFormatType_32BGRA is recommended.
--
-- ProRes encoded media can contain up to 12bits/ch. If your source is ProRes encoded and you wish to preserve more than 8bits/ch during decompression then use one of the following pixel formats: kCVPixelFormatType_4444AYpCbCr16, kCVPixelFormatType_422YpCbCr16, kCVPixelFormatType_422YpCbCr10, or kCVPixelFormatType_64ARGB.  AVAssetReader does not support scaling with any of these high bit depth pixel formats. If you use them then do not specify kCVPixelBufferWidthKey or kCVPixelBufferHeightKey in your outputSettings dictionary. If you plan to append these sample buffers to an AVAssetWriterInput then note that only the ProRes encoders support these pixel formats.
--
-- ProRes 4444 encoded media can contain a mathematically lossless alpha channel. To preserve the alpha channel during decompression use a pixel format with an alpha component such as kCVPixelFormatType_4444AYpCbCr16 or kCVPixelFormatType_64ARGB. To test whether your source contains an alpha channel check that the track's format description has kCMFormatDescriptionExtension_Depth and that its value is 32.
--
-- ObjC selector: @+ assetReaderTrackOutputWithTrack:outputSettings:@
assetReaderTrackOutputWithTrack_outputSettings :: (IsAVAssetTrack track, IsNSDictionary outputSettings) => track -> outputSettings -> IO (Id AVAssetReaderTrackOutput)
assetReaderTrackOutputWithTrack_outputSettings track outputSettings =
  do
    cls' <- getRequiredClass "AVAssetReaderTrackOutput"
    sendClassMessage cls' assetReaderTrackOutputWithTrack_outputSettingsSelector (toAVAssetTrack track) (toNSDictionary outputSettings)

-- | initWithTrack:outputSettings:
--
-- Returns an instance of AVAssetReaderTrackOutput for reading from the specified track and supplying media data according to the specified output settings.
--
-- @track@ — The AVAssetTrack from which the resulting AVAssetReaderTrackOutput should read sample buffers.
--
-- @outputSettings@ — An NSDictionary of output settings to be used for sample output.  See AVAudioSettings.h for available output settings for audio tracks or AVVideoSettings.h for available output settings for video tracks and also for more information about how to construct an output settings dictionary.
--
-- Returns: An instance of AVAssetReaderTrackOutput.
--
-- The track must be one of the tracks contained by the target AVAssetReader's asset.
--
-- A value of nil for outputSettings configures the output to vend samples in their original format as stored by the specified track.  Initialization will fail if the output settings cannot be used with the specified track.
--
-- AVAssetReaderTrackOutput can only produce uncompressed output.  For audio output settings, this means that AVFormatIDKey must be kAudioFormatLinearPCM.  For video output settings, this means that the dictionary must follow the rules for uncompressed video output, as laid out in AVVideoSettings.h.  AVAssetReaderTrackOutput does not support the AVAudioSettings.h key AVSampleRateConverterAudioQualityKey or the following AVVideoSettings.h keys:
--
-- AVVideoCleanApertureKey		AVVideoPixelAspectRatioKey		AVVideoScalingModeKey
--
-- When constructing video output settings the choice of pixel format will affect the performance and quality of the decompression. For optimal performance when decompressing video the requested pixel format should be one that the decoder supports natively to avoid unnecessary conversions. Below are some recommendations:
--
-- For H.264 use kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange, or kCVPixelFormatType_420YpCbCr8BiPlanarFullRange if the video is known to be full range.  For JPEG on iOS, use kCVPixelFormatType_420YpCbCr8BiPlanarFullRange.
--
-- For other codecs on OSX, kCVPixelFormatType_422YpCbCr8 is the preferred pixel format for video and is generally the most performant when decoding. If you need to work in the RGB domain then kCVPixelFormatType_32BGRA is recommended.
--
-- ProRes encoded media can contain up to 12bits/ch. If your source is ProRes encoded and you wish to preserve more than 8bits/ch during decompression then use one of the following pixel formats: kCVPixelFormatType_4444AYpCbCr16, kCVPixelFormatType_422YpCbCr16, kCVPixelFormatType_422YpCbCr10, or kCVPixelFormatType_64ARGB.  AVAssetReader does not support scaling with any of these high bit depth pixel formats. If you use them then do not specify kCVPixelBufferWidthKey or kCVPixelBufferHeightKey in your outputSettings dictionary. If you plan to append these sample buffers to an AVAssetWriterInput then note that only the ProRes encoders support these pixel formats.
--
-- ProRes 4444 encoded media can contain a mathematically lossless alpha channel. To preserve the alpha channel during decompression use a pixel format with an alpha component such as kCVPixelFormatType_4444AYpCbCr16 or kCVPixelFormatType_64ARGB.  To test whether your source contains an alpha channel check that the track's format description has kCMFormatDescriptionExtension_Depth and that its value is 32.
--
-- This method throws an exception for any of the following reasons:		- the output settings dictionary contains an unsupported key mentioned above		- the output settings dictionary does not contain any recognized key		- output settings are not compatible with track's media type		- track output settings would cause the output to yield compressed samples
--
-- ObjC selector: @- initWithTrack:outputSettings:@
initWithTrack_outputSettings :: (IsAVAssetReaderTrackOutput avAssetReaderTrackOutput, IsAVAssetTrack track, IsNSDictionary outputSettings) => avAssetReaderTrackOutput -> track -> outputSettings -> IO (Id AVAssetReaderTrackOutput)
initWithTrack_outputSettings avAssetReaderTrackOutput track outputSettings =
  sendOwnedMessage avAssetReaderTrackOutput initWithTrack_outputSettingsSelector (toAVAssetTrack track) (toNSDictionary outputSettings)

-- | track
--
-- The track from which the receiver reads sample buffers.
--
-- The value of this property is an AVAssetTrack owned by the target AVAssetReader's asset.
--
-- ObjC selector: @- track@
track :: IsAVAssetReaderTrackOutput avAssetReaderTrackOutput => avAssetReaderTrackOutput -> IO (Id AVAssetTrack)
track avAssetReaderTrackOutput =
  sendMessage avAssetReaderTrackOutput trackSelector

-- | outputSettings
--
-- The output settings used by the receiver.
--
-- The value of this property is an NSDictionary that contains values for keys as specified by either AVAudioSettings.h for audio tracks or AVVideoSettings.h for video tracks.  A value of nil indicates that the receiver will vend samples in their original format as stored in the target track.
--
-- ObjC selector: @- outputSettings@
outputSettings :: IsAVAssetReaderTrackOutput avAssetReaderTrackOutput => avAssetReaderTrackOutput -> IO (Id NSDictionary)
outputSettings avAssetReaderTrackOutput =
  sendMessage avAssetReaderTrackOutput outputSettingsSelector

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchAlgorithmSpectral, are defined in AVAudioProcessingSettings.h.  An NSInvalidArgumentException will be raised if this property is set to a value other than the constants defined in that file.
--
-- The default value is AVAudioTimePitchAlgorithmSpectral.
--
-- This property throws an exception for any of the following reasons:		- a value is set value after reading has started		- a value is set other than AVAudioTimePitchAlgorithmSpectral, AVAudioTimePitchAlgorithmTimeDomain, or AVAudioTimePitchAlgorithmVarispeed.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVAssetReaderTrackOutput avAssetReaderTrackOutput => avAssetReaderTrackOutput -> IO (Id NSString)
audioTimePitchAlgorithm avAssetReaderTrackOutput =
  sendMessage avAssetReaderTrackOutput audioTimePitchAlgorithmSelector

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchAlgorithmSpectral, are defined in AVAudioProcessingSettings.h.  An NSInvalidArgumentException will be raised if this property is set to a value other than the constants defined in that file.
--
-- The default value is AVAudioTimePitchAlgorithmSpectral.
--
-- This property throws an exception for any of the following reasons:		- a value is set value after reading has started		- a value is set other than AVAudioTimePitchAlgorithmSpectral, AVAudioTimePitchAlgorithmTimeDomain, or AVAudioTimePitchAlgorithmVarispeed.
--
-- ObjC selector: @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVAssetReaderTrackOutput avAssetReaderTrackOutput, IsNSString value) => avAssetReaderTrackOutput -> value -> IO ()
setAudioTimePitchAlgorithm avAssetReaderTrackOutput value =
  sendMessage avAssetReaderTrackOutput setAudioTimePitchAlgorithmSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReaderTrackOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReaderTrackOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderTrackOutputWithTrack:outputSettings:@
assetReaderTrackOutputWithTrack_outputSettingsSelector :: Selector '[Id AVAssetTrack, Id NSDictionary] (Id AVAssetReaderTrackOutput)
assetReaderTrackOutputWithTrack_outputSettingsSelector = mkSelector "assetReaderTrackOutputWithTrack:outputSettings:"

-- | @Selector@ for @initWithTrack:outputSettings:@
initWithTrack_outputSettingsSelector :: Selector '[Id AVAssetTrack, Id NSDictionary] (Id AVAssetReaderTrackOutput)
initWithTrack_outputSettingsSelector = mkSelector "initWithTrack:outputSettings:"

-- | @Selector@ for @track@
trackSelector :: Selector '[] (Id AVAssetTrack)
trackSelector = mkSelector "track"

-- | @Selector@ for @outputSettings@
outputSettingsSelector :: Selector '[] (Id NSDictionary)
outputSettingsSelector = mkSelector "outputSettings"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector '[] (Id NSString)
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector '[Id NSString] ()
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

