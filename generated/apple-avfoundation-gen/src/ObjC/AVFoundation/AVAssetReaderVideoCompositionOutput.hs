{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetReaderVideoCompositionOutput
--
-- AVAssetReaderVideoCompositionOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading video frames that have been composited together from the frames in one or more AVAssetTracks of an AVAssetReader's AVAsset.
--
-- Clients can read the video frames composited from one or more asset tracks by adding an instance of AVAssetReaderVideoCompositionOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method.
--
-- Generated bindings for @AVAssetReaderVideoCompositionOutput@.
module ObjC.AVFoundation.AVAssetReaderVideoCompositionOutput
  ( AVAssetReaderVideoCompositionOutput
  , IsAVAssetReaderVideoCompositionOutput(..)
  , init_
  , new
  , assetReaderVideoCompositionOutputWithVideoTracks_videoSettings
  , initWithVideoTracks_videoSettings
  , videoTracks
  , videoSettings
  , videoComposition
  , setVideoComposition
  , customVideoCompositor
  , assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector
  , customVideoCompositorSelector
  , initSelector
  , initWithVideoTracks_videoSettingsSelector
  , newSelector
  , setVideoCompositionSelector
  , videoCompositionSelector
  , videoSettingsSelector
  , videoTracksSelector


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
init_ :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id AVAssetReaderVideoCompositionOutput)
init_ avAssetReaderVideoCompositionOutput =
  sendOwnedMessage avAssetReaderVideoCompositionOutput initSelector

-- | @+ new@
new :: IO (Id AVAssetReaderVideoCompositionOutput)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderVideoCompositionOutput"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:
--
-- Creates an instance of AVAssetReaderVideoCompositionOutput for reading composited video from the specified video tracks and supplying media data according to the specified video settings.
--
-- @tracks@ — An NSArray of AVAssetTrack objects from which the resulting AVAssetReaderVideoCompositionOutput should read video frames for compositing.
--
-- @videoSettings@ — An NSDictionary of video settings to be used for video output.  See AVVideoSettings.h for more information about how to construct a video settings dictionary.
--
-- Returns: An instance of AVAssetReaderVideoCompositionOutput.
--
-- Each track must be one of the tracks owned by the target AVAssetReader's asset and must be of media type AVMediaTypeVideo.
--
-- A value of nil for videoSettings configures the output to return samples in a convenient uncompressed format, with properties determined according to the properties of the specified video tracks.  Initialization will fail if the video settings cannot be used with the specified tracks.
--
-- AVAssetReaderVideoCompositionOutput can only produce uncompressed output.  This means that the video settings dictionary must follow the rules for uncompressed video output, as laid out in AVVideoSettings.h.  In addition, the following keys are not supported:
--
-- AVVideoCleanApertureKey		AVVideoPixelAspectRatioKey		AVVideoScalingModeKey
--
-- ObjC selector: @+ assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:@
assetReaderVideoCompositionOutputWithVideoTracks_videoSettings :: (IsNSArray videoTracks, IsNSDictionary videoSettings) => videoTracks -> videoSettings -> IO (Id AVAssetReaderVideoCompositionOutput)
assetReaderVideoCompositionOutputWithVideoTracks_videoSettings videoTracks videoSettings =
  do
    cls' <- getRequiredClass "AVAssetReaderVideoCompositionOutput"
    sendClassMessage cls' assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector (toNSArray videoTracks) (toNSDictionary videoSettings)

-- | initWithVideoTracks:videoSettings:
--
-- Creates an instance of AVAssetReaderVideoCompositionOutput for reading composited video from the specified video tracks and supplying media data according to the specified video settings.
--
-- @tracks@ — An NSArray of AVAssetTrack objects from which the resulting AVAssetReaderVideoCompositionOutput should read video frames for compositing.
--
-- @videoSettings@ — An NSDictionary of video settings to be used for video output.  See AVVideoSettings.h for more information about how to construct a video settings dictionary.
--
-- Returns: An instance of AVAssetReaderVideoCompositionOutput.
--
-- Each track must be one of the tracks owned by the target AVAssetReader's asset and must be of media type AVMediaTypeVideo.
--
-- A value of nil for videoSettings configures the output to return samples in a convenient uncompressed format, with properties determined according to the properties of the specified video tracks.  Initialization will fail if the video settings cannot be used with the specified tracks.
--
-- AVAssetReaderVideoCompositionOutput can only produce uncompressed output.  This means that the video settings dictionary must follow the rules for uncompressed video output, as laid out in AVVideoSettings.h.
--
-- This method throws an exception for any of the following reasons:		- any video track is not of media type AVMediaTypeVideo		- any video track is not part of this asset reader output's AVAsset		- track output settings would cause the output to yield compressed samples		- video settings does not follow the rules for uncompressed video output (AVVideoSettings.h)		- video settings contains any of the following keys:			- AVVideoCleanApertureKey			- AVVideoPixelAspectRatioKey			- AVVideoScalingModeKey			- AVVideoDecompressionPropertiesKey
--
-- ObjC selector: @- initWithVideoTracks:videoSettings:@
initWithVideoTracks_videoSettings :: (IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput, IsNSArray videoTracks, IsNSDictionary videoSettings) => avAssetReaderVideoCompositionOutput -> videoTracks -> videoSettings -> IO (Id AVAssetReaderVideoCompositionOutput)
initWithVideoTracks_videoSettings avAssetReaderVideoCompositionOutput videoTracks videoSettings =
  sendOwnedMessage avAssetReaderVideoCompositionOutput initWithVideoTracks_videoSettingsSelector (toNSArray videoTracks) (toNSDictionary videoSettings)

-- | videoTracks
--
-- The tracks from which the receiver reads composited video.
--
-- The value of this property is an NSArray of AVAssetTracks owned by the target AVAssetReader's asset.
--
-- ObjC selector: @- videoTracks@
videoTracks :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id NSArray)
videoTracks avAssetReaderVideoCompositionOutput =
  sendMessage avAssetReaderVideoCompositionOutput videoTracksSelector

-- | videoSettings
--
-- The video settings used by the receiver.
--
-- The value of this property is an NSDictionary that contains values for keys as specified by AVVideoSettings.h.  A value of nil indicates that the receiver will return video frames in a convenient uncompressed format, with properties determined according to the properties of the receiver's video tracks.
--
-- ObjC selector: @- videoSettings@
videoSettings :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id NSDictionary)
videoSettings avAssetReaderVideoCompositionOutput =
  sendMessage avAssetReaderVideoCompositionOutput videoSettingsSelector

-- | videoComposition
--
-- The composition of video used by the receiver.
--
-- The value of this property is an AVVideoComposition that can be used to specify the visual arrangement of video frames read from each source track over the timeline of the source asset.
--
-- This property throws an exception if a value is set after reading has started.
--
-- ObjC selector: @- videoComposition@
videoComposition :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id AVVideoComposition)
videoComposition avAssetReaderVideoCompositionOutput =
  sendMessage avAssetReaderVideoCompositionOutput videoCompositionSelector

-- | videoComposition
--
-- The composition of video used by the receiver.
--
-- The value of this property is an AVVideoComposition that can be used to specify the visual arrangement of video frames read from each source track over the timeline of the source asset.
--
-- This property throws an exception if a value is set after reading has started.
--
-- ObjC selector: @- setVideoComposition:@
setVideoComposition :: (IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput, IsAVVideoComposition value) => avAssetReaderVideoCompositionOutput -> value -> IO ()
setVideoComposition avAssetReaderVideoCompositionOutput value =
  sendMessage avAssetReaderVideoCompositionOutput setVideoCompositionSelector (toAVVideoComposition value)

-- | customVideoCompositor
--
-- Indicates the custom video compositor instance used by the receiver.
--
-- This property is nil if there is no video compositor, or if the internal video compositor is in use.
--
-- ObjC selector: @- customVideoCompositor@
customVideoCompositor :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO RawId
customVideoCompositor avAssetReaderVideoCompositionOutput =
  sendMessage avAssetReaderVideoCompositionOutput customVideoCompositorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReaderVideoCompositionOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReaderVideoCompositionOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:@
assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector :: Selector '[Id NSArray, Id NSDictionary] (Id AVAssetReaderVideoCompositionOutput)
assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector = mkSelector "assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:"

-- | @Selector@ for @initWithVideoTracks:videoSettings:@
initWithVideoTracks_videoSettingsSelector :: Selector '[Id NSArray, Id NSDictionary] (Id AVAssetReaderVideoCompositionOutput)
initWithVideoTracks_videoSettingsSelector = mkSelector "initWithVideoTracks:videoSettings:"

-- | @Selector@ for @videoTracks@
videoTracksSelector :: Selector '[] (Id NSArray)
videoTracksSelector = mkSelector "videoTracks"

-- | @Selector@ for @videoSettings@
videoSettingsSelector :: Selector '[] (Id NSDictionary)
videoSettingsSelector = mkSelector "videoSettings"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector '[] (Id AVVideoComposition)
videoCompositionSelector = mkSelector "videoComposition"

-- | @Selector@ for @setVideoComposition:@
setVideoCompositionSelector :: Selector '[Id AVVideoComposition] ()
setVideoCompositionSelector = mkSelector "setVideoComposition:"

-- | @Selector@ for @customVideoCompositor@
customVideoCompositorSelector :: Selector '[] RawId
customVideoCompositorSelector = mkSelector "customVideoCompositor"

