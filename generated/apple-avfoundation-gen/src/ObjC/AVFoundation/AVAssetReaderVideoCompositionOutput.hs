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
  , initSelector
  , newSelector
  , assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector
  , initWithVideoTracks_videoSettingsSelector
  , videoTracksSelector
  , videoSettingsSelector
  , videoCompositionSelector
  , setVideoCompositionSelector
  , customVideoCompositorSelector


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
init_ :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id AVAssetReaderVideoCompositionOutput)
init_ avAssetReaderVideoCompositionOutput  =
    sendMsg avAssetReaderVideoCompositionOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetReaderVideoCompositionOutput)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderVideoCompositionOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr videoTracks $ \raw_videoTracks ->
      withObjCPtr videoSettings $ \raw_videoSettings ->
        sendClassMsg cls' (mkSelector "assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:") (retPtr retVoid) [argPtr (castPtr raw_videoTracks :: Ptr ()), argPtr (castPtr raw_videoSettings :: Ptr ())] >>= retainedObject . castPtr

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
initWithVideoTracks_videoSettings avAssetReaderVideoCompositionOutput  videoTracks videoSettings =
  withObjCPtr videoTracks $ \raw_videoTracks ->
    withObjCPtr videoSettings $ \raw_videoSettings ->
        sendMsg avAssetReaderVideoCompositionOutput (mkSelector "initWithVideoTracks:videoSettings:") (retPtr retVoid) [argPtr (castPtr raw_videoTracks :: Ptr ()), argPtr (castPtr raw_videoSettings :: Ptr ())] >>= ownedObject . castPtr

-- | videoTracks
--
-- The tracks from which the receiver reads composited video.
--
-- The value of this property is an NSArray of AVAssetTracks owned by the target AVAssetReader's asset.
--
-- ObjC selector: @- videoTracks@
videoTracks :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id NSArray)
videoTracks avAssetReaderVideoCompositionOutput  =
    sendMsg avAssetReaderVideoCompositionOutput (mkSelector "videoTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoSettings
--
-- The video settings used by the receiver.
--
-- The value of this property is an NSDictionary that contains values for keys as specified by AVVideoSettings.h.  A value of nil indicates that the receiver will return video frames in a convenient uncompressed format, with properties determined according to the properties of the receiver's video tracks.
--
-- ObjC selector: @- videoSettings@
videoSettings :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO (Id NSDictionary)
videoSettings avAssetReaderVideoCompositionOutput  =
    sendMsg avAssetReaderVideoCompositionOutput (mkSelector "videoSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

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
videoComposition avAssetReaderVideoCompositionOutput  =
    sendMsg avAssetReaderVideoCompositionOutput (mkSelector "videoComposition") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setVideoComposition avAssetReaderVideoCompositionOutput  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avAssetReaderVideoCompositionOutput (mkSelector "setVideoComposition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | customVideoCompositor
--
-- Indicates the custom video compositor instance used by the receiver.
--
-- This property is nil if there is no video compositor, or if the internal video compositor is in use.
--
-- ObjC selector: @- customVideoCompositor@
customVideoCompositor :: IsAVAssetReaderVideoCompositionOutput avAssetReaderVideoCompositionOutput => avAssetReaderVideoCompositionOutput -> IO RawId
customVideoCompositor avAssetReaderVideoCompositionOutput  =
    fmap (RawId . castPtr) $ sendMsg avAssetReaderVideoCompositionOutput (mkSelector "customVideoCompositor") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:@
assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector :: Selector
assetReaderVideoCompositionOutputWithVideoTracks_videoSettingsSelector = mkSelector "assetReaderVideoCompositionOutputWithVideoTracks:videoSettings:"

-- | @Selector@ for @initWithVideoTracks:videoSettings:@
initWithVideoTracks_videoSettingsSelector :: Selector
initWithVideoTracks_videoSettingsSelector = mkSelector "initWithVideoTracks:videoSettings:"

-- | @Selector@ for @videoTracks@
videoTracksSelector :: Selector
videoTracksSelector = mkSelector "videoTracks"

-- | @Selector@ for @videoSettings@
videoSettingsSelector :: Selector
videoSettingsSelector = mkSelector "videoSettings"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector
videoCompositionSelector = mkSelector "videoComposition"

-- | @Selector@ for @setVideoComposition:@
setVideoCompositionSelector :: Selector
setVideoCompositionSelector = mkSelector "setVideoComposition:"

-- | @Selector@ for @customVideoCompositor@
customVideoCompositorSelector :: Selector
customVideoCompositorSelector = mkSelector "customVideoCompositor"

