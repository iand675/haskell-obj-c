{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemTrack
--
-- An AVPlayerItemTrack carries a reference to an AVAssetTrack as well as presentation settings for that track.
--
-- Note that inspection of assets tracks is provided by AVAssetTrack.		This class is intended to represent presentation state for a track of an asset that's played by an AVPlayer and AVPlayerItem.
--
-- Generated bindings for @AVPlayerItemTrack@.
module ObjC.AVFoundation.AVPlayerItemTrack
  ( AVPlayerItemTrack
  , IsAVPlayerItemTrack(..)
  , assetTrack
  , enabled
  , setEnabled
  , currentVideoFrameRate
  , videoFieldMode
  , setVideoFieldMode
  , assetTrackSelector
  , currentVideoFrameRateSelector
  , enabledSelector
  , setEnabledSelector
  , setVideoFieldModeSelector
  , videoFieldModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | assetTrack
--
-- Indicates the AVAssetTrack for which the AVPlayerItemTrack represents presentation state.
--
-- This property is not observable.	Clients must serialize their access to the resulting AVAssetTrack and related objects on the associated AVPlayer's	notification queue.  By default, this queue is the main queue.
--
-- ObjC selector: @- assetTrack@
assetTrack :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> IO (Id AVAssetTrack)
assetTrack avPlayerItemTrack =
  sendMessage avPlayerItemTrack assetTrackSelector

-- | enabled
--
-- Indicates whether the track is enabled for presentation during playback.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- enabled@
enabled :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> IO Bool
enabled avPlayerItemTrack =
  sendMessage avPlayerItemTrack enabledSelector

-- | enabled
--
-- Indicates whether the track is enabled for presentation during playback.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> Bool -> IO ()
setEnabled avPlayerItemTrack value =
  sendMessage avPlayerItemTrack setEnabledSelector value

-- | currentVideoFrameRate
--
-- If the media type of the assetTrack is AVMediaTypeVideo, indicates the current frame rate of the track as it plays, in units of frames per second. If the item is not playing, or if the media type of the track is not video, the value of this property is 0.
--
-- This property is not observable.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- currentVideoFrameRate@
currentVideoFrameRate :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> IO CFloat
currentVideoFrameRate avPlayerItemTrack =
  sendMessage avPlayerItemTrack currentVideoFrameRateSelector

-- | videoFieldMode
--
-- If the media type of the assetTrack is AVMediaTypeVideo, specifies the handling of video frames that contain multiple fields.
--
-- A value of nil indicates default processing of video frames. If you want video fields to be deinterlaced, set videoFieldMode to AVPlayerItemTrackVideoFieldModeDeinterlaceFields. 				You can test whether video being played has multiple fields by examining the underlying AVAssetTrack's format descriptions. See -[AVAssetTrack formatDescriptions] and, for video format descriptions, kCMFormatDescriptionExtension_FieldCount.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- videoFieldMode@
videoFieldMode :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> IO (Id NSString)
videoFieldMode avPlayerItemTrack =
  sendMessage avPlayerItemTrack videoFieldModeSelector

-- | videoFieldMode
--
-- If the media type of the assetTrack is AVMediaTypeVideo, specifies the handling of video frames that contain multiple fields.
--
-- A value of nil indicates default processing of video frames. If you want video fields to be deinterlaced, set videoFieldMode to AVPlayerItemTrackVideoFieldModeDeinterlaceFields. 				You can test whether video being played has multiple fields by examining the underlying AVAssetTrack's format descriptions. See -[AVAssetTrack formatDescriptions] and, for video format descriptions, kCMFormatDescriptionExtension_FieldCount.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setVideoFieldMode:@
setVideoFieldMode :: (IsAVPlayerItemTrack avPlayerItemTrack, IsNSString value) => avPlayerItemTrack -> value -> IO ()
setVideoFieldMode avPlayerItemTrack value =
  sendMessage avPlayerItemTrack setVideoFieldModeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assetTrack@
assetTrackSelector :: Selector '[] (Id AVAssetTrack)
assetTrackSelector = mkSelector "assetTrack"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @currentVideoFrameRate@
currentVideoFrameRateSelector :: Selector '[] CFloat
currentVideoFrameRateSelector = mkSelector "currentVideoFrameRate"

-- | @Selector@ for @videoFieldMode@
videoFieldModeSelector :: Selector '[] (Id NSString)
videoFieldModeSelector = mkSelector "videoFieldMode"

-- | @Selector@ for @setVideoFieldMode:@
setVideoFieldModeSelector :: Selector '[Id NSString] ()
setVideoFieldModeSelector = mkSelector "setVideoFieldMode:"

