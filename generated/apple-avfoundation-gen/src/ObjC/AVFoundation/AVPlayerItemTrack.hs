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
  , enabledSelector
  , setEnabledSelector
  , currentVideoFrameRateSelector
  , videoFieldModeSelector
  , setVideoFieldModeSelector


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

-- | assetTrack
--
-- Indicates the AVAssetTrack for which the AVPlayerItemTrack represents presentation state.
--
-- This property is not observable.	Clients must serialize their access to the resulting AVAssetTrack and related objects on the associated AVPlayer's	notification queue.  By default, this queue is the main queue.
--
-- ObjC selector: @- assetTrack@
assetTrack :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> IO (Id AVAssetTrack)
assetTrack avPlayerItemTrack  =
    sendMsg avPlayerItemTrack (mkSelector "assetTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | enabled
--
-- Indicates whether the track is enabled for presentation during playback.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- enabled@
enabled :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> IO Bool
enabled avPlayerItemTrack  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItemTrack (mkSelector "enabled") retCULong []

-- | enabled
--
-- Indicates whether the track is enabled for presentation during playback.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVPlayerItemTrack avPlayerItemTrack => avPlayerItemTrack -> Bool -> IO ()
setEnabled avPlayerItemTrack  value =
    sendMsg avPlayerItemTrack (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

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
currentVideoFrameRate avPlayerItemTrack  =
    sendMsg avPlayerItemTrack (mkSelector "currentVideoFrameRate") retCFloat []

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
videoFieldMode avPlayerItemTrack  =
    sendMsg avPlayerItemTrack (mkSelector "videoFieldMode") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setVideoFieldMode avPlayerItemTrack  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avPlayerItemTrack (mkSelector "setVideoFieldMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assetTrack@
assetTrackSelector :: Selector
assetTrackSelector = mkSelector "assetTrack"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @currentVideoFrameRate@
currentVideoFrameRateSelector :: Selector
currentVideoFrameRateSelector = mkSelector "currentVideoFrameRate"

-- | @Selector@ for @videoFieldMode@
videoFieldModeSelector :: Selector
videoFieldModeSelector = mkSelector "videoFieldMode"

-- | @Selector@ for @setVideoFieldMode:@
setVideoFieldModeSelector :: Selector
setVideoFieldModeSelector = mkSelector "setVideoFieldMode:"

