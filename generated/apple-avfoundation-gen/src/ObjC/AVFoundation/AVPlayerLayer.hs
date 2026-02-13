{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVPlayerLayer@.
module ObjC.AVFoundation.AVPlayerLayer
  ( AVPlayerLayer
  , IsAVPlayerLayer(..)
  , playerLayerWithPlayer
  , copyDisplayedPixelBuffer
  , player
  , setPlayer
  , videoGravity
  , setVideoGravity
  , readyForDisplay
  , pixelBufferAttributes
  , setPixelBufferAttributes
  , copyDisplayedPixelBufferSelector
  , pixelBufferAttributesSelector
  , playerLayerWithPlayerSelector
  , playerSelector
  , readyForDisplaySelector
  , setPixelBufferAttributesSelector
  , setPlayerSelector
  , setVideoGravitySelector
  , videoGravitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | layerWithPlayer:
--
-- Returns an instance of AVPlayerLayer to display the visual output of the specified AVPlayer.
--
-- Returns: An instance of AVPlayerLayer.
--
-- ObjC selector: @+ playerLayerWithPlayer:@
playerLayerWithPlayer :: IsAVPlayer player => player -> IO (Id AVPlayerLayer)
playerLayerWithPlayer player =
  do
    cls' <- getRequiredClass "AVPlayerLayer"
    sendClassMessage cls' playerLayerWithPlayerSelector (toAVPlayer player)

-- | copyDisplayedPixelBuffer
--
-- Returns a retained reference to the pixel buffer currently displayed in this AVPlayerLayer. This will return NULL if the displayed pixel buffer is protected, no image is currently being displayed, if the current player's rate is non-zero or if the image is unavailable.
--
-- This will only return the current image while the media is paused, otherwise this will return nil. Clients must release the pixel buffer after use.
--
-- Do not write to the returned CVPixelBuffer's attachments or pixel data.
--
-- ObjC selector: @- copyDisplayedPixelBuffer@
copyDisplayedPixelBuffer :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO (Ptr ())
copyDisplayedPixelBuffer avPlayerLayer =
  sendOwnedMessage avPlayerLayer copyDisplayedPixelBufferSelector

-- | player
--
-- Indicates the instance of AVPlayer for which the AVPlayerLayer displays visual output
--
-- ObjC selector: @- player@
player :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO (Id AVPlayer)
player avPlayerLayer =
  sendMessage avPlayerLayer playerSelector

-- | player
--
-- Indicates the instance of AVPlayer for which the AVPlayerLayer displays visual output
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsAVPlayerLayer avPlayerLayer, IsAVPlayer value) => avPlayerLayer -> value -> IO ()
setPlayer avPlayerLayer value =
  sendMessage avPlayerLayer setPlayerSelector (toAVPlayer value)

-- | videoGravity
--
-- A string defining how the video is displayed within an AVPlayerLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill  					and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default. 					See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO (Id NSString)
videoGravity avPlayerLayer =
  sendMessage avPlayerLayer videoGravitySelector

-- | videoGravity
--
-- A string defining how the video is displayed within an AVPlayerLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill  					and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default. 					See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVPlayerLayer avPlayerLayer, IsNSString value) => avPlayerLayer -> value -> IO ()
setVideoGravity avPlayerLayer value =
  sendMessage avPlayerLayer setVideoGravitySelector (toNSString value)

-- | readyForDisplay
--
-- Boolean indicating that the first video frame has been made ready for display for the current item of the associated AVPlayer.
--
-- Use this property as an indicator of when best to show or animate-in an AVPlayerLayer into view. 					An AVPlayerLayer may be displayed, or made visible, while this property is NO, however the layer will not have any user-visible content until the value becomes YES. Note that if an animation is added to an AVPlayerLayer before it becomes readyForDisplay the video image displayed inside might not animate with the receiver.					This property remains NO for an AVPlayer currentItem whose AVAsset contains no enabled video tracks.					This property is key-value observable.
--
-- ObjC selector: @- readyForDisplay@
readyForDisplay :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO Bool
readyForDisplay avPlayerLayer =
  sendMessage avPlayerLayer readyForDisplaySelector

-- | pixelBufferAttributes
--
-- The client requirements for the visual output displayed in AVPlayerLayer during playback.
--
-- Pixel buffer attribute keys are defined in <CoreVideo/CVPixelBuffer.h>					This property is key-value observable.
--
-- ObjC selector: @- pixelBufferAttributes@
pixelBufferAttributes :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO (Id NSDictionary)
pixelBufferAttributes avPlayerLayer =
  sendMessage avPlayerLayer pixelBufferAttributesSelector

-- | pixelBufferAttributes
--
-- The client requirements for the visual output displayed in AVPlayerLayer during playback.
--
-- Pixel buffer attribute keys are defined in <CoreVideo/CVPixelBuffer.h>					This property is key-value observable.
--
-- ObjC selector: @- setPixelBufferAttributes:@
setPixelBufferAttributes :: (IsAVPlayerLayer avPlayerLayer, IsNSDictionary value) => avPlayerLayer -> value -> IO ()
setPixelBufferAttributes avPlayerLayer value =
  sendMessage avPlayerLayer setPixelBufferAttributesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playerLayerWithPlayer:@
playerLayerWithPlayerSelector :: Selector '[Id AVPlayer] (Id AVPlayerLayer)
playerLayerWithPlayerSelector = mkSelector "playerLayerWithPlayer:"

-- | @Selector@ for @copyDisplayedPixelBuffer@
copyDisplayedPixelBufferSelector :: Selector '[] (Ptr ())
copyDisplayedPixelBufferSelector = mkSelector "copyDisplayedPixelBuffer"

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id AVPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector '[Id AVPlayer] ()
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector '[] (Id NSString)
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector '[Id NSString] ()
setVideoGravitySelector = mkSelector "setVideoGravity:"

-- | @Selector@ for @readyForDisplay@
readyForDisplaySelector :: Selector '[] Bool
readyForDisplaySelector = mkSelector "readyForDisplay"

-- | @Selector@ for @pixelBufferAttributes@
pixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
pixelBufferAttributesSelector = mkSelector "pixelBufferAttributes"

-- | @Selector@ for @setPixelBufferAttributes:@
setPixelBufferAttributesSelector :: Selector '[Id NSDictionary] ()
setPixelBufferAttributesSelector = mkSelector "setPixelBufferAttributes:"

