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
  , playerLayerWithPlayerSelector
  , copyDisplayedPixelBufferSelector
  , playerSelector
  , setPlayerSelector
  , videoGravitySelector
  , setVideoGravitySelector
  , readyForDisplaySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    withObjCPtr player $ \raw_player ->
      sendClassMsg cls' (mkSelector "playerLayerWithPlayer:") (retPtr retVoid) [argPtr (castPtr raw_player :: Ptr ())] >>= retainedObject . castPtr

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
copyDisplayedPixelBuffer avPlayerLayer  =
  fmap castPtr $ sendMsg avPlayerLayer (mkSelector "copyDisplayedPixelBuffer") (retPtr retVoid) []

-- | player
--
-- Indicates the instance of AVPlayer for which the AVPlayerLayer displays visual output
--
-- ObjC selector: @- player@
player :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO (Id AVPlayer)
player avPlayerLayer  =
  sendMsg avPlayerLayer (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | player
--
-- Indicates the instance of AVPlayer for which the AVPlayerLayer displays visual output
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsAVPlayerLayer avPlayerLayer, IsAVPlayer value) => avPlayerLayer -> value -> IO ()
setPlayer avPlayerLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerLayer (mkSelector "setPlayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | videoGravity
--
-- A string defining how the video is displayed within an AVPlayerLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill  					and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default. 					See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO (Id NSString)
videoGravity avPlayerLayer  =
  sendMsg avPlayerLayer (mkSelector "videoGravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoGravity
--
-- A string defining how the video is displayed within an AVPlayerLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill  					and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default. 					See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVPlayerLayer avPlayerLayer, IsNSString value) => avPlayerLayer -> value -> IO ()
setVideoGravity avPlayerLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerLayer (mkSelector "setVideoGravity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | readyForDisplay
--
-- Boolean indicating that the first video frame has been made ready for display for the current item of the associated AVPlayer.
--
-- Use this property as an indicator of when best to show or animate-in an AVPlayerLayer into view. 					An AVPlayerLayer may be displayed, or made visible, while this property is NO, however the layer will not have any user-visible content until the value becomes YES. Note that if an animation is added to an AVPlayerLayer before it becomes readyForDisplay the video image displayed inside might not animate with the receiver.					This property remains NO for an AVPlayer currentItem whose AVAsset contains no enabled video tracks.					This property is key-value observable.
--
-- ObjC selector: @- readyForDisplay@
readyForDisplay :: IsAVPlayerLayer avPlayerLayer => avPlayerLayer -> IO Bool
readyForDisplay avPlayerLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerLayer (mkSelector "readyForDisplay") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playerLayerWithPlayer:@
playerLayerWithPlayerSelector :: Selector
playerLayerWithPlayerSelector = mkSelector "playerLayerWithPlayer:"

-- | @Selector@ for @copyDisplayedPixelBuffer@
copyDisplayedPixelBufferSelector :: Selector
copyDisplayedPixelBufferSelector = mkSelector "copyDisplayedPixelBuffer"

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector
setVideoGravitySelector = mkSelector "setVideoGravity:"

-- | @Selector@ for @readyForDisplay@
readyForDisplaySelector :: Selector
readyForDisplaySelector = mkSelector "readyForDisplay"

