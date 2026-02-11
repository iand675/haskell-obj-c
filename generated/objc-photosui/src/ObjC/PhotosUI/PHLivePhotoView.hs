{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHLivePhotoView@.
module ObjC.PhotosUI.PHLivePhotoView
  ( PHLivePhotoView
  , IsPHLivePhotoView(..)
  , startPlaybackWithStyle
  , stopPlayback
  , stopPlaybackAnimated
  , livePhoto
  , setLivePhoto
  , contentMode
  , setContentMode
  , audioVolume
  , setAudioVolume
  , muted
  , setMuted
  , livePhotoBadgeView
  , startPlaybackWithStyleSelector
  , stopPlaybackSelector
  , stopPlaybackAnimatedSelector
  , livePhotoSelector
  , setLivePhotoSelector
  , contentModeSelector
  , setContentModeSelector
  , audioVolumeSelector
  , setAudioVolumeSelector
  , mutedSelector
  , setMutedSelector
  , livePhotoBadgeViewSelector

  -- * Enum types
  , PHLivePhotoViewContentMode(PHLivePhotoViewContentMode)
  , pattern PHLivePhotoViewContentModeAspectFit
  , pattern PHLivePhotoViewContentModeAspectFill
  , PHLivePhotoViewPlaybackStyle(PHLivePhotoViewPlaybackStyle)
  , pattern PHLivePhotoViewPlaybackStyleUndefined
  , pattern PHLivePhotoViewPlaybackStyleFull
  , pattern PHLivePhotoViewPlaybackStyleHint

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

import ObjC.PhotosUI.Internal.Classes
import ObjC.PhotosUI.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Photos.Internal.Classes

-- | The following methods allow the client to manually trigger playback. If the live photo is changed during playback, it will be immediately interrupted.
--
-- ObjC selector: @- startPlaybackWithStyle:@
startPlaybackWithStyle :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> PHLivePhotoViewPlaybackStyle -> IO ()
startPlaybackWithStyle phLivePhotoView  playbackStyle =
  sendMsg phLivePhotoView (mkSelector "startPlaybackWithStyle:") retVoid [argCLong (coerce playbackStyle)]

-- | @- stopPlayback@
stopPlayback :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO ()
stopPlayback phLivePhotoView  =
  sendMsg phLivePhotoView (mkSelector "stopPlayback") retVoid []

-- | Stops live photo playback. If animated is NO, the photo is immediately displayed.
--
-- ObjC selector: @- stopPlaybackAnimated:@
stopPlaybackAnimated :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> Bool -> IO ()
stopPlaybackAnimated phLivePhotoView  animated =
  sendMsg phLivePhotoView (mkSelector "stopPlaybackAnimated:") retVoid [argCULong (if animated then 1 else 0)]

-- | Live photo displayed in the receiver.
--
-- ObjC selector: @- livePhoto@
livePhoto :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO (Id PHLivePhoto)
livePhoto phLivePhotoView  =
  sendMsg phLivePhotoView (mkSelector "livePhoto") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Live photo displayed in the receiver.
--
-- ObjC selector: @- setLivePhoto:@
setLivePhoto :: (IsPHLivePhotoView phLivePhotoView, IsPHLivePhoto value) => phLivePhotoView -> value -> IO ()
setLivePhoto phLivePhotoView  value =
withObjCPtr value $ \raw_value ->
    sendMsg phLivePhotoView (mkSelector "setLivePhoto:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The mode in which the receiver will display its content. Defaults to PHLivePhotoViewContentModeAspectFit.
--
-- ObjC selector: @- contentMode@
contentMode :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO PHLivePhotoViewContentMode
contentMode phLivePhotoView  =
  fmap (coerce :: CLong -> PHLivePhotoViewContentMode) $ sendMsg phLivePhotoView (mkSelector "contentMode") retCLong []

-- | The mode in which the receiver will display its content. Defaults to PHLivePhotoViewContentModeAspectFit.
--
-- ObjC selector: @- setContentMode:@
setContentMode :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> PHLivePhotoViewContentMode -> IO ()
setContentMode phLivePhotoView  value =
  sendMsg phLivePhotoView (mkSelector "setContentMode:") retVoid [argCLong (coerce value)]

-- | The audio volume during playback
--
-- ObjC selector: @- audioVolume@
audioVolume :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO CFloat
audioVolume phLivePhotoView  =
  sendMsg phLivePhotoView (mkSelector "audioVolume") retCFloat []

-- | The audio volume during playback
--
-- ObjC selector: @- setAudioVolume:@
setAudioVolume :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> CFloat -> IO ()
setAudioVolume phLivePhotoView  value =
  sendMsg phLivePhotoView (mkSelector "setAudioVolume:") retVoid [argCFloat (fromIntegral value)]

-- | Indicates whether the audio of the Live Photo is muted.
--
-- ObjC selector: @- muted@
muted :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO Bool
muted phLivePhotoView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phLivePhotoView (mkSelector "muted") retCULong []

-- | Indicates whether the audio of the Live Photo is muted.
--
-- ObjC selector: @- setMuted:@
setMuted :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> Bool -> IO ()
setMuted phLivePhotoView  value =
  sendMsg phLivePhotoView (mkSelector "setMuted:") retVoid [argCULong (if value then 1 else 0)]

-- | Directly access the livePhotoBadge in cases where it should be added to a different place in the view hierarchy and not the live photo view. This can be useful when the live photo view is added to a scroll view.
--
-- ObjC selector: @- livePhotoBadgeView@
livePhotoBadgeView :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO (Id NSView)
livePhotoBadgeView phLivePhotoView  =
  sendMsg phLivePhotoView (mkSelector "livePhotoBadgeView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startPlaybackWithStyle:@
startPlaybackWithStyleSelector :: Selector
startPlaybackWithStyleSelector = mkSelector "startPlaybackWithStyle:"

-- | @Selector@ for @stopPlayback@
stopPlaybackSelector :: Selector
stopPlaybackSelector = mkSelector "stopPlayback"

-- | @Selector@ for @stopPlaybackAnimated:@
stopPlaybackAnimatedSelector :: Selector
stopPlaybackAnimatedSelector = mkSelector "stopPlaybackAnimated:"

-- | @Selector@ for @livePhoto@
livePhotoSelector :: Selector
livePhotoSelector = mkSelector "livePhoto"

-- | @Selector@ for @setLivePhoto:@
setLivePhotoSelector :: Selector
setLivePhotoSelector = mkSelector "setLivePhoto:"

-- | @Selector@ for @contentMode@
contentModeSelector :: Selector
contentModeSelector = mkSelector "contentMode"

-- | @Selector@ for @setContentMode:@
setContentModeSelector :: Selector
setContentModeSelector = mkSelector "setContentMode:"

-- | @Selector@ for @audioVolume@
audioVolumeSelector :: Selector
audioVolumeSelector = mkSelector "audioVolume"

-- | @Selector@ for @setAudioVolume:@
setAudioVolumeSelector :: Selector
setAudioVolumeSelector = mkSelector "setAudioVolume:"

-- | @Selector@ for @muted@
mutedSelector :: Selector
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector
setMutedSelector = mkSelector "setMuted:"

-- | @Selector@ for @livePhotoBadgeView@
livePhotoBadgeViewSelector :: Selector
livePhotoBadgeViewSelector = mkSelector "livePhotoBadgeView"

