{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , livePhoto
  , setLivePhoto
  , contentMode
  , setContentMode
  , audioVolume
  , setAudioVolume
  , muted
  , setMuted
  , livePhotoBadgeView
  , audioVolumeSelector
  , contentModeSelector
  , delegateSelector
  , livePhotoBadgeViewSelector
  , livePhotoSelector
  , mutedSelector
  , setAudioVolumeSelector
  , setContentModeSelector
  , setDelegateSelector
  , setLivePhotoSelector
  , setMutedSelector
  , startPlaybackWithStyleSelector
  , stopPlaybackAnimatedSelector
  , stopPlaybackSelector

  -- * Enum types
  , PHLivePhotoViewContentMode(PHLivePhotoViewContentMode)
  , pattern PHLivePhotoViewContentModeAspectFit
  , pattern PHLivePhotoViewContentModeAspectFill
  , PHLivePhotoViewPlaybackStyle(PHLivePhotoViewPlaybackStyle)
  , pattern PHLivePhotoViewPlaybackStyleUndefined
  , pattern PHLivePhotoViewPlaybackStyleFull
  , pattern PHLivePhotoViewPlaybackStyleHint

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
startPlaybackWithStyle phLivePhotoView playbackStyle =
  sendMessage phLivePhotoView startPlaybackWithStyleSelector playbackStyle

-- | @- stopPlayback@
stopPlayback :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO ()
stopPlayback phLivePhotoView =
  sendMessage phLivePhotoView stopPlaybackSelector

-- | Stops live photo playback. If animated is NO, the photo is immediately displayed.
--
-- ObjC selector: @- stopPlaybackAnimated:@
stopPlaybackAnimated :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> Bool -> IO ()
stopPlaybackAnimated phLivePhotoView animated =
  sendMessage phLivePhotoView stopPlaybackAnimatedSelector animated

-- | @- delegate@
delegate :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO RawId
delegate phLivePhotoView =
  sendMessage phLivePhotoView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> RawId -> IO ()
setDelegate phLivePhotoView value =
  sendMessage phLivePhotoView setDelegateSelector value

-- | Live photo displayed in the receiver.
--
-- ObjC selector: @- livePhoto@
livePhoto :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO (Id PHLivePhoto)
livePhoto phLivePhotoView =
  sendMessage phLivePhotoView livePhotoSelector

-- | Live photo displayed in the receiver.
--
-- ObjC selector: @- setLivePhoto:@
setLivePhoto :: (IsPHLivePhotoView phLivePhotoView, IsPHLivePhoto value) => phLivePhotoView -> value -> IO ()
setLivePhoto phLivePhotoView value =
  sendMessage phLivePhotoView setLivePhotoSelector (toPHLivePhoto value)

-- | The mode in which the receiver will display its content. Defaults to PHLivePhotoViewContentModeAspectFit.
--
-- ObjC selector: @- contentMode@
contentMode :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO PHLivePhotoViewContentMode
contentMode phLivePhotoView =
  sendMessage phLivePhotoView contentModeSelector

-- | The mode in which the receiver will display its content. Defaults to PHLivePhotoViewContentModeAspectFit.
--
-- ObjC selector: @- setContentMode:@
setContentMode :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> PHLivePhotoViewContentMode -> IO ()
setContentMode phLivePhotoView value =
  sendMessage phLivePhotoView setContentModeSelector value

-- | The audio volume during playback
--
-- ObjC selector: @- audioVolume@
audioVolume :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO CFloat
audioVolume phLivePhotoView =
  sendMessage phLivePhotoView audioVolumeSelector

-- | The audio volume during playback
--
-- ObjC selector: @- setAudioVolume:@
setAudioVolume :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> CFloat -> IO ()
setAudioVolume phLivePhotoView value =
  sendMessage phLivePhotoView setAudioVolumeSelector value

-- | Indicates whether the audio of the Live Photo is muted.
--
-- ObjC selector: @- muted@
muted :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO Bool
muted phLivePhotoView =
  sendMessage phLivePhotoView mutedSelector

-- | Indicates whether the audio of the Live Photo is muted.
--
-- ObjC selector: @- setMuted:@
setMuted :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> Bool -> IO ()
setMuted phLivePhotoView value =
  sendMessage phLivePhotoView setMutedSelector value

-- | Directly access the livePhotoBadge in cases where it should be added to a different place in the view hierarchy and not the live photo view. This can be useful when the live photo view is added to a scroll view.
--
-- ObjC selector: @- livePhotoBadgeView@
livePhotoBadgeView :: IsPHLivePhotoView phLivePhotoView => phLivePhotoView -> IO (Id NSView)
livePhotoBadgeView phLivePhotoView =
  sendMessage phLivePhotoView livePhotoBadgeViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startPlaybackWithStyle:@
startPlaybackWithStyleSelector :: Selector '[PHLivePhotoViewPlaybackStyle] ()
startPlaybackWithStyleSelector = mkSelector "startPlaybackWithStyle:"

-- | @Selector@ for @stopPlayback@
stopPlaybackSelector :: Selector '[] ()
stopPlaybackSelector = mkSelector "stopPlayback"

-- | @Selector@ for @stopPlaybackAnimated:@
stopPlaybackAnimatedSelector :: Selector '[Bool] ()
stopPlaybackAnimatedSelector = mkSelector "stopPlaybackAnimated:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @livePhoto@
livePhotoSelector :: Selector '[] (Id PHLivePhoto)
livePhotoSelector = mkSelector "livePhoto"

-- | @Selector@ for @setLivePhoto:@
setLivePhotoSelector :: Selector '[Id PHLivePhoto] ()
setLivePhotoSelector = mkSelector "setLivePhoto:"

-- | @Selector@ for @contentMode@
contentModeSelector :: Selector '[] PHLivePhotoViewContentMode
contentModeSelector = mkSelector "contentMode"

-- | @Selector@ for @setContentMode:@
setContentModeSelector :: Selector '[PHLivePhotoViewContentMode] ()
setContentModeSelector = mkSelector "setContentMode:"

-- | @Selector@ for @audioVolume@
audioVolumeSelector :: Selector '[] CFloat
audioVolumeSelector = mkSelector "audioVolume"

-- | @Selector@ for @setAudioVolume:@
setAudioVolumeSelector :: Selector '[CFloat] ()
setAudioVolumeSelector = mkSelector "setAudioVolume:"

-- | @Selector@ for @muted@
mutedSelector :: Selector '[] Bool
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector '[Bool] ()
setMutedSelector = mkSelector "setMuted:"

-- | @Selector@ for @livePhotoBadgeView@
livePhotoBadgeViewSelector :: Selector '[] (Id NSView)
livePhotoBadgeViewSelector = mkSelector "livePhotoBadgeView"

