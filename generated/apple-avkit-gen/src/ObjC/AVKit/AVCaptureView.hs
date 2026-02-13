{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureView
--
-- AVCaptureView is a subclass of NSView that can be used to display standard user interface controls for capturing media data.
--
-- Generated bindings for @AVCaptureView@.
module ObjC.AVKit.AVCaptureView
  ( AVCaptureView
  , IsAVCaptureView(..)
  , setSession_showVideoPreview_showAudioPreview
  , session
  , fileOutput
  , delegate
  , setDelegate
  , controlsStyle
  , setControlsStyle
  , videoGravity
  , setVideoGravity
  , controlsStyleSelector
  , delegateSelector
  , fileOutputSelector
  , sessionSelector
  , setControlsStyleSelector
  , setDelegateSelector
  , setSession_showVideoPreview_showAudioPreviewSelector
  , setVideoGravitySelector
  , videoGravitySelector

  -- * Enum types
  , AVCaptureViewControlsStyle(AVCaptureViewControlsStyle)
  , pattern AVCaptureViewControlsStyleInline
  , pattern AVCaptureViewControlsStyleFloating
  , pattern AVCaptureViewControlsStyleInlineDeviceSelection
  , pattern AVCaptureViewControlsStyleDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVKit.Internal.Classes
import ObjC.AVKit.Internal.Enums
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setSession:allowVideoSourceSelection:allowAudioSourceSelection:
--
-- Sets the session represented by this view.
--
-- @session@ — The session to be represented.
--
-- @showVideoPreview@ — Whether or not video preview should be shown. If YES, capture inputs for video media data will be added, removed, or modified depending on device availability and user selection.
--
-- @showAudioPreview@ — Whether or not audio preview should be shown. If YES, capture inputs for audio media data will be added, removed, or modified depending on device availability and user selection.
--
-- The view must either show audio preview or video preview or both. Furthermore, the view may modify the capture session, for example, to access media data for preview or when the user select a new capture source. Only the default session is started and stopped automatically. The provided session must be manually started and stopped.
--
-- ObjC selector: @- setSession:showVideoPreview:showAudioPreview:@
setSession_showVideoPreview_showAudioPreview :: (IsAVCaptureView avCaptureView, IsAVCaptureSession session) => avCaptureView -> session -> Bool -> Bool -> IO ()
setSession_showVideoPreview_showAudioPreview avCaptureView session showVideoPreview showAudioPreview =
  sendMessage avCaptureView setSession_showVideoPreview_showAudioPreviewSelector (toAVCaptureSession session) showVideoPreview showAudioPreview

-- | session
--
-- A capture session represented by this view.
--
-- Modifying the capture session will impact its visual representation in the view. The default value is a session configured for movie file recordings of audio and video media data. Use -setSession:showVideoPreview:showAudioPreview: to change the value of this property.
--
-- ObjC selector: @- session@
session :: IsAVCaptureView avCaptureView => avCaptureView -> IO (Id AVCaptureSession)
session avCaptureView =
  sendMessage avCaptureView sessionSelector

-- | fileOutput
--
-- A capture file output used to record media data.
--
-- The value of this property is the first instance of AVCaptureFileOutput contained in the session's outputs array or nil if no such instance is found. In the latter case the capture view's start recording button will be disabled. However, the controls for choosing input sources may still be enabled.
--
-- ObjC selector: @- fileOutput@
fileOutput :: IsAVCaptureView avCaptureView => avCaptureView -> IO (Id AVCaptureFileOutput)
fileOutput avCaptureView =
  sendMessage avCaptureView fileOutputSelector

-- | delegate
--
-- The capture view's delegate.
--
-- The start recording button will be disabled if the delegate is not set.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCaptureView avCaptureView => avCaptureView -> IO RawId
delegate avCaptureView =
  sendMessage avCaptureView delegateSelector

-- | delegate
--
-- The capture view's delegate.
--
-- The start recording button will be disabled if the delegate is not set.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVCaptureView avCaptureView => avCaptureView -> RawId -> IO ()
setDelegate avCaptureView value =
  sendMessage avCaptureView setDelegateSelector value

-- | controlsStyle
--
-- The style of the capture controls pane associated with the view.
--
-- ObjC selector: @- controlsStyle@
controlsStyle :: IsAVCaptureView avCaptureView => avCaptureView -> IO AVCaptureViewControlsStyle
controlsStyle avCaptureView =
  sendMessage avCaptureView controlsStyleSelector

-- | controlsStyle
--
-- The style of the capture controls pane associated with the view.
--
-- ObjC selector: @- setControlsStyle:@
setControlsStyle :: IsAVCaptureView avCaptureView => avCaptureView -> AVCaptureViewControlsStyle -> IO ()
setControlsStyle avCaptureView value =
  sendMessage avCaptureView setControlsStyleSelector value

-- | videoGravity
--
-- A string defining how the video is displayed within the views bounds rect.
--
-- Options are AVLayerVideoGravityResize, AVLayerVideoGravityResizeAspect and AVLayerVideoGravityResizeAspectFill. AVLayerVideoGravityResizeAspect is default.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVCaptureView avCaptureView => avCaptureView -> IO (Id NSString)
videoGravity avCaptureView =
  sendMessage avCaptureView videoGravitySelector

-- | videoGravity
--
-- A string defining how the video is displayed within the views bounds rect.
--
-- Options are AVLayerVideoGravityResize, AVLayerVideoGravityResizeAspect and AVLayerVideoGravityResizeAspectFill. AVLayerVideoGravityResizeAspect is default.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVCaptureView avCaptureView, IsNSString value) => avCaptureView -> value -> IO ()
setVideoGravity avCaptureView value =
  sendMessage avCaptureView setVideoGravitySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSession:showVideoPreview:showAudioPreview:@
setSession_showVideoPreview_showAudioPreviewSelector :: Selector '[Id AVCaptureSession, Bool, Bool] ()
setSession_showVideoPreview_showAudioPreviewSelector = mkSelector "setSession:showVideoPreview:showAudioPreview:"

-- | @Selector@ for @session@
sessionSelector :: Selector '[] (Id AVCaptureSession)
sessionSelector = mkSelector "session"

-- | @Selector@ for @fileOutput@
fileOutputSelector :: Selector '[] (Id AVCaptureFileOutput)
fileOutputSelector = mkSelector "fileOutput"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @controlsStyle@
controlsStyleSelector :: Selector '[] AVCaptureViewControlsStyle
controlsStyleSelector = mkSelector "controlsStyle"

-- | @Selector@ for @setControlsStyle:@
setControlsStyleSelector :: Selector '[AVCaptureViewControlsStyle] ()
setControlsStyleSelector = mkSelector "setControlsStyle:"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector '[] (Id NSString)
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector '[Id NSString] ()
setVideoGravitySelector = mkSelector "setVideoGravity:"

