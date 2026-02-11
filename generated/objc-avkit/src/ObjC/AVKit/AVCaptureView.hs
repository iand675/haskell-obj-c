{-# LANGUAGE PatternSynonyms #-}
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
  , controlsStyle
  , setControlsStyle
  , videoGravity
  , setVideoGravity
  , setSession_showVideoPreview_showAudioPreviewSelector
  , sessionSelector
  , fileOutputSelector
  , controlsStyleSelector
  , setControlsStyleSelector
  , videoGravitySelector
  , setVideoGravitySelector

  -- * Enum types
  , AVCaptureViewControlsStyle(AVCaptureViewControlsStyle)
  , pattern AVCaptureViewControlsStyleInline
  , pattern AVCaptureViewControlsStyleFloating
  , pattern AVCaptureViewControlsStyleInlineDeviceSelection
  , pattern AVCaptureViewControlsStyleDefault

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
setSession_showVideoPreview_showAudioPreview avCaptureView  session showVideoPreview showAudioPreview =
withObjCPtr session $ \raw_session ->
    sendMsg avCaptureView (mkSelector "setSession:showVideoPreview:showAudioPreview:") retVoid [argPtr (castPtr raw_session :: Ptr ()), argCULong (if showVideoPreview then 1 else 0), argCULong (if showAudioPreview then 1 else 0)]

-- | session
--
-- A capture session represented by this view.
--
-- Modifying the capture session will impact its visual representation in the view. The default value is a session configured for movie file recordings of audio and video media data. Use -setSession:showVideoPreview:showAudioPreview: to change the value of this property.
--
-- ObjC selector: @- session@
session :: IsAVCaptureView avCaptureView => avCaptureView -> IO (Id AVCaptureSession)
session avCaptureView  =
  sendMsg avCaptureView (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fileOutput
--
-- A capture file output used to record media data.
--
-- The value of this property is the first instance of AVCaptureFileOutput contained in the session's outputs array or nil if no such instance is found. In the latter case the capture view's start recording button will be disabled. However, the controls for choosing input sources may still be enabled.
--
-- ObjC selector: @- fileOutput@
fileOutput :: IsAVCaptureView avCaptureView => avCaptureView -> IO (Id AVCaptureFileOutput)
fileOutput avCaptureView  =
  sendMsg avCaptureView (mkSelector "fileOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | controlsStyle
--
-- The style of the capture controls pane associated with the view.
--
-- ObjC selector: @- controlsStyle@
controlsStyle :: IsAVCaptureView avCaptureView => avCaptureView -> IO AVCaptureViewControlsStyle
controlsStyle avCaptureView  =
  fmap (coerce :: CLong -> AVCaptureViewControlsStyle) $ sendMsg avCaptureView (mkSelector "controlsStyle") retCLong []

-- | controlsStyle
--
-- The style of the capture controls pane associated with the view.
--
-- ObjC selector: @- setControlsStyle:@
setControlsStyle :: IsAVCaptureView avCaptureView => avCaptureView -> AVCaptureViewControlsStyle -> IO ()
setControlsStyle avCaptureView  value =
  sendMsg avCaptureView (mkSelector "setControlsStyle:") retVoid [argCLong (coerce value)]

-- | videoGravity
--
-- A string defining how the video is displayed within the views bounds rect.
--
-- Options are AVLayerVideoGravityResize, AVLayerVideoGravityResizeAspect and AVLayerVideoGravityResizeAspectFill. AVLayerVideoGravityResizeAspect is default.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVCaptureView avCaptureView => avCaptureView -> IO (Id NSString)
videoGravity avCaptureView  =
  sendMsg avCaptureView (mkSelector "videoGravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoGravity
--
-- A string defining how the video is displayed within the views bounds rect.
--
-- Options are AVLayerVideoGravityResize, AVLayerVideoGravityResizeAspect and AVLayerVideoGravityResizeAspectFill. AVLayerVideoGravityResizeAspect is default.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVCaptureView avCaptureView, IsNSString value) => avCaptureView -> value -> IO ()
setVideoGravity avCaptureView  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptureView (mkSelector "setVideoGravity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSession:showVideoPreview:showAudioPreview:@
setSession_showVideoPreview_showAudioPreviewSelector :: Selector
setSession_showVideoPreview_showAudioPreviewSelector = mkSelector "setSession:showVideoPreview:showAudioPreview:"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @fileOutput@
fileOutputSelector :: Selector
fileOutputSelector = mkSelector "fileOutput"

-- | @Selector@ for @controlsStyle@
controlsStyleSelector :: Selector
controlsStyleSelector = mkSelector "controlsStyle"

-- | @Selector@ for @setControlsStyle:@
setControlsStyleSelector :: Selector
setControlsStyleSelector = mkSelector "setControlsStyle:"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector
setVideoGravitySelector = mkSelector "setVideoGravity:"

