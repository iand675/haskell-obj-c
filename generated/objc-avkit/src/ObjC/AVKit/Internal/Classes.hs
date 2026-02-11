{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AVKit.Internal.Classes (
    module ObjC.AVKit.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- AVPictureInPictureController ----------

-- | AVPictureInPictureController
--
-- AVPictureInPictureController is a subclass of NSObject that can be used to present the contents of an AVPlayerLayer or AVPlayerView floating on top of applications.
-- 
-- Phantom type for @AVPictureInPictureController@.
data AVPictureInPictureController

instance IsObjCObject (Id AVPictureInPictureController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPictureInPictureController"

class IsNSObject a => IsAVPictureInPictureController a where
  toAVPictureInPictureController :: a -> Id AVPictureInPictureController

instance IsAVPictureInPictureController (Id AVPictureInPictureController) where
  toAVPictureInPictureController = unsafeCastId

instance IsNSObject (Id AVPictureInPictureController) where
  toNSObject = unsafeCastId

-- ---------- AVPictureInPictureControllerContentSource ----------

-- | AVPictureInPictureControllerContentSource
--
-- A content source for AVPictureInPictureController.
--
-- Create a content source with an appropriate layer, and use it to initialize the AVPictureInPictureController.
-- 
-- Phantom type for @AVPictureInPictureControllerContentSource@.
data AVPictureInPictureControllerContentSource

instance IsObjCObject (Id AVPictureInPictureControllerContentSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPictureInPictureControllerContentSource"

class IsNSObject a => IsAVPictureInPictureControllerContentSource a where
  toAVPictureInPictureControllerContentSource :: a -> Id AVPictureInPictureControllerContentSource

instance IsAVPictureInPictureControllerContentSource (Id AVPictureInPictureControllerContentSource) where
  toAVPictureInPictureControllerContentSource = unsafeCastId

instance IsNSObject (Id AVPictureInPictureControllerContentSource) where
  toNSObject = unsafeCastId

-- ---------- AVPlaybackSpeed ----------

-- | AVPlaybackSpeed
--
-- Class used to define a user selectable playback speed in a playback UI.
-- 
-- Phantom type for @AVPlaybackSpeed@.
data AVPlaybackSpeed

instance IsObjCObject (Id AVPlaybackSpeed) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlaybackSpeed"

class IsNSObject a => IsAVPlaybackSpeed a where
  toAVPlaybackSpeed :: a -> Id AVPlaybackSpeed

instance IsAVPlaybackSpeed (Id AVPlaybackSpeed) where
  toAVPlaybackSpeed = unsafeCastId

instance IsNSObject (Id AVPlaybackSpeed) where
  toNSObject = unsafeCastId

-- ---------- AVCaptureView ----------

-- | AVCaptureView
--
-- AVCaptureView is a subclass of NSView that can be used to display standard user interface controls for capturing media data.
-- 
-- Phantom type for @AVCaptureView@.
data AVCaptureView

instance IsObjCObject (Id AVCaptureView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVCaptureView"

class IsNSView a => IsAVCaptureView a where
  toAVCaptureView :: a -> Id AVCaptureView

instance IsAVCaptureView (Id AVCaptureView) where
  toAVCaptureView = unsafeCastId

instance IsNSObject (Id AVCaptureView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AVCaptureView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id AVCaptureView) where
  toNSView = unsafeCastId

-- ---------- AVPlayerView ----------

-- | AVPlayerView
--
-- AVPlayerView is a subclass of NSView that can be used to display the visual content of an AVPlayer object and the standard playback controls.
-- 
-- Phantom type for @AVPlayerView@.
data AVPlayerView

instance IsObjCObject (Id AVPlayerView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVPlayerView"

class IsNSView a => IsAVPlayerView a where
  toAVPlayerView :: a -> Id AVPlayerView

instance IsAVPlayerView (Id AVPlayerView) where
  toAVPlayerView = unsafeCastId

instance IsNSObject (Id AVPlayerView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AVPlayerView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id AVPlayerView) where
  toNSView = unsafeCastId

-- ---------- AVRoutePickerView ----------

-- | Phantom type for @AVRoutePickerView@.
data AVRoutePickerView

instance IsObjCObject (Id AVRoutePickerView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AVRoutePickerView"

class IsNSView a => IsAVRoutePickerView a where
  toAVRoutePickerView :: a -> Id AVRoutePickerView

instance IsAVRoutePickerView (Id AVRoutePickerView) where
  toAVRoutePickerView = unsafeCastId

instance IsNSObject (Id AVRoutePickerView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AVRoutePickerView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id AVRoutePickerView) where
  toNSView = unsafeCastId
