{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ReplayKit.Internal.Classes (
    module ObjC.ReplayKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- RPBroadcastActivityController ----------

-- | RPBroadcastActivityController
--
-- Controller object that allows clients to present the macOS broadcast picker and returns the RPBroadcastController object that controls broadcast functionality.
-- 
-- Phantom type for @RPBroadcastActivityController@.
data RPBroadcastActivityController

instance IsObjCObject (Id RPBroadcastActivityController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "RPBroadcastActivityController"

class IsNSObject a => IsRPBroadcastActivityController a where
  toRPBroadcastActivityController :: a -> Id RPBroadcastActivityController

instance IsRPBroadcastActivityController (Id RPBroadcastActivityController) where
  toRPBroadcastActivityController = unsafeCastId

instance IsNSObject (Id RPBroadcastActivityController) where
  toNSObject = unsafeCastId

-- ---------- RPBroadcastController ----------

-- | RPBroadcastController
--
-- Available once a user has successfully initiated a broadcast using an RPBroadcastActivityViewController. Can be used to start, pause and stop a broadcast.
-- 
-- Phantom type for @RPBroadcastController@.
data RPBroadcastController

instance IsObjCObject (Id RPBroadcastController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "RPBroadcastController"

class IsNSObject a => IsRPBroadcastController a where
  toRPBroadcastController :: a -> Id RPBroadcastController

instance IsRPBroadcastController (Id RPBroadcastController) where
  toRPBroadcastController = unsafeCastId

instance IsNSObject (Id RPBroadcastController) where
  toNSObject = unsafeCastId

-- ---------- RPBroadcastHandler ----------

-- | RPBroadcastProcessExtension
--
-- Base class for extensions that are responsible for handling video and audio data.
-- 
-- Phantom type for @RPBroadcastHandler@.
data RPBroadcastHandler

instance IsObjCObject (Id RPBroadcastHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "RPBroadcastHandler"

class IsNSObject a => IsRPBroadcastHandler a where
  toRPBroadcastHandler :: a -> Id RPBroadcastHandler

instance IsRPBroadcastHandler (Id RPBroadcastHandler) where
  toRPBroadcastHandler = unsafeCastId

instance IsNSObject (Id RPBroadcastHandler) where
  toNSObject = unsafeCastId

-- ---------- RPScreenRecorder ----------

-- | RPScreenRecorder
--
-- Singleton class used to control app recording.
-- 
-- Phantom type for @RPScreenRecorder@.
data RPScreenRecorder

instance IsObjCObject (Id RPScreenRecorder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "RPScreenRecorder"

class IsNSObject a => IsRPScreenRecorder a where
  toRPScreenRecorder :: a -> Id RPScreenRecorder

instance IsRPScreenRecorder (Id RPScreenRecorder) where
  toRPScreenRecorder = unsafeCastId

instance IsNSObject (Id RPScreenRecorder) where
  toNSObject = unsafeCastId

-- ---------- RPBroadcastSampleHandler ----------

-- | RPBroadcastSampleHandler
--
-- Subclass this class to handle CMSampleBuffer objects as they are captured by ReplayKit. To enable this mode of handling, set the RPBroadcastProcessMode in the extension's info.plist to RPBroadcastProcessModeSampleBuffer.
-- 
-- Phantom type for @RPBroadcastSampleHandler@.
data RPBroadcastSampleHandler

instance IsObjCObject (Id RPBroadcastSampleHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "RPBroadcastSampleHandler"

class IsRPBroadcastHandler a => IsRPBroadcastSampleHandler a where
  toRPBroadcastSampleHandler :: a -> Id RPBroadcastSampleHandler

instance IsRPBroadcastSampleHandler (Id RPBroadcastSampleHandler) where
  toRPBroadcastSampleHandler = unsafeCastId

instance IsNSObject (Id RPBroadcastSampleHandler) where
  toNSObject = unsafeCastId

instance IsRPBroadcastHandler (Id RPBroadcastSampleHandler) where
  toRPBroadcastHandler = unsafeCastId

-- ---------- RPPreviewViewController ----------

-- | Phantom type for @RPPreviewViewController@.
data RPPreviewViewController

instance IsObjCObject (Id RPPreviewViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "RPPreviewViewController"

class IsNSViewController a => IsRPPreviewViewController a where
  toRPPreviewViewController :: a -> Id RPPreviewViewController

instance IsRPPreviewViewController (Id RPPreviewViewController) where
  toRPPreviewViewController = unsafeCastId

instance IsNSObject (Id RPPreviewViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id RPPreviewViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id RPPreviewViewController) where
  toNSViewController = unsafeCastId
