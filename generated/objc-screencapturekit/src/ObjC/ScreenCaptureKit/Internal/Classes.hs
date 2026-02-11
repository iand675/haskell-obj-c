{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ScreenCaptureKit.Internal.Classes (
    module ObjC.ScreenCaptureKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SCContentFilter ----------

-- | SCContentFilter
--
-- SCContentFilter is a object that determines the exact content to be captured in the SCStream. It can be filtered through displays, windows, excluded windows or applications.
-- 
-- Phantom type for @SCContentFilter@.
data SCContentFilter

instance IsObjCObject (Id SCContentFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCContentFilter"

class IsNSObject a => IsSCContentFilter a where
  toSCContentFilter :: a -> Id SCContentFilter

instance IsSCContentFilter (Id SCContentFilter) where
  toSCContentFilter = unsafeCastId

instance IsNSObject (Id SCContentFilter) where
  toNSObject = unsafeCastId

-- ---------- SCContentSharingPicker ----------

-- | SCContentSharingPicker
--
-- SCContentSharingPicker is an object created by client applications to opt-in to Control Center's content picker
-- 
-- Phantom type for @SCContentSharingPicker@.
data SCContentSharingPicker

instance IsObjCObject (Id SCContentSharingPicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCContentSharingPicker"

class IsNSObject a => IsSCContentSharingPicker a where
  toSCContentSharingPicker :: a -> Id SCContentSharingPicker

instance IsSCContentSharingPicker (Id SCContentSharingPicker) where
  toSCContentSharingPicker = unsafeCastId

instance IsNSObject (Id SCContentSharingPicker) where
  toNSObject = unsafeCastId

-- ---------- SCContentSharingPickerConfiguration ----------

-- | SCContentSharingPickerConfiguration
--
-- SCContentSharingPickerConfiguration is an object which can optionally be set on the SCContentSharingPicker for customized configuration.
-- 
-- Phantom type for @SCContentSharingPickerConfiguration@.
data SCContentSharingPickerConfiguration

instance IsObjCObject (Id SCContentSharingPickerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCContentSharingPickerConfiguration"

class IsNSObject a => IsSCContentSharingPickerConfiguration a where
  toSCContentSharingPickerConfiguration :: a -> Id SCContentSharingPickerConfiguration

instance IsSCContentSharingPickerConfiguration (Id SCContentSharingPickerConfiguration) where
  toSCContentSharingPickerConfiguration = unsafeCastId

instance IsNSObject (Id SCContentSharingPickerConfiguration) where
  toNSObject = unsafeCastId

-- ---------- SCDisplay ----------

-- | Phantom type for @SCDisplay@.
data SCDisplay

instance IsObjCObject (Id SCDisplay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCDisplay"

class IsNSObject a => IsSCDisplay a where
  toSCDisplay :: a -> Id SCDisplay

instance IsSCDisplay (Id SCDisplay) where
  toSCDisplay = unsafeCastId

instance IsNSObject (Id SCDisplay) where
  toNSObject = unsafeCastId

-- ---------- SCRecordingOutput ----------

-- | Phantom type for @SCRecordingOutput@.
data SCRecordingOutput

instance IsObjCObject (Id SCRecordingOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCRecordingOutput"

class IsNSObject a => IsSCRecordingOutput a where
  toSCRecordingOutput :: a -> Id SCRecordingOutput

instance IsSCRecordingOutput (Id SCRecordingOutput) where
  toSCRecordingOutput = unsafeCastId

instance IsNSObject (Id SCRecordingOutput) where
  toNSObject = unsafeCastId

-- ---------- SCRecordingOutputConfiguration ----------

-- | SCRecordingOutputConfiguration
--
-- SCRecordingOutputConfiguration is an object that encapsulates the configuration for recording.
-- 
-- Phantom type for @SCRecordingOutputConfiguration@.
data SCRecordingOutputConfiguration

instance IsObjCObject (Id SCRecordingOutputConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCRecordingOutputConfiguration"

class IsNSObject a => IsSCRecordingOutputConfiguration a where
  toSCRecordingOutputConfiguration :: a -> Id SCRecordingOutputConfiguration

instance IsSCRecordingOutputConfiguration (Id SCRecordingOutputConfiguration) where
  toSCRecordingOutputConfiguration = unsafeCastId

instance IsNSObject (Id SCRecordingOutputConfiguration) where
  toNSObject = unsafeCastId

-- ---------- SCRunningApplication ----------

-- | Phantom type for @SCRunningApplication@.
data SCRunningApplication

instance IsObjCObject (Id SCRunningApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCRunningApplication"

class IsNSObject a => IsSCRunningApplication a where
  toSCRunningApplication :: a -> Id SCRunningApplication

instance IsSCRunningApplication (Id SCRunningApplication) where
  toSCRunningApplication = unsafeCastId

instance IsNSObject (Id SCRunningApplication) where
  toNSObject = unsafeCastId

-- ---------- SCScreenshotConfiguration ----------

-- | Phantom type for @SCScreenshotConfiguration@.
data SCScreenshotConfiguration

instance IsObjCObject (Id SCScreenshotConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCScreenshotConfiguration"

class IsNSObject a => IsSCScreenshotConfiguration a where
  toSCScreenshotConfiguration :: a -> Id SCScreenshotConfiguration

instance IsSCScreenshotConfiguration (Id SCScreenshotConfiguration) where
  toSCScreenshotConfiguration = unsafeCastId

instance IsNSObject (Id SCScreenshotConfiguration) where
  toNSObject = unsafeCastId

-- ---------- SCScreenshotManager ----------

-- | Phantom type for @SCScreenshotManager@.
data SCScreenshotManager

instance IsObjCObject (Id SCScreenshotManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCScreenshotManager"

class IsNSObject a => IsSCScreenshotManager a where
  toSCScreenshotManager :: a -> Id SCScreenshotManager

instance IsSCScreenshotManager (Id SCScreenshotManager) where
  toSCScreenshotManager = unsafeCastId

instance IsNSObject (Id SCScreenshotManager) where
  toNSObject = unsafeCastId

-- ---------- SCScreenshotOutput ----------

-- | Phantom type for @SCScreenshotOutput@.
data SCScreenshotOutput

instance IsObjCObject (Id SCScreenshotOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCScreenshotOutput"

class IsNSObject a => IsSCScreenshotOutput a where
  toSCScreenshotOutput :: a -> Id SCScreenshotOutput

instance IsSCScreenshotOutput (Id SCScreenshotOutput) where
  toSCScreenshotOutput = unsafeCastId

instance IsNSObject (Id SCScreenshotOutput) where
  toNSObject = unsafeCastId

-- ---------- SCShareableContent ----------

-- | Phantom type for @SCShareableContent@.
data SCShareableContent

instance IsObjCObject (Id SCShareableContent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCShareableContent"

class IsNSObject a => IsSCShareableContent a where
  toSCShareableContent :: a -> Id SCShareableContent

instance IsSCShareableContent (Id SCShareableContent) where
  toSCShareableContent = unsafeCastId

instance IsNSObject (Id SCShareableContent) where
  toNSObject = unsafeCastId

-- ---------- SCShareableContentInfo ----------

-- | SCShareableContentInfo
--
-- SCShareableContentInformation is an object that has information about the content of the stream
-- 
-- Phantom type for @SCShareableContentInfo@.
data SCShareableContentInfo

instance IsObjCObject (Id SCShareableContentInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCShareableContentInfo"

class IsNSObject a => IsSCShareableContentInfo a where
  toSCShareableContentInfo :: a -> Id SCShareableContentInfo

instance IsSCShareableContentInfo (Id SCShareableContentInfo) where
  toSCShareableContentInfo = unsafeCastId

instance IsNSObject (Id SCShareableContentInfo) where
  toNSObject = unsafeCastId

-- ---------- SCStream ----------

-- | Phantom type for @SCStream@.
data SCStream

instance IsObjCObject (Id SCStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCStream"

class IsNSObject a => IsSCStream a where
  toSCStream :: a -> Id SCStream

instance IsSCStream (Id SCStream) where
  toSCStream = unsafeCastId

instance IsNSObject (Id SCStream) where
  toNSObject = unsafeCastId

-- ---------- SCStreamConfiguration ----------

-- | SCStreamConfiguration
--
-- SCStreamConfiguration is an object that encapsulates the SCStream properties such as output width, height, pixelformat and others.
-- 
-- Phantom type for @SCStreamConfiguration@.
data SCStreamConfiguration

instance IsObjCObject (Id SCStreamConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCStreamConfiguration"

class IsNSObject a => IsSCStreamConfiguration a where
  toSCStreamConfiguration :: a -> Id SCStreamConfiguration

instance IsSCStreamConfiguration (Id SCStreamConfiguration) where
  toSCStreamConfiguration = unsafeCastId

instance IsNSObject (Id SCStreamConfiguration) where
  toNSObject = unsafeCastId

-- ---------- SCWindow ----------

-- | Phantom type for @SCWindow@.
data SCWindow

instance IsObjCObject (Id SCWindow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCWindow"

class IsNSObject a => IsSCWindow a where
  toSCWindow :: a -> Id SCWindow

instance IsSCWindow (Id SCWindow) where
  toSCWindow = unsafeCastId

instance IsNSObject (Id SCWindow) where
  toNSObject = unsafeCastId
