{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Quartz.Internal.Classes (
    module ObjC.Quartz.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.CoreImage.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.QuartzCore.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- ---------- IKImageBrowserCell ----------

-- | Phantom type for @IKImageBrowserCell@.
data IKImageBrowserCell

instance IsObjCObject (Id IKImageBrowserCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKImageBrowserCell"

class IsNSObject a => IsIKImageBrowserCell a where
  toIKImageBrowserCell :: a -> Id IKImageBrowserCell

instance IsIKImageBrowserCell (Id IKImageBrowserCell) where
  toIKImageBrowserCell = unsafeCastId

instance IsNSObject (Id IKImageBrowserCell) where
  toNSObject = unsafeCastId

-- ---------- IKSaveOptions ----------

-- | IKSaveOptions
--
-- The IKSaveOptions class initializes, adds, and manages user interface options for saving image data.
-- 
-- Phantom type for @IKSaveOptions@.
data IKSaveOptions

instance IsObjCObject (Id IKSaveOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKSaveOptions"

class IsNSObject a => IsIKSaveOptions a where
  toIKSaveOptions :: a -> Id IKSaveOptions

instance IsIKSaveOptions (Id IKSaveOptions) where
  toIKSaveOptions = unsafeCastId

instance IsNSObject (Id IKSaveOptions) where
  toNSObject = unsafeCastId

-- ---------- IKSlideshow ----------

-- | IKSlideshow
--
-- IKSlideshow handles a slideshow with images, PDFs & more.
-- 
-- Phantom type for @IKSlideshow@.
data IKSlideshow

instance IsObjCObject (Id IKSlideshow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKSlideshow"

class IsNSObject a => IsIKSlideshow a where
  toIKSlideshow :: a -> Id IKSlideshow

instance IsIKSlideshow (Id IKSlideshow) where
  toIKSlideshow = unsafeCastId

instance IsNSObject (Id IKSlideshow) where
  toNSObject = unsafeCastId

-- ---------- QCComposition ----------

-- | Phantom type for @QCComposition@.
data QCComposition

instance IsObjCObject (Id QCComposition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCComposition"

class IsNSObject a => IsQCComposition a where
  toQCComposition :: a -> Id QCComposition

instance IsQCComposition (Id QCComposition) where
  toQCComposition = unsafeCastId

instance IsNSObject (Id QCComposition) where
  toNSObject = unsafeCastId

-- ---------- QCCompositionRepository ----------

-- | Phantom type for @QCCompositionRepository@.
data QCCompositionRepository

instance IsObjCObject (Id QCCompositionRepository) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCCompositionRepository"

class IsNSObject a => IsQCCompositionRepository a where
  toQCCompositionRepository :: a -> Id QCCompositionRepository

instance IsQCCompositionRepository (Id QCCompositionRepository) where
  toQCCompositionRepository = unsafeCastId

instance IsNSObject (Id QCCompositionRepository) where
  toNSObject = unsafeCastId

-- ---------- QCPlugIn ----------

-- | Phantom type for @QCPlugIn@.
data QCPlugIn

instance IsObjCObject (Id QCPlugIn) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCPlugIn"

class IsNSObject a => IsQCPlugIn a where
  toQCPlugIn :: a -> Id QCPlugIn

instance IsQCPlugIn (Id QCPlugIn) where
  toQCPlugIn = unsafeCastId

instance IsNSObject (Id QCPlugIn) where
  toNSObject = unsafeCastId

-- ---------- QCRenderer ----------

-- | Phantom type for @QCRenderer@.
data QCRenderer

instance IsObjCObject (Id QCRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCRenderer"

class IsNSObject a => IsQCRenderer a where
  toQCRenderer :: a -> Id QCRenderer

instance IsQCRenderer (Id QCRenderer) where
  toQCRenderer = unsafeCastId

instance IsNSObject (Id QCRenderer) where
  toNSObject = unsafeCastId

-- ---------- QuartzFilter ----------

-- | Phantom type for @QuartzFilter@.
data QuartzFilter

instance IsObjCObject (Id QuartzFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QuartzFilter"

class IsNSObject a => IsQuartzFilter a where
  toQuartzFilter :: a -> Id QuartzFilter

instance IsQuartzFilter (Id QuartzFilter) where
  toQuartzFilter = unsafeCastId

instance IsNSObject (Id QuartzFilter) where
  toNSObject = unsafeCastId

-- ---------- QuartzFilterManager ----------

-- | Phantom type for @QuartzFilterManager@.
data QuartzFilterManager

instance IsObjCObject (Id QuartzFilterManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QuartzFilterManager"

class IsNSObject a => IsQuartzFilterManager a where
  toQuartzFilterManager :: a -> Id QuartzFilterManager

instance IsQuartzFilterManager (Id QuartzFilterManager) where
  toQuartzFilterManager = unsafeCastId

instance IsNSObject (Id QuartzFilterManager) where
  toNSObject = unsafeCastId

-- ---------- QCPatchController ----------

-- | Phantom type for @QCPatchController@.
data QCPatchController

instance IsObjCObject (Id QCPatchController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCPatchController"

class IsNSController a => IsQCPatchController a where
  toQCPatchController :: a -> Id QCPatchController

instance IsQCPatchController (Id QCPatchController) where
  toQCPatchController = unsafeCastId

instance IsNSController (Id QCPatchController) where
  toNSController = unsafeCastId

instance IsNSObject (Id QCPatchController) where
  toNSObject = unsafeCastId

-- ---------- QCCompositionLayer ----------

-- | Phantom type for @QCCompositionLayer@.
data QCCompositionLayer

instance IsObjCObject (Id QCCompositionLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCCompositionLayer"

class IsCAOpenGLLayer a => IsQCCompositionLayer a where
  toQCCompositionLayer :: a -> Id QCCompositionLayer

instance IsQCCompositionLayer (Id QCCompositionLayer) where
  toQCCompositionLayer = unsafeCastId

instance IsCALayer (Id QCCompositionLayer) where
  toCALayer = unsafeCastId

instance IsCAOpenGLLayer (Id QCCompositionLayer) where
  toCAOpenGLLayer = unsafeCastId

instance IsNSObject (Id QCCompositionLayer) where
  toNSObject = unsafeCastId

-- ---------- IKCameraDeviceView ----------

-- | IKCameraDeviceView
--
-- IKCameraDeviceView displays content of a Image Capture supported camera.
-- 
-- Phantom type for @IKCameraDeviceView@.
data IKCameraDeviceView

instance IsObjCObject (Id IKCameraDeviceView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKCameraDeviceView"

class IsNSView a => IsIKCameraDeviceView a where
  toIKCameraDeviceView :: a -> Id IKCameraDeviceView

instance IsIKCameraDeviceView (Id IKCameraDeviceView) where
  toIKCameraDeviceView = unsafeCastId

instance IsNSObject (Id IKCameraDeviceView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKCameraDeviceView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKCameraDeviceView) where
  toNSView = unsafeCastId

-- ---------- IKDeviceBrowserView ----------

-- | IKDeviceBrowserView
--
-- IKDeviceBrowserView displays Image Capture cameras and scanners.
-- 
-- Phantom type for @IKDeviceBrowserView@.
data IKDeviceBrowserView

instance IsObjCObject (Id IKDeviceBrowserView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKDeviceBrowserView"

class IsNSView a => IsIKDeviceBrowserView a where
  toIKDeviceBrowserView :: a -> Id IKDeviceBrowserView

instance IsIKDeviceBrowserView (Id IKDeviceBrowserView) where
  toIKDeviceBrowserView = unsafeCastId

instance IsNSObject (Id IKDeviceBrowserView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKDeviceBrowserView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKDeviceBrowserView) where
  toNSView = unsafeCastId

-- ---------- IKFilterBrowserView ----------

-- | IKFilterBrowserView
--
-- 2006 Apple Inc. All rights reserved.	 Coming to a Leopard installation near you
--
-- View containing the elements of the IKFilterBrowser
--
-- See discussion in IKFilterBrowserPanel
-- 
-- Phantom type for @IKFilterBrowserView@.
data IKFilterBrowserView

instance IsObjCObject (Id IKFilterBrowserView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKFilterBrowserView"

class IsNSView a => IsIKFilterBrowserView a where
  toIKFilterBrowserView :: a -> Id IKFilterBrowserView

instance IsIKFilterBrowserView (Id IKFilterBrowserView) where
  toIKFilterBrowserView = unsafeCastId

instance IsNSObject (Id IKFilterBrowserView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKFilterBrowserView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKFilterBrowserView) where
  toNSView = unsafeCastId

-- ---------- IKFilterUIView ----------

-- | Phantom type for @IKFilterUIView@.
data IKFilterUIView

instance IsObjCObject (Id IKFilterUIView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKFilterUIView"

class IsNSView a => IsIKFilterUIView a where
  toIKFilterUIView :: a -> Id IKFilterUIView

instance IsIKFilterUIView (Id IKFilterUIView) where
  toIKFilterUIView = unsafeCastId

instance IsNSObject (Id IKFilterUIView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKFilterUIView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKFilterUIView) where
  toNSView = unsafeCastId

-- ---------- IKImageBrowserView ----------

-- | IKImageBrowserView
--
-- An IKImageBrowserView object is a view that display and browse images and movies. It supports scrolling and zooming.
--
-- The IKImageBrowserView is deprecated. Please switch to NSCollectionView.
-- 
-- Phantom type for @IKImageBrowserView@.
data IKImageBrowserView

instance IsObjCObject (Id IKImageBrowserView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKImageBrowserView"

class IsNSView a => IsIKImageBrowserView a where
  toIKImageBrowserView :: a -> Id IKImageBrowserView

instance IsIKImageBrowserView (Id IKImageBrowserView) where
  toIKImageBrowserView = unsafeCastId

instance IsNSObject (Id IKImageBrowserView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKImageBrowserView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKImageBrowserView) where
  toNSView = unsafeCastId

-- ---------- IKImageView ----------

-- | IKImageView
--
-- The IKImageView class provides an efficient way to display images in a view while at the same time supporting a number of image editing operations.
-- 
-- Phantom type for @IKImageView@.
data IKImageView

instance IsObjCObject (Id IKImageView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKImageView"

class IsNSView a => IsIKImageView a where
  toIKImageView :: a -> Id IKImageView

instance IsIKImageView (Id IKImageView) where
  toIKImageView = unsafeCastId

instance IsNSObject (Id IKImageView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKImageView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKImageView) where
  toNSView = unsafeCastId

-- ---------- IKScannerDeviceView ----------

-- | IKScannerDeviceView
--
-- IKScannerDeviceView displays a UI to work with Image Capture supported scanners.
-- 
-- Phantom type for @IKScannerDeviceView@.
data IKScannerDeviceView

instance IsObjCObject (Id IKScannerDeviceView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKScannerDeviceView"

class IsNSView a => IsIKScannerDeviceView a where
  toIKScannerDeviceView :: a -> Id IKScannerDeviceView

instance IsIKScannerDeviceView (Id IKScannerDeviceView) where
  toIKScannerDeviceView = unsafeCastId

instance IsNSObject (Id IKScannerDeviceView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IKScannerDeviceView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IKScannerDeviceView) where
  toNSView = unsafeCastId

-- ---------- QCCompositionParameterView ----------

-- | Phantom type for @QCCompositionParameterView@.
data QCCompositionParameterView

instance IsObjCObject (Id QCCompositionParameterView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCCompositionParameterView"

class IsNSView a => IsQCCompositionParameterView a where
  toQCCompositionParameterView :: a -> Id QCCompositionParameterView

instance IsQCCompositionParameterView (Id QCCompositionParameterView) where
  toQCCompositionParameterView = unsafeCastId

instance IsNSObject (Id QCCompositionParameterView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id QCCompositionParameterView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id QCCompositionParameterView) where
  toNSView = unsafeCastId

-- ---------- QCCompositionPickerView ----------

-- | Phantom type for @QCCompositionPickerView@.
data QCCompositionPickerView

instance IsObjCObject (Id QCCompositionPickerView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCCompositionPickerView"

class IsNSView a => IsQCCompositionPickerView a where
  toQCCompositionPickerView :: a -> Id QCCompositionPickerView

instance IsQCCompositionPickerView (Id QCCompositionPickerView) where
  toQCCompositionPickerView = unsafeCastId

instance IsNSObject (Id QCCompositionPickerView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id QCCompositionPickerView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id QCCompositionPickerView) where
  toNSView = unsafeCastId

-- ---------- QCView ----------

-- | Phantom type for @QCView@.
data QCView

instance IsObjCObject (Id QCView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCView"

class IsNSView a => IsQCView a where
  toQCView :: a -> Id QCView

instance IsQCView (Id QCView) where
  toQCView = unsafeCastId

instance IsNSObject (Id QCView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id QCView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id QCView) where
  toNSView = unsafeCastId

-- ---------- QuartzFilterView ----------

-- | Phantom type for @QuartzFilterView@.
data QuartzFilterView

instance IsObjCObject (Id QuartzFilterView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QuartzFilterView"

class IsNSView a => IsQuartzFilterView a where
  toQuartzFilterView :: a -> Id QuartzFilterView

instance IsQuartzFilterView (Id QuartzFilterView) where
  toQuartzFilterView = unsafeCastId

instance IsNSObject (Id QuartzFilterView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id QuartzFilterView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id QuartzFilterView) where
  toNSView = unsafeCastId

-- ---------- QCPlugInViewController ----------

-- | Phantom type for @QCPlugInViewController@.
data QCPlugInViewController

instance IsObjCObject (Id QCPlugInViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCPlugInViewController"

class IsNSViewController a => IsQCPlugInViewController a where
  toQCPlugInViewController :: a -> Id QCPlugInViewController

instance IsQCPlugInViewController (Id QCPlugInViewController) where
  toQCPlugInViewController = unsafeCastId

instance IsNSObject (Id QCPlugInViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id QCPlugInViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id QCPlugInViewController) where
  toNSViewController = unsafeCastId

-- ---------- IKFilterBrowserPanel ----------

-- | IKFilterBrowserPanel
--
-- The IKFilterBrowserPanel provides the shared IKFilterBrowser with its runtime model.
--
-- See information in the introduction.
-- 
-- Phantom type for @IKFilterBrowserPanel@.
data IKFilterBrowserPanel

instance IsObjCObject (Id IKFilterBrowserPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKFilterBrowserPanel"

class IsNSPanel a => IsIKFilterBrowserPanel a where
  toIKFilterBrowserPanel :: a -> Id IKFilterBrowserPanel

instance IsIKFilterBrowserPanel (Id IKFilterBrowserPanel) where
  toIKFilterBrowserPanel = unsafeCastId

instance IsNSObject (Id IKFilterBrowserPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id IKFilterBrowserPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id IKFilterBrowserPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id IKFilterBrowserPanel) where
  toNSWindow = unsafeCastId

-- ---------- IKImageEditPanel ----------

-- | IKImageEditPanel
--
-- The IKImageEditPanel class provides a panel, that is, a utility window that floats on top of document windows, optimized for image editing.
-- 
-- Phantom type for @IKImageEditPanel@.
data IKImageEditPanel

instance IsObjCObject (Id IKImageEditPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKImageEditPanel"

class IsNSPanel a => IsIKImageEditPanel a where
  toIKImageEditPanel :: a -> Id IKImageEditPanel

instance IsIKImageEditPanel (Id IKImageEditPanel) where
  toIKImageEditPanel = unsafeCastId

instance IsNSObject (Id IKImageEditPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id IKImageEditPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id IKImageEditPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id IKImageEditPanel) where
  toNSWindow = unsafeCastId

-- ---------- IKPictureTaker ----------

-- | IKPictureTaker
--
-- An IKPictureTaker object is a panel that allows users to choose and crop an image. It supports browsing of the file system and includes a recents popup-menu. The IKPictureTaker lets the user to crop a choosen image or to take snapshot from a camera like the built-in iSight.
-- 
-- Phantom type for @IKPictureTaker@.
data IKPictureTaker

instance IsObjCObject (Id IKPictureTaker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IKPictureTaker"

class IsNSPanel a => IsIKPictureTaker a where
  toIKPictureTaker :: a -> Id IKPictureTaker

instance IsIKPictureTaker (Id IKPictureTaker) where
  toIKPictureTaker = unsafeCastId

instance IsNSObject (Id IKPictureTaker) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id IKPictureTaker) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id IKPictureTaker) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id IKPictureTaker) where
  toNSWindow = unsafeCastId

-- ---------- QCCompositionPickerPanel ----------

-- | Phantom type for @QCCompositionPickerPanel@.
data QCCompositionPickerPanel

instance IsObjCObject (Id QCCompositionPickerPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QCCompositionPickerPanel"

class IsNSPanel a => IsQCCompositionPickerPanel a where
  toQCCompositionPickerPanel :: a -> Id QCCompositionPickerPanel

instance IsQCCompositionPickerPanel (Id QCCompositionPickerPanel) where
  toQCCompositionPickerPanel = unsafeCastId

instance IsNSObject (Id QCCompositionPickerPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id QCCompositionPickerPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id QCCompositionPickerPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id QCCompositionPickerPanel) where
  toNSWindow = unsafeCastId
