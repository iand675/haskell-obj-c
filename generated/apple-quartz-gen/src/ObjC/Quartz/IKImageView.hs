{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKImageView
--
-- The IKImageView class provides an efficient way to display images in a view while at the same time supporting a number of image editing operations.
--
-- Generated bindings for @IKImageView@.
module ObjC.Quartz.IKImageView
  ( IKImageView
  , IsIKImageView(..)
  , setImage_imageProperties
  , setImageWithURL
  , image
  , imageSize
  , imageProperties
  , setRotationAngle_centerPoint
  , rotateImageLeft
  , rotateImageRight
  , setImageZoomFactor_centerPoint
  , zoomImageToRect
  , zoomImageToFit
  , zoomImageToActualSize
  , zoomIn
  , zoomOut
  , flipImageHorizontal
  , flipImageVertical
  , crop
  , setOverlay_forType
  , overlayForType
  , scrollToPoint
  , scrollToRect
  , convertViewPointToImagePoint
  , convertViewRectToImageRect
  , convertImagePointToViewPoint
  , convertImageRectToViewRect
  , delegate
  , setDelegate
  , zoomFactor
  , setZoomFactor
  , rotationAngle
  , setRotationAngle
  , currentToolMode
  , setCurrentToolMode
  , autoresizes
  , setAutoresizes
  , hasHorizontalScroller
  , setHasHorizontalScroller
  , hasVerticalScroller
  , setHasVerticalScroller
  , autohidesScrollers
  , setAutohidesScrollers
  , supportsDragAndDrop
  , setSupportsDragAndDrop
  , editable
  , setEditable
  , doubleClickOpensImageEditPanel
  , setDoubleClickOpensImageEditPanel
  , imageCorrection
  , setImageCorrection
  , backgroundColor
  , setBackgroundColor
  , autohidesScrollersSelector
  , autoresizesSelector
  , backgroundColorSelector
  , convertImagePointToViewPointSelector
  , convertImageRectToViewRectSelector
  , convertViewPointToImagePointSelector
  , convertViewRectToImageRectSelector
  , cropSelector
  , currentToolModeSelector
  , delegateSelector
  , doubleClickOpensImageEditPanelSelector
  , editableSelector
  , flipImageHorizontalSelector
  , flipImageVerticalSelector
  , hasHorizontalScrollerSelector
  , hasVerticalScrollerSelector
  , imageCorrectionSelector
  , imagePropertiesSelector
  , imageSelector
  , imageSizeSelector
  , overlayForTypeSelector
  , rotateImageLeftSelector
  , rotateImageRightSelector
  , rotationAngleSelector
  , scrollToPointSelector
  , scrollToRectSelector
  , setAutohidesScrollersSelector
  , setAutoresizesSelector
  , setBackgroundColorSelector
  , setCurrentToolModeSelector
  , setDelegateSelector
  , setDoubleClickOpensImageEditPanelSelector
  , setEditableSelector
  , setHasHorizontalScrollerSelector
  , setHasVerticalScrollerSelector
  , setImageCorrectionSelector
  , setImageWithURLSelector
  , setImageZoomFactor_centerPointSelector
  , setImage_imagePropertiesSelector
  , setOverlay_forTypeSelector
  , setRotationAngleSelector
  , setRotationAngle_centerPointSelector
  , setSupportsDragAndDropSelector
  , setZoomFactorSelector
  , supportsDragAndDropSelector
  , zoomFactorSelector
  , zoomImageToActualSizeSelector
  , zoomImageToFitSelector
  , zoomImageToRectSelector
  , zoomInSelector
  , zoomOutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | setImage:imageProperties:
--
-- Sets the image & metadata (both retrieved from ImageIO).
--
-- ObjC selector: @- setImage:imageProperties:@
setImage_imageProperties :: (IsIKImageView ikImageView, IsNSDictionary metaData) => ikImageView -> Ptr () -> metaData -> IO ()
setImage_imageProperties ikImageView image metaData =
  sendMessage ikImageView setImage_imagePropertiesSelector image (toNSDictionary metaData)

-- | setImageWithURL:
--
-- Initializes an image view with the image specified by a URL.
--
-- ObjC selector: @- setImageWithURL:@
setImageWithURL :: (IsIKImageView ikImageView, IsNSURL url) => ikImageView -> url -> IO ()
setImageWithURL ikImageView url =
  sendMessage ikImageView setImageWithURLSelector (toNSURL url)

-- | image
--
-- Returns the image associated with the view, after any image corrections.
--
-- ObjC selector: @- image@
image :: IsIKImageView ikImageView => ikImageView -> IO (Ptr ())
image ikImageView =
  sendMessage ikImageView imageSelector

-- | imageSize
--
-- Returns the size of the image in the image view.
--
-- ObjC selector: @- imageSize@
imageSize :: IsIKImageView ikImageView => ikImageView -> IO NSSize
imageSize ikImageView =
  sendMessage ikImageView imageSizeSelector

-- | imageProperties
--
-- Returns the metadata for the image in the view.
--
-- ObjC selector: @- imageProperties@
imageProperties :: IsIKImageView ikImageView => ikImageView -> IO (Id NSDictionary)
imageProperties ikImageView =
  sendMessage ikImageView imagePropertiesSelector

-- | setRotationAngle:centerPoint:
--
-- Sets the rotation angle at the provided origin.
--
-- ObjC selector: @- setRotationAngle:centerPoint:@
setRotationAngle_centerPoint :: IsIKImageView ikImageView => ikImageView -> CDouble -> NSPoint -> IO ()
setRotationAngle_centerPoint ikImageView rotationAngle centerPoint =
  sendMessage ikImageView setRotationAngle_centerPointSelector rotationAngle centerPoint

-- | rotateImageLeft:
--
-- Rotates the image left.
--
-- ObjC selector: @- rotateImageLeft:@
rotateImageLeft :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
rotateImageLeft ikImageView sender =
  sendMessage ikImageView rotateImageLeftSelector sender

-- | rotateImageRight:
--
-- Rotates the image right.
--
-- ObjC selector: @- rotateImageRight:@
rotateImageRight :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
rotateImageRight ikImageView sender =
  sendMessage ikImageView rotateImageRightSelector sender

-- | setImageZoomFactor:centerPoint:
--
-- Sets the zoom factor at the provided origin.
--
-- ObjC selector: @- setImageZoomFactor:centerPoint:@
setImageZoomFactor_centerPoint :: IsIKImageView ikImageView => ikImageView -> CDouble -> NSPoint -> IO ()
setImageZoomFactor_centerPoint ikImageView zoomFactor centerPoint =
  sendMessage ikImageView setImageZoomFactor_centerPointSelector zoomFactor centerPoint

-- | zoomImageToRect:
--
-- Zooms the image so that it fits in the specified rectangle.
--
-- ObjC selector: @- zoomImageToRect:@
zoomImageToRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO ()
zoomImageToRect ikImageView rect =
  sendMessage ikImageView zoomImageToRectSelector rect

-- | zoomImageToFit:
--
-- Zooms the image so that it fits in the image view.
--
-- ObjC selector: @- zoomImageToFit:@
zoomImageToFit :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomImageToFit ikImageView sender =
  sendMessage ikImageView zoomImageToFitSelector sender

-- | zoomImageToActualSize:
--
-- Zooms the image so that it is displayed using its true size.
--
-- ObjC selector: @- zoomImageToActualSize:@
zoomImageToActualSize :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomImageToActualSize ikImageView sender =
  sendMessage ikImageView zoomImageToActualSizeSelector sender

-- | zoomIn:
--
-- Zooms the image in.
--
-- ObjC selector: @- zoomIn:@
zoomIn :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomIn ikImageView sender =
  sendMessage ikImageView zoomInSelector sender

-- | zoomOut:
--
-- Zooms the image out.
--
-- ObjC selector: @- zoomOut:@
zoomOut :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomOut ikImageView sender =
  sendMessage ikImageView zoomOutSelector sender

-- | flipImageHorizontal:
--
-- Flips an image along the horizontal axis.
--
-- ObjC selector: @- flipImageHorizontal:@
flipImageHorizontal :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
flipImageHorizontal ikImageView sender =
  sendMessage ikImageView flipImageHorizontalSelector sender

-- | flipImageVertical:
--
-- Flips an image along the vertical axis.
--
-- ObjC selector: @- flipImageVertical:@
flipImageVertical :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
flipImageVertical ikImageView sender =
  sendMessage ikImageView flipImageVerticalSelector sender

-- | crop:
--
-- Crops the image using the current selection.
--
-- ObjC selector: @- crop:@
crop :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
crop ikImageView sender =
  sendMessage ikImageView cropSelector sender

-- | setOverlay:forType:
--
-- Sets an overlay (Core Animation layer) for the image or the image background.
--
-- ObjC selector: @- setOverlay:forType:@
setOverlay_forType :: (IsIKImageView ikImageView, IsCALayer layer, IsNSString layerType) => ikImageView -> layer -> layerType -> IO ()
setOverlay_forType ikImageView layer layerType =
  sendMessage ikImageView setOverlay_forTypeSelector (toCALayer layer) (toNSString layerType)

-- | overlayForType:
--
-- Returns the overlay (Core Animation layer) for the image or the image background.
--
-- ObjC selector: @- overlayForType:@
overlayForType :: (IsIKImageView ikImageView, IsNSString layerType) => ikImageView -> layerType -> IO (Id CALayer)
overlayForType ikImageView layerType =
  sendMessage ikImageView overlayForTypeSelector (toNSString layerType)

-- | scrollToPoint:
--
-- Scrolls the view to the specified point.
--
-- ObjC selector: @- scrollToPoint:@
scrollToPoint :: IsIKImageView ikImageView => ikImageView -> NSPoint -> IO ()
scrollToPoint ikImageView point =
  sendMessage ikImageView scrollToPointSelector point

-- | scrollToRect:
--
-- Scrolls the view so that it includes the provided rectangular area.
--
-- ObjC selector: @- scrollToRect:@
scrollToRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO ()
scrollToRect ikImageView rect =
  sendMessage ikImageView scrollToRectSelector rect

-- | convertViewPointToImagePoint:
--
-- Converts an image view coordinate to an image coordinate.
--
-- ObjC selector: @- convertViewPointToImagePoint:@
convertViewPointToImagePoint :: IsIKImageView ikImageView => ikImageView -> NSPoint -> IO NSPoint
convertViewPointToImagePoint ikImageView viewPoint =
  sendMessage ikImageView convertViewPointToImagePointSelector viewPoint

-- | convertViewRectToImageRect:
--
-- Converts an image view rectangle to an image rectangle.
--
-- ObjC selector: @- convertViewRectToImageRect:@
convertViewRectToImageRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO NSRect
convertViewRectToImageRect ikImageView viewRect =
  sendMessage ikImageView convertViewRectToImageRectSelector viewRect

-- | convertImagePointToViewPoint:
--
-- Converts an image coordinate to an image view coordinate.
--
-- ObjC selector: @- convertImagePointToViewPoint:@
convertImagePointToViewPoint :: IsIKImageView ikImageView => ikImageView -> NSPoint -> IO NSPoint
convertImagePointToViewPoint ikImageView imagePoint =
  sendMessage ikImageView convertImagePointToViewPointSelector imagePoint

-- | convertImageRectToViewRect:
--
-- Converts an image rectangle to an image view rectangle.
--
-- ObjC selector: @- convertImageRectToViewRect:@
convertImageRectToViewRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO NSRect
convertImageRectToViewRect ikImageView imageRect =
  sendMessage ikImageView convertImageRectToViewRectSelector imageRect

-- | delegate
--
-- Specifies the delegate object of the receiver.
--
-- ObjC selector: @- delegate@
delegate :: IsIKImageView ikImageView => ikImageView -> IO RawId
delegate ikImageView =
  sendMessage ikImageView delegateSelector

-- | delegate
--
-- Specifies the delegate object of the receiver.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
setDelegate ikImageView value =
  sendMessage ikImageView setDelegateSelector value

-- | zoomFactor
--
-- Specifies the zoom factor for the image view.
--
-- ObjC selector: @- zoomFactor@
zoomFactor :: IsIKImageView ikImageView => ikImageView -> IO CDouble
zoomFactor ikImageView =
  sendMessage ikImageView zoomFactorSelector

-- | zoomFactor
--
-- Specifies the zoom factor for the image view.
--
-- ObjC selector: @- setZoomFactor:@
setZoomFactor :: IsIKImageView ikImageView => ikImageView -> CDouble -> IO ()
setZoomFactor ikImageView value =
  sendMessage ikImageView setZoomFactorSelector value

-- | rotationAngle
--
-- Specifies the rotation angle for the image view.
--
-- ObjC selector: @- rotationAngle@
rotationAngle :: IsIKImageView ikImageView => ikImageView -> IO CDouble
rotationAngle ikImageView =
  sendMessage ikImageView rotationAngleSelector

-- | rotationAngle
--
-- Specifies the rotation angle for the image view.
--
-- ObjC selector: @- setRotationAngle:@
setRotationAngle :: IsIKImageView ikImageView => ikImageView -> CDouble -> IO ()
setRotationAngle ikImageView value =
  sendMessage ikImageView setRotationAngleSelector value

-- | currentToolMode
--
-- Specifies the current tool mode for the image view.
--
-- ObjC selector: @- currentToolMode@
currentToolMode :: IsIKImageView ikImageView => ikImageView -> IO (Id NSString)
currentToolMode ikImageView =
  sendMessage ikImageView currentToolModeSelector

-- | currentToolMode
--
-- Specifies the current tool mode for the image view.
--
-- ObjC selector: @- setCurrentToolMode:@
setCurrentToolMode :: (IsIKImageView ikImageView, IsNSString value) => ikImageView -> value -> IO ()
setCurrentToolMode ikImageView value =
  sendMessage ikImageView setCurrentToolModeSelector (toNSString value)

-- | autoresizes
--
-- Specifies the automatic resizing state for the image view.
--
-- ObjC selector: @- autoresizes@
autoresizes :: IsIKImageView ikImageView => ikImageView -> IO Bool
autoresizes ikImageView =
  sendMessage ikImageView autoresizesSelector

-- | autoresizes
--
-- Specifies the automatic resizing state for the image view.
--
-- ObjC selector: @- setAutoresizes:@
setAutoresizes :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setAutoresizes ikImageView value =
  sendMessage ikImageView setAutoresizesSelector value

-- | hasHorizontalScroller
--
-- Specifies the horizontal scroll bar state for the image view.
--
-- ObjC selector: @- hasHorizontalScroller@
hasHorizontalScroller :: IsIKImageView ikImageView => ikImageView -> IO Bool
hasHorizontalScroller ikImageView =
  sendMessage ikImageView hasHorizontalScrollerSelector

-- | hasHorizontalScroller
--
-- Specifies the horizontal scroll bar state for the image view.
--
-- ObjC selector: @- setHasHorizontalScroller:@
setHasHorizontalScroller :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setHasHorizontalScroller ikImageView value =
  sendMessage ikImageView setHasHorizontalScrollerSelector value

-- | hasVerticalScroller
--
-- Specifies the vertical scroll bar state for the image view.
--
-- ObjC selector: @- hasVerticalScroller@
hasVerticalScroller :: IsIKImageView ikImageView => ikImageView -> IO Bool
hasVerticalScroller ikImageView =
  sendMessage ikImageView hasVerticalScrollerSelector

-- | hasVerticalScroller
--
-- Specifies the vertical scroll bar state for the image view.
--
-- ObjC selector: @- setHasVerticalScroller:@
setHasVerticalScroller :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setHasVerticalScroller ikImageView value =
  sendMessage ikImageView setHasVerticalScrollerSelector value

-- | autohidesScrollers
--
-- Specifies the automatic-hiding scroll bar state for the image view.
--
-- ObjC selector: @- autohidesScrollers@
autohidesScrollers :: IsIKImageView ikImageView => ikImageView -> IO Bool
autohidesScrollers ikImageView =
  sendMessage ikImageView autohidesScrollersSelector

-- | autohidesScrollers
--
-- Specifies the automatic-hiding scroll bar state for the image view.
--
-- ObjC selector: @- setAutohidesScrollers:@
setAutohidesScrollers :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setAutohidesScrollers ikImageView value =
  sendMessage ikImageView setAutohidesScrollersSelector value

-- | supportsDragAndDrop
--
-- Specifies the drag-and-drop support state for the image view.
--
-- ObjC selector: @- supportsDragAndDrop@
supportsDragAndDrop :: IsIKImageView ikImageView => ikImageView -> IO Bool
supportsDragAndDrop ikImageView =
  sendMessage ikImageView supportsDragAndDropSelector

-- | supportsDragAndDrop
--
-- Specifies the drag-and-drop support state for the image view.
--
-- ObjC selector: @- setSupportsDragAndDrop:@
setSupportsDragAndDrop :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setSupportsDragAndDrop ikImageView value =
  sendMessage ikImageView setSupportsDragAndDropSelector value

-- | editable
--
-- Specifies the editable state for the image view.
--
-- ObjC selector: @- editable@
editable :: IsIKImageView ikImageView => ikImageView -> IO Bool
editable ikImageView =
  sendMessage ikImageView editableSelector

-- | editable
--
-- Specifies the editable state for the image view.
--
-- ObjC selector: @- setEditable:@
setEditable :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setEditable ikImageView value =
  sendMessage ikImageView setEditableSelector value

-- | doubleClickOpensImageEditPane
--
-- Specifies the image-opening state of the editing pane in the image view.
--
-- ObjC selector: @- doubleClickOpensImageEditPanel@
doubleClickOpensImageEditPanel :: IsIKImageView ikImageView => ikImageView -> IO Bool
doubleClickOpensImageEditPanel ikImageView =
  sendMessage ikImageView doubleClickOpensImageEditPanelSelector

-- | doubleClickOpensImageEditPane
--
-- Specifies the image-opening state of the editing pane in the image view.
--
-- ObjC selector: @- setDoubleClickOpensImageEditPanel:@
setDoubleClickOpensImageEditPanel :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setDoubleClickOpensImageEditPanel ikImageView value =
  sendMessage ikImageView setDoubleClickOpensImageEditPanelSelector value

-- | imageCorrection
--
-- Specifies a Core Image filter for image correction.
--
-- ObjC selector: @- imageCorrection@
imageCorrection :: IsIKImageView ikImageView => ikImageView -> IO (Id CIFilter)
imageCorrection ikImageView =
  sendMessage ikImageView imageCorrectionSelector

-- | imageCorrection
--
-- Specifies a Core Image filter for image correction.
--
-- ObjC selector: @- setImageCorrection:@
setImageCorrection :: (IsIKImageView ikImageView, IsCIFilter value) => ikImageView -> value -> IO ()
setImageCorrection ikImageView value =
  sendMessage ikImageView setImageCorrectionSelector (toCIFilter value)

-- | backgroundColor
--
-- Specifies the background color for the image view.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsIKImageView ikImageView => ikImageView -> IO (Id NSColor)
backgroundColor ikImageView =
  sendMessage ikImageView backgroundColorSelector

-- | backgroundColor
--
-- Specifies the background color for the image view.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsIKImageView ikImageView, IsNSColor value) => ikImageView -> value -> IO ()
setBackgroundColor ikImageView value =
  sendMessage ikImageView setBackgroundColorSelector (toNSColor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setImage:imageProperties:@
setImage_imagePropertiesSelector :: Selector '[Ptr (), Id NSDictionary] ()
setImage_imagePropertiesSelector = mkSelector "setImage:imageProperties:"

-- | @Selector@ for @setImageWithURL:@
setImageWithURLSelector :: Selector '[Id NSURL] ()
setImageWithURLSelector = mkSelector "setImageWithURL:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Ptr ())
imageSelector = mkSelector "image"

-- | @Selector@ for @imageSize@
imageSizeSelector :: Selector '[] NSSize
imageSizeSelector = mkSelector "imageSize"

-- | @Selector@ for @imageProperties@
imagePropertiesSelector :: Selector '[] (Id NSDictionary)
imagePropertiesSelector = mkSelector "imageProperties"

-- | @Selector@ for @setRotationAngle:centerPoint:@
setRotationAngle_centerPointSelector :: Selector '[CDouble, NSPoint] ()
setRotationAngle_centerPointSelector = mkSelector "setRotationAngle:centerPoint:"

-- | @Selector@ for @rotateImageLeft:@
rotateImageLeftSelector :: Selector '[RawId] ()
rotateImageLeftSelector = mkSelector "rotateImageLeft:"

-- | @Selector@ for @rotateImageRight:@
rotateImageRightSelector :: Selector '[RawId] ()
rotateImageRightSelector = mkSelector "rotateImageRight:"

-- | @Selector@ for @setImageZoomFactor:centerPoint:@
setImageZoomFactor_centerPointSelector :: Selector '[CDouble, NSPoint] ()
setImageZoomFactor_centerPointSelector = mkSelector "setImageZoomFactor:centerPoint:"

-- | @Selector@ for @zoomImageToRect:@
zoomImageToRectSelector :: Selector '[NSRect] ()
zoomImageToRectSelector = mkSelector "zoomImageToRect:"

-- | @Selector@ for @zoomImageToFit:@
zoomImageToFitSelector :: Selector '[RawId] ()
zoomImageToFitSelector = mkSelector "zoomImageToFit:"

-- | @Selector@ for @zoomImageToActualSize:@
zoomImageToActualSizeSelector :: Selector '[RawId] ()
zoomImageToActualSizeSelector = mkSelector "zoomImageToActualSize:"

-- | @Selector@ for @zoomIn:@
zoomInSelector :: Selector '[RawId] ()
zoomInSelector = mkSelector "zoomIn:"

-- | @Selector@ for @zoomOut:@
zoomOutSelector :: Selector '[RawId] ()
zoomOutSelector = mkSelector "zoomOut:"

-- | @Selector@ for @flipImageHorizontal:@
flipImageHorizontalSelector :: Selector '[RawId] ()
flipImageHorizontalSelector = mkSelector "flipImageHorizontal:"

-- | @Selector@ for @flipImageVertical:@
flipImageVerticalSelector :: Selector '[RawId] ()
flipImageVerticalSelector = mkSelector "flipImageVertical:"

-- | @Selector@ for @crop:@
cropSelector :: Selector '[RawId] ()
cropSelector = mkSelector "crop:"

-- | @Selector@ for @setOverlay:forType:@
setOverlay_forTypeSelector :: Selector '[Id CALayer, Id NSString] ()
setOverlay_forTypeSelector = mkSelector "setOverlay:forType:"

-- | @Selector@ for @overlayForType:@
overlayForTypeSelector :: Selector '[Id NSString] (Id CALayer)
overlayForTypeSelector = mkSelector "overlayForType:"

-- | @Selector@ for @scrollToPoint:@
scrollToPointSelector :: Selector '[NSPoint] ()
scrollToPointSelector = mkSelector "scrollToPoint:"

-- | @Selector@ for @scrollToRect:@
scrollToRectSelector :: Selector '[NSRect] ()
scrollToRectSelector = mkSelector "scrollToRect:"

-- | @Selector@ for @convertViewPointToImagePoint:@
convertViewPointToImagePointSelector :: Selector '[NSPoint] NSPoint
convertViewPointToImagePointSelector = mkSelector "convertViewPointToImagePoint:"

-- | @Selector@ for @convertViewRectToImageRect:@
convertViewRectToImageRectSelector :: Selector '[NSRect] NSRect
convertViewRectToImageRectSelector = mkSelector "convertViewRectToImageRect:"

-- | @Selector@ for @convertImagePointToViewPoint:@
convertImagePointToViewPointSelector :: Selector '[NSPoint] NSPoint
convertImagePointToViewPointSelector = mkSelector "convertImagePointToViewPoint:"

-- | @Selector@ for @convertImageRectToViewRect:@
convertImageRectToViewRectSelector :: Selector '[NSRect] NSRect
convertImageRectToViewRectSelector = mkSelector "convertImageRectToViewRect:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @zoomFactor@
zoomFactorSelector :: Selector '[] CDouble
zoomFactorSelector = mkSelector "zoomFactor"

-- | @Selector@ for @setZoomFactor:@
setZoomFactorSelector :: Selector '[CDouble] ()
setZoomFactorSelector = mkSelector "setZoomFactor:"

-- | @Selector@ for @rotationAngle@
rotationAngleSelector :: Selector '[] CDouble
rotationAngleSelector = mkSelector "rotationAngle"

-- | @Selector@ for @setRotationAngle:@
setRotationAngleSelector :: Selector '[CDouble] ()
setRotationAngleSelector = mkSelector "setRotationAngle:"

-- | @Selector@ for @currentToolMode@
currentToolModeSelector :: Selector '[] (Id NSString)
currentToolModeSelector = mkSelector "currentToolMode"

-- | @Selector@ for @setCurrentToolMode:@
setCurrentToolModeSelector :: Selector '[Id NSString] ()
setCurrentToolModeSelector = mkSelector "setCurrentToolMode:"

-- | @Selector@ for @autoresizes@
autoresizesSelector :: Selector '[] Bool
autoresizesSelector = mkSelector "autoresizes"

-- | @Selector@ for @setAutoresizes:@
setAutoresizesSelector :: Selector '[Bool] ()
setAutoresizesSelector = mkSelector "setAutoresizes:"

-- | @Selector@ for @hasHorizontalScroller@
hasHorizontalScrollerSelector :: Selector '[] Bool
hasHorizontalScrollerSelector = mkSelector "hasHorizontalScroller"

-- | @Selector@ for @setHasHorizontalScroller:@
setHasHorizontalScrollerSelector :: Selector '[Bool] ()
setHasHorizontalScrollerSelector = mkSelector "setHasHorizontalScroller:"

-- | @Selector@ for @hasVerticalScroller@
hasVerticalScrollerSelector :: Selector '[] Bool
hasVerticalScrollerSelector = mkSelector "hasVerticalScroller"

-- | @Selector@ for @setHasVerticalScroller:@
setHasVerticalScrollerSelector :: Selector '[Bool] ()
setHasVerticalScrollerSelector = mkSelector "setHasVerticalScroller:"

-- | @Selector@ for @autohidesScrollers@
autohidesScrollersSelector :: Selector '[] Bool
autohidesScrollersSelector = mkSelector "autohidesScrollers"

-- | @Selector@ for @setAutohidesScrollers:@
setAutohidesScrollersSelector :: Selector '[Bool] ()
setAutohidesScrollersSelector = mkSelector "setAutohidesScrollers:"

-- | @Selector@ for @supportsDragAndDrop@
supportsDragAndDropSelector :: Selector '[] Bool
supportsDragAndDropSelector = mkSelector "supportsDragAndDrop"

-- | @Selector@ for @setSupportsDragAndDrop:@
setSupportsDragAndDropSelector :: Selector '[Bool] ()
setSupportsDragAndDropSelector = mkSelector "setSupportsDragAndDrop:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @doubleClickOpensImageEditPanel@
doubleClickOpensImageEditPanelSelector :: Selector '[] Bool
doubleClickOpensImageEditPanelSelector = mkSelector "doubleClickOpensImageEditPanel"

-- | @Selector@ for @setDoubleClickOpensImageEditPanel:@
setDoubleClickOpensImageEditPanelSelector :: Selector '[Bool] ()
setDoubleClickOpensImageEditPanelSelector = mkSelector "setDoubleClickOpensImageEditPanel:"

-- | @Selector@ for @imageCorrection@
imageCorrectionSelector :: Selector '[] (Id CIFilter)
imageCorrectionSelector = mkSelector "imageCorrection"

-- | @Selector@ for @setImageCorrection:@
setImageCorrectionSelector :: Selector '[Id CIFilter] ()
setImageCorrectionSelector = mkSelector "setImageCorrection:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

