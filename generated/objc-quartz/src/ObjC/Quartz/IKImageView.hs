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
  , setImage_imagePropertiesSelector
  , setImageWithURLSelector
  , imageSelector
  , imageSizeSelector
  , imagePropertiesSelector
  , setRotationAngle_centerPointSelector
  , rotateImageLeftSelector
  , rotateImageRightSelector
  , setImageZoomFactor_centerPointSelector
  , zoomImageToRectSelector
  , zoomImageToFitSelector
  , zoomImageToActualSizeSelector
  , zoomInSelector
  , zoomOutSelector
  , flipImageHorizontalSelector
  , flipImageVerticalSelector
  , cropSelector
  , setOverlay_forTypeSelector
  , overlayForTypeSelector
  , scrollToPointSelector
  , scrollToRectSelector
  , convertViewPointToImagePointSelector
  , convertViewRectToImageRectSelector
  , convertImagePointToViewPointSelector
  , convertImageRectToViewRectSelector
  , delegateSelector
  , setDelegateSelector
  , zoomFactorSelector
  , setZoomFactorSelector
  , rotationAngleSelector
  , setRotationAngleSelector
  , currentToolModeSelector
  , setCurrentToolModeSelector
  , autoresizesSelector
  , setAutoresizesSelector
  , hasHorizontalScrollerSelector
  , setHasHorizontalScrollerSelector
  , hasVerticalScrollerSelector
  , setHasVerticalScrollerSelector
  , autohidesScrollersSelector
  , setAutohidesScrollersSelector
  , supportsDragAndDropSelector
  , setSupportsDragAndDropSelector
  , editableSelector
  , setEditableSelector
  , doubleClickOpensImageEditPanelSelector
  , setDoubleClickOpensImageEditPanelSelector
  , imageCorrectionSelector
  , setImageCorrectionSelector
  , backgroundColorSelector
  , setBackgroundColorSelector


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
setImage_imageProperties ikImageView  image metaData =
withObjCPtr metaData $ \raw_metaData ->
    sendMsg ikImageView (mkSelector "setImage:imageProperties:") retVoid [argPtr image, argPtr (castPtr raw_metaData :: Ptr ())]

-- | setImageWithURL:
--
-- Initializes an image view with the image specified by a URL.
--
-- ObjC selector: @- setImageWithURL:@
setImageWithURL :: (IsIKImageView ikImageView, IsNSURL url) => ikImageView -> url -> IO ()
setImageWithURL ikImageView  url =
withObjCPtr url $ \raw_url ->
    sendMsg ikImageView (mkSelector "setImageWithURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | image
--
-- Returns the image associated with the view, after any image corrections.
--
-- ObjC selector: @- image@
image :: IsIKImageView ikImageView => ikImageView -> IO (Ptr ())
image ikImageView  =
  fmap castPtr $ sendMsg ikImageView (mkSelector "image") (retPtr retVoid) []

-- | imageSize
--
-- Returns the size of the image in the image view.
--
-- ObjC selector: @- imageSize@
imageSize :: IsIKImageView ikImageView => ikImageView -> IO NSSize
imageSize ikImageView  =
  sendMsgStret ikImageView (mkSelector "imageSize") retNSSize []

-- | imageProperties
--
-- Returns the metadata for the image in the view.
--
-- ObjC selector: @- imageProperties@
imageProperties :: IsIKImageView ikImageView => ikImageView -> IO (Id NSDictionary)
imageProperties ikImageView  =
  sendMsg ikImageView (mkSelector "imageProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setRotationAngle:centerPoint:
--
-- Sets the rotation angle at the provided origin.
--
-- ObjC selector: @- setRotationAngle:centerPoint:@
setRotationAngle_centerPoint :: IsIKImageView ikImageView => ikImageView -> CDouble -> NSPoint -> IO ()
setRotationAngle_centerPoint ikImageView  rotationAngle centerPoint =
  sendMsg ikImageView (mkSelector "setRotationAngle:centerPoint:") retVoid [argCDouble (fromIntegral rotationAngle), argNSPoint centerPoint]

-- | rotateImageLeft:
--
-- Rotates the image left.
--
-- ObjC selector: @- rotateImageLeft:@
rotateImageLeft :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
rotateImageLeft ikImageView  sender =
  sendMsg ikImageView (mkSelector "rotateImageLeft:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | rotateImageRight:
--
-- Rotates the image right.
--
-- ObjC selector: @- rotateImageRight:@
rotateImageRight :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
rotateImageRight ikImageView  sender =
  sendMsg ikImageView (mkSelector "rotateImageRight:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | setImageZoomFactor:centerPoint:
--
-- Sets the zoom factor at the provided origin.
--
-- ObjC selector: @- setImageZoomFactor:centerPoint:@
setImageZoomFactor_centerPoint :: IsIKImageView ikImageView => ikImageView -> CDouble -> NSPoint -> IO ()
setImageZoomFactor_centerPoint ikImageView  zoomFactor centerPoint =
  sendMsg ikImageView (mkSelector "setImageZoomFactor:centerPoint:") retVoid [argCDouble (fromIntegral zoomFactor), argNSPoint centerPoint]

-- | zoomImageToRect:
--
-- Zooms the image so that it fits in the specified rectangle.
--
-- ObjC selector: @- zoomImageToRect:@
zoomImageToRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO ()
zoomImageToRect ikImageView  rect =
  sendMsg ikImageView (mkSelector "zoomImageToRect:") retVoid [argNSRect rect]

-- | zoomImageToFit:
--
-- Zooms the image so that it fits in the image view.
--
-- ObjC selector: @- zoomImageToFit:@
zoomImageToFit :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomImageToFit ikImageView  sender =
  sendMsg ikImageView (mkSelector "zoomImageToFit:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | zoomImageToActualSize:
--
-- Zooms the image so that it is displayed using its true size.
--
-- ObjC selector: @- zoomImageToActualSize:@
zoomImageToActualSize :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomImageToActualSize ikImageView  sender =
  sendMsg ikImageView (mkSelector "zoomImageToActualSize:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | zoomIn:
--
-- Zooms the image in.
--
-- ObjC selector: @- zoomIn:@
zoomIn :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomIn ikImageView  sender =
  sendMsg ikImageView (mkSelector "zoomIn:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | zoomOut:
--
-- Zooms the image out.
--
-- ObjC selector: @- zoomOut:@
zoomOut :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
zoomOut ikImageView  sender =
  sendMsg ikImageView (mkSelector "zoomOut:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | flipImageHorizontal:
--
-- Flips an image along the horizontal axis.
--
-- ObjC selector: @- flipImageHorizontal:@
flipImageHorizontal :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
flipImageHorizontal ikImageView  sender =
  sendMsg ikImageView (mkSelector "flipImageHorizontal:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | flipImageVertical:
--
-- Flips an image along the vertical axis.
--
-- ObjC selector: @- flipImageVertical:@
flipImageVertical :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
flipImageVertical ikImageView  sender =
  sendMsg ikImageView (mkSelector "flipImageVertical:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | crop:
--
-- Crops the image using the current selection.
--
-- ObjC selector: @- crop:@
crop :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
crop ikImageView  sender =
  sendMsg ikImageView (mkSelector "crop:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | setOverlay:forType:
--
-- Sets an overlay (Core Animation layer) for the image or the image background.
--
-- ObjC selector: @- setOverlay:forType:@
setOverlay_forType :: (IsIKImageView ikImageView, IsCALayer layer, IsNSString layerType) => ikImageView -> layer -> layerType -> IO ()
setOverlay_forType ikImageView  layer layerType =
withObjCPtr layer $ \raw_layer ->
  withObjCPtr layerType $ \raw_layerType ->
      sendMsg ikImageView (mkSelector "setOverlay:forType:") retVoid [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_layerType :: Ptr ())]

-- | overlayForType:
--
-- Returns the overlay (Core Animation layer) for the image or the image background.
--
-- ObjC selector: @- overlayForType:@
overlayForType :: (IsIKImageView ikImageView, IsNSString layerType) => ikImageView -> layerType -> IO (Id CALayer)
overlayForType ikImageView  layerType =
withObjCPtr layerType $ \raw_layerType ->
    sendMsg ikImageView (mkSelector "overlayForType:") (retPtr retVoid) [argPtr (castPtr raw_layerType :: Ptr ())] >>= retainedObject . castPtr

-- | scrollToPoint:
--
-- Scrolls the view to the specified point.
--
-- ObjC selector: @- scrollToPoint:@
scrollToPoint :: IsIKImageView ikImageView => ikImageView -> NSPoint -> IO ()
scrollToPoint ikImageView  point =
  sendMsg ikImageView (mkSelector "scrollToPoint:") retVoid [argNSPoint point]

-- | scrollToRect:
--
-- Scrolls the view so that it includes the provided rectangular area.
--
-- ObjC selector: @- scrollToRect:@
scrollToRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO ()
scrollToRect ikImageView  rect =
  sendMsg ikImageView (mkSelector "scrollToRect:") retVoid [argNSRect rect]

-- | convertViewPointToImagePoint:
--
-- Converts an image view coordinate to an image coordinate.
--
-- ObjC selector: @- convertViewPointToImagePoint:@
convertViewPointToImagePoint :: IsIKImageView ikImageView => ikImageView -> NSPoint -> IO NSPoint
convertViewPointToImagePoint ikImageView  viewPoint =
  sendMsgStret ikImageView (mkSelector "convertViewPointToImagePoint:") retNSPoint [argNSPoint viewPoint]

-- | convertViewRectToImageRect:
--
-- Converts an image view rectangle to an image rectangle.
--
-- ObjC selector: @- convertViewRectToImageRect:@
convertViewRectToImageRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO NSRect
convertViewRectToImageRect ikImageView  viewRect =
  sendMsgStret ikImageView (mkSelector "convertViewRectToImageRect:") retNSRect [argNSRect viewRect]

-- | convertImagePointToViewPoint:
--
-- Converts an image coordinate to an image view coordinate.
--
-- ObjC selector: @- convertImagePointToViewPoint:@
convertImagePointToViewPoint :: IsIKImageView ikImageView => ikImageView -> NSPoint -> IO NSPoint
convertImagePointToViewPoint ikImageView  imagePoint =
  sendMsgStret ikImageView (mkSelector "convertImagePointToViewPoint:") retNSPoint [argNSPoint imagePoint]

-- | convertImageRectToViewRect:
--
-- Converts an image rectangle to an image view rectangle.
--
-- ObjC selector: @- convertImageRectToViewRect:@
convertImageRectToViewRect :: IsIKImageView ikImageView => ikImageView -> NSRect -> IO NSRect
convertImageRectToViewRect ikImageView  imageRect =
  sendMsgStret ikImageView (mkSelector "convertImageRectToViewRect:") retNSRect [argNSRect imageRect]

-- | delegate
--
-- Specifies the delegate object of the receiver.
--
-- ObjC selector: @- delegate@
delegate :: IsIKImageView ikImageView => ikImageView -> IO RawId
delegate ikImageView  =
  fmap (RawId . castPtr) $ sendMsg ikImageView (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- Specifies the delegate object of the receiver.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKImageView ikImageView => ikImageView -> RawId -> IO ()
setDelegate ikImageView  value =
  sendMsg ikImageView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | zoomFactor
--
-- Specifies the zoom factor for the image view.
--
-- ObjC selector: @- zoomFactor@
zoomFactor :: IsIKImageView ikImageView => ikImageView -> IO CDouble
zoomFactor ikImageView  =
  sendMsg ikImageView (mkSelector "zoomFactor") retCDouble []

-- | zoomFactor
--
-- Specifies the zoom factor for the image view.
--
-- ObjC selector: @- setZoomFactor:@
setZoomFactor :: IsIKImageView ikImageView => ikImageView -> CDouble -> IO ()
setZoomFactor ikImageView  value =
  sendMsg ikImageView (mkSelector "setZoomFactor:") retVoid [argCDouble (fromIntegral value)]

-- | rotationAngle
--
-- Specifies the rotation angle for the image view.
--
-- ObjC selector: @- rotationAngle@
rotationAngle :: IsIKImageView ikImageView => ikImageView -> IO CDouble
rotationAngle ikImageView  =
  sendMsg ikImageView (mkSelector "rotationAngle") retCDouble []

-- | rotationAngle
--
-- Specifies the rotation angle for the image view.
--
-- ObjC selector: @- setRotationAngle:@
setRotationAngle :: IsIKImageView ikImageView => ikImageView -> CDouble -> IO ()
setRotationAngle ikImageView  value =
  sendMsg ikImageView (mkSelector "setRotationAngle:") retVoid [argCDouble (fromIntegral value)]

-- | currentToolMode
--
-- Specifies the current tool mode for the image view.
--
-- ObjC selector: @- currentToolMode@
currentToolMode :: IsIKImageView ikImageView => ikImageView -> IO (Id NSString)
currentToolMode ikImageView  =
  sendMsg ikImageView (mkSelector "currentToolMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | currentToolMode
--
-- Specifies the current tool mode for the image view.
--
-- ObjC selector: @- setCurrentToolMode:@
setCurrentToolMode :: (IsIKImageView ikImageView, IsNSString value) => ikImageView -> value -> IO ()
setCurrentToolMode ikImageView  value =
withObjCPtr value $ \raw_value ->
    sendMsg ikImageView (mkSelector "setCurrentToolMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | autoresizes
--
-- Specifies the automatic resizing state for the image view.
--
-- ObjC selector: @- autoresizes@
autoresizes :: IsIKImageView ikImageView => ikImageView -> IO Bool
autoresizes ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "autoresizes") retCULong []

-- | autoresizes
--
-- Specifies the automatic resizing state for the image view.
--
-- ObjC selector: @- setAutoresizes:@
setAutoresizes :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setAutoresizes ikImageView  value =
  sendMsg ikImageView (mkSelector "setAutoresizes:") retVoid [argCULong (if value then 1 else 0)]

-- | hasHorizontalScroller
--
-- Specifies the horizontal scroll bar state for the image view.
--
-- ObjC selector: @- hasHorizontalScroller@
hasHorizontalScroller :: IsIKImageView ikImageView => ikImageView -> IO Bool
hasHorizontalScroller ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "hasHorizontalScroller") retCULong []

-- | hasHorizontalScroller
--
-- Specifies the horizontal scroll bar state for the image view.
--
-- ObjC selector: @- setHasHorizontalScroller:@
setHasHorizontalScroller :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setHasHorizontalScroller ikImageView  value =
  sendMsg ikImageView (mkSelector "setHasHorizontalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | hasVerticalScroller
--
-- Specifies the vertical scroll bar state for the image view.
--
-- ObjC selector: @- hasVerticalScroller@
hasVerticalScroller :: IsIKImageView ikImageView => ikImageView -> IO Bool
hasVerticalScroller ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "hasVerticalScroller") retCULong []

-- | hasVerticalScroller
--
-- Specifies the vertical scroll bar state for the image view.
--
-- ObjC selector: @- setHasVerticalScroller:@
setHasVerticalScroller :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setHasVerticalScroller ikImageView  value =
  sendMsg ikImageView (mkSelector "setHasVerticalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | autohidesScrollers
--
-- Specifies the automatic-hiding scroll bar state for the image view.
--
-- ObjC selector: @- autohidesScrollers@
autohidesScrollers :: IsIKImageView ikImageView => ikImageView -> IO Bool
autohidesScrollers ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "autohidesScrollers") retCULong []

-- | autohidesScrollers
--
-- Specifies the automatic-hiding scroll bar state for the image view.
--
-- ObjC selector: @- setAutohidesScrollers:@
setAutohidesScrollers :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setAutohidesScrollers ikImageView  value =
  sendMsg ikImageView (mkSelector "setAutohidesScrollers:") retVoid [argCULong (if value then 1 else 0)]

-- | supportsDragAndDrop
--
-- Specifies the drag-and-drop support state for the image view.
--
-- ObjC selector: @- supportsDragAndDrop@
supportsDragAndDrop :: IsIKImageView ikImageView => ikImageView -> IO Bool
supportsDragAndDrop ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "supportsDragAndDrop") retCULong []

-- | supportsDragAndDrop
--
-- Specifies the drag-and-drop support state for the image view.
--
-- ObjC selector: @- setSupportsDragAndDrop:@
setSupportsDragAndDrop :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setSupportsDragAndDrop ikImageView  value =
  sendMsg ikImageView (mkSelector "setSupportsDragAndDrop:") retVoid [argCULong (if value then 1 else 0)]

-- | editable
--
-- Specifies the editable state for the image view.
--
-- ObjC selector: @- editable@
editable :: IsIKImageView ikImageView => ikImageView -> IO Bool
editable ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "editable") retCULong []

-- | editable
--
-- Specifies the editable state for the image view.
--
-- ObjC selector: @- setEditable:@
setEditable :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setEditable ikImageView  value =
  sendMsg ikImageView (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | doubleClickOpensImageEditPane
--
-- Specifies the image-opening state of the editing pane in the image view.
--
-- ObjC selector: @- doubleClickOpensImageEditPanel@
doubleClickOpensImageEditPanel :: IsIKImageView ikImageView => ikImageView -> IO Bool
doubleClickOpensImageEditPanel ikImageView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikImageView (mkSelector "doubleClickOpensImageEditPanel") retCULong []

-- | doubleClickOpensImageEditPane
--
-- Specifies the image-opening state of the editing pane in the image view.
--
-- ObjC selector: @- setDoubleClickOpensImageEditPanel:@
setDoubleClickOpensImageEditPanel :: IsIKImageView ikImageView => ikImageView -> Bool -> IO ()
setDoubleClickOpensImageEditPanel ikImageView  value =
  sendMsg ikImageView (mkSelector "setDoubleClickOpensImageEditPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | imageCorrection
--
-- Specifies a Core Image filter for image correction.
--
-- ObjC selector: @- imageCorrection@
imageCorrection :: IsIKImageView ikImageView => ikImageView -> IO (Id CIFilter)
imageCorrection ikImageView  =
  sendMsg ikImageView (mkSelector "imageCorrection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | imageCorrection
--
-- Specifies a Core Image filter for image correction.
--
-- ObjC selector: @- setImageCorrection:@
setImageCorrection :: (IsIKImageView ikImageView, IsCIFilter value) => ikImageView -> value -> IO ()
setImageCorrection ikImageView  value =
withObjCPtr value $ \raw_value ->
    sendMsg ikImageView (mkSelector "setImageCorrection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | backgroundColor
--
-- Specifies the background color for the image view.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsIKImageView ikImageView => ikImageView -> IO (Id NSColor)
backgroundColor ikImageView  =
  sendMsg ikImageView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | backgroundColor
--
-- Specifies the background color for the image view.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsIKImageView ikImageView, IsNSColor value) => ikImageView -> value -> IO ()
setBackgroundColor ikImageView  value =
withObjCPtr value $ \raw_value ->
    sendMsg ikImageView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setImage:imageProperties:@
setImage_imagePropertiesSelector :: Selector
setImage_imagePropertiesSelector = mkSelector "setImage:imageProperties:"

-- | @Selector@ for @setImageWithURL:@
setImageWithURLSelector :: Selector
setImageWithURLSelector = mkSelector "setImageWithURL:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @imageSize@
imageSizeSelector :: Selector
imageSizeSelector = mkSelector "imageSize"

-- | @Selector@ for @imageProperties@
imagePropertiesSelector :: Selector
imagePropertiesSelector = mkSelector "imageProperties"

-- | @Selector@ for @setRotationAngle:centerPoint:@
setRotationAngle_centerPointSelector :: Selector
setRotationAngle_centerPointSelector = mkSelector "setRotationAngle:centerPoint:"

-- | @Selector@ for @rotateImageLeft:@
rotateImageLeftSelector :: Selector
rotateImageLeftSelector = mkSelector "rotateImageLeft:"

-- | @Selector@ for @rotateImageRight:@
rotateImageRightSelector :: Selector
rotateImageRightSelector = mkSelector "rotateImageRight:"

-- | @Selector@ for @setImageZoomFactor:centerPoint:@
setImageZoomFactor_centerPointSelector :: Selector
setImageZoomFactor_centerPointSelector = mkSelector "setImageZoomFactor:centerPoint:"

-- | @Selector@ for @zoomImageToRect:@
zoomImageToRectSelector :: Selector
zoomImageToRectSelector = mkSelector "zoomImageToRect:"

-- | @Selector@ for @zoomImageToFit:@
zoomImageToFitSelector :: Selector
zoomImageToFitSelector = mkSelector "zoomImageToFit:"

-- | @Selector@ for @zoomImageToActualSize:@
zoomImageToActualSizeSelector :: Selector
zoomImageToActualSizeSelector = mkSelector "zoomImageToActualSize:"

-- | @Selector@ for @zoomIn:@
zoomInSelector :: Selector
zoomInSelector = mkSelector "zoomIn:"

-- | @Selector@ for @zoomOut:@
zoomOutSelector :: Selector
zoomOutSelector = mkSelector "zoomOut:"

-- | @Selector@ for @flipImageHorizontal:@
flipImageHorizontalSelector :: Selector
flipImageHorizontalSelector = mkSelector "flipImageHorizontal:"

-- | @Selector@ for @flipImageVertical:@
flipImageVerticalSelector :: Selector
flipImageVerticalSelector = mkSelector "flipImageVertical:"

-- | @Selector@ for @crop:@
cropSelector :: Selector
cropSelector = mkSelector "crop:"

-- | @Selector@ for @setOverlay:forType:@
setOverlay_forTypeSelector :: Selector
setOverlay_forTypeSelector = mkSelector "setOverlay:forType:"

-- | @Selector@ for @overlayForType:@
overlayForTypeSelector :: Selector
overlayForTypeSelector = mkSelector "overlayForType:"

-- | @Selector@ for @scrollToPoint:@
scrollToPointSelector :: Selector
scrollToPointSelector = mkSelector "scrollToPoint:"

-- | @Selector@ for @scrollToRect:@
scrollToRectSelector :: Selector
scrollToRectSelector = mkSelector "scrollToRect:"

-- | @Selector@ for @convertViewPointToImagePoint:@
convertViewPointToImagePointSelector :: Selector
convertViewPointToImagePointSelector = mkSelector "convertViewPointToImagePoint:"

-- | @Selector@ for @convertViewRectToImageRect:@
convertViewRectToImageRectSelector :: Selector
convertViewRectToImageRectSelector = mkSelector "convertViewRectToImageRect:"

-- | @Selector@ for @convertImagePointToViewPoint:@
convertImagePointToViewPointSelector :: Selector
convertImagePointToViewPointSelector = mkSelector "convertImagePointToViewPoint:"

-- | @Selector@ for @convertImageRectToViewRect:@
convertImageRectToViewRectSelector :: Selector
convertImageRectToViewRectSelector = mkSelector "convertImageRectToViewRect:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @zoomFactor@
zoomFactorSelector :: Selector
zoomFactorSelector = mkSelector "zoomFactor"

-- | @Selector@ for @setZoomFactor:@
setZoomFactorSelector :: Selector
setZoomFactorSelector = mkSelector "setZoomFactor:"

-- | @Selector@ for @rotationAngle@
rotationAngleSelector :: Selector
rotationAngleSelector = mkSelector "rotationAngle"

-- | @Selector@ for @setRotationAngle:@
setRotationAngleSelector :: Selector
setRotationAngleSelector = mkSelector "setRotationAngle:"

-- | @Selector@ for @currentToolMode@
currentToolModeSelector :: Selector
currentToolModeSelector = mkSelector "currentToolMode"

-- | @Selector@ for @setCurrentToolMode:@
setCurrentToolModeSelector :: Selector
setCurrentToolModeSelector = mkSelector "setCurrentToolMode:"

-- | @Selector@ for @autoresizes@
autoresizesSelector :: Selector
autoresizesSelector = mkSelector "autoresizes"

-- | @Selector@ for @setAutoresizes:@
setAutoresizesSelector :: Selector
setAutoresizesSelector = mkSelector "setAutoresizes:"

-- | @Selector@ for @hasHorizontalScroller@
hasHorizontalScrollerSelector :: Selector
hasHorizontalScrollerSelector = mkSelector "hasHorizontalScroller"

-- | @Selector@ for @setHasHorizontalScroller:@
setHasHorizontalScrollerSelector :: Selector
setHasHorizontalScrollerSelector = mkSelector "setHasHorizontalScroller:"

-- | @Selector@ for @hasVerticalScroller@
hasVerticalScrollerSelector :: Selector
hasVerticalScrollerSelector = mkSelector "hasVerticalScroller"

-- | @Selector@ for @setHasVerticalScroller:@
setHasVerticalScrollerSelector :: Selector
setHasVerticalScrollerSelector = mkSelector "setHasVerticalScroller:"

-- | @Selector@ for @autohidesScrollers@
autohidesScrollersSelector :: Selector
autohidesScrollersSelector = mkSelector "autohidesScrollers"

-- | @Selector@ for @setAutohidesScrollers:@
setAutohidesScrollersSelector :: Selector
setAutohidesScrollersSelector = mkSelector "setAutohidesScrollers:"

-- | @Selector@ for @supportsDragAndDrop@
supportsDragAndDropSelector :: Selector
supportsDragAndDropSelector = mkSelector "supportsDragAndDrop"

-- | @Selector@ for @setSupportsDragAndDrop:@
setSupportsDragAndDropSelector :: Selector
setSupportsDragAndDropSelector = mkSelector "setSupportsDragAndDrop:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @doubleClickOpensImageEditPanel@
doubleClickOpensImageEditPanelSelector :: Selector
doubleClickOpensImageEditPanelSelector = mkSelector "doubleClickOpensImageEditPanel"

-- | @Selector@ for @setDoubleClickOpensImageEditPanel:@
setDoubleClickOpensImageEditPanelSelector :: Selector
setDoubleClickOpensImageEditPanelSelector = mkSelector "setDoubleClickOpensImageEditPanel:"

-- | @Selector@ for @imageCorrection@
imageCorrectionSelector :: Selector
imageCorrectionSelector = mkSelector "imageCorrection"

-- | @Selector@ for @setImageCorrection:@
setImageCorrectionSelector :: Selector
setImageCorrectionSelector = mkSelector "setImageCorrection:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

