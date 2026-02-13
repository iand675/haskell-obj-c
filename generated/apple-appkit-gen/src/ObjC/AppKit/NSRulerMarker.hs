{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRulerMarker@.
module ObjC.AppKit.NSRulerMarker
  ( NSRulerMarker
  , IsNSRulerMarker(..)
  , initWithRulerView_markerLocation_image_imageOrigin
  , initWithCoder
  , init_
  , drawRect
  , trackMouse_adding
  , ruler
  , markerLocation
  , setMarkerLocation
  , image
  , setImage
  , imageOrigin
  , setImageOrigin
  , movable
  , setMovable
  , removable
  , setRemovable
  , dragging
  , representedObject
  , setRepresentedObject
  , imageRectInRuler
  , thicknessRequiredInRuler
  , draggingSelector
  , drawRectSelector
  , imageOriginSelector
  , imageRectInRulerSelector
  , imageSelector
  , initSelector
  , initWithCoderSelector
  , initWithRulerView_markerLocation_image_imageOriginSelector
  , markerLocationSelector
  , movableSelector
  , removableSelector
  , representedObjectSelector
  , rulerSelector
  , setImageOriginSelector
  , setImageSelector
  , setMarkerLocationSelector
  , setMovableSelector
  , setRemovableSelector
  , setRepresentedObjectSelector
  , thicknessRequiredInRulerSelector
  , trackMouse_addingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- initWithRulerView:markerLocation:image:imageOrigin:@
initWithRulerView_markerLocation_image_imageOrigin :: (IsNSRulerMarker nsRulerMarker, IsNSRulerView ruler, IsNSImage image) => nsRulerMarker -> ruler -> CDouble -> image -> NSPoint -> IO (Id NSRulerMarker)
initWithRulerView_markerLocation_image_imageOrigin nsRulerMarker ruler location image imageOrigin =
  sendOwnedMessage nsRulerMarker initWithRulerView_markerLocation_image_imageOriginSelector (toNSRulerView ruler) location (toNSImage image) imageOrigin

-- | @- initWithCoder:@
initWithCoder :: (IsNSRulerMarker nsRulerMarker, IsNSCoder coder) => nsRulerMarker -> coder -> IO (Id NSRulerMarker)
initWithCoder nsRulerMarker coder =
  sendOwnedMessage nsRulerMarker initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO (Id NSRulerMarker)
init_ nsRulerMarker =
  sendOwnedMessage nsRulerMarker initSelector

-- | @- drawRect:@
drawRect :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> NSRect -> IO ()
drawRect nsRulerMarker rect =
  sendMessage nsRulerMarker drawRectSelector rect

-- | @- trackMouse:adding:@
trackMouse_adding :: (IsNSRulerMarker nsRulerMarker, IsNSEvent mouseDownEvent) => nsRulerMarker -> mouseDownEvent -> Bool -> IO Bool
trackMouse_adding nsRulerMarker mouseDownEvent isAdding =
  sendMessage nsRulerMarker trackMouse_addingSelector (toNSEvent mouseDownEvent) isAdding

-- | ********************* Query/Set basic attributes **********************
--
-- ObjC selector: @- ruler@
ruler :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO (Id NSRulerView)
ruler nsRulerMarker =
  sendMessage nsRulerMarker rulerSelector

-- | @- markerLocation@
markerLocation :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO CDouble
markerLocation nsRulerMarker =
  sendMessage nsRulerMarker markerLocationSelector

-- | @- setMarkerLocation:@
setMarkerLocation :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> CDouble -> IO ()
setMarkerLocation nsRulerMarker value =
  sendMessage nsRulerMarker setMarkerLocationSelector value

-- | @- image@
image :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO (Id NSImage)
image nsRulerMarker =
  sendMessage nsRulerMarker imageSelector

-- | @- setImage:@
setImage :: (IsNSRulerMarker nsRulerMarker, IsNSImage value) => nsRulerMarker -> value -> IO ()
setImage nsRulerMarker value =
  sendMessage nsRulerMarker setImageSelector (toNSImage value)

-- | @- imageOrigin@
imageOrigin :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO NSPoint
imageOrigin nsRulerMarker =
  sendMessage nsRulerMarker imageOriginSelector

-- | @- setImageOrigin:@
setImageOrigin :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> NSPoint -> IO ()
setImageOrigin nsRulerMarker value =
  sendMessage nsRulerMarker setImageOriginSelector value

-- | @- movable@
movable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO Bool
movable nsRulerMarker =
  sendMessage nsRulerMarker movableSelector

-- | @- setMovable:@
setMovable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> Bool -> IO ()
setMovable nsRulerMarker value =
  sendMessage nsRulerMarker setMovableSelector value

-- | @- removable@
removable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO Bool
removable nsRulerMarker =
  sendMessage nsRulerMarker removableSelector

-- | @- setRemovable:@
setRemovable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> Bool -> IO ()
setRemovable nsRulerMarker value =
  sendMessage nsRulerMarker setRemovableSelector value

-- | @- dragging@
dragging :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO Bool
dragging nsRulerMarker =
  sendMessage nsRulerMarker draggingSelector

-- | @- representedObject@
representedObject :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO RawId
representedObject nsRulerMarker =
  sendMessage nsRulerMarker representedObjectSelector

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> RawId -> IO ()
setRepresentedObject nsRulerMarker value =
  sendMessage nsRulerMarker setRepresentedObjectSelector value

-- | ************************ Ruler facilities *************************
--
-- ObjC selector: @- imageRectInRuler@
imageRectInRuler :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO NSRect
imageRectInRuler nsRulerMarker =
  sendMessage nsRulerMarker imageRectInRulerSelector

-- | @- thicknessRequiredInRuler@
thicknessRequiredInRuler :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO CDouble
thicknessRequiredInRuler nsRulerMarker =
  sendMessage nsRulerMarker thicknessRequiredInRulerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRulerView:markerLocation:image:imageOrigin:@
initWithRulerView_markerLocation_image_imageOriginSelector :: Selector '[Id NSRulerView, CDouble, Id NSImage, NSPoint] (Id NSRulerMarker)
initWithRulerView_markerLocation_image_imageOriginSelector = mkSelector "initWithRulerView:markerLocation:image:imageOrigin:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSRulerMarker)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSRulerMarker)
initSelector = mkSelector "init"

-- | @Selector@ for @drawRect:@
drawRectSelector :: Selector '[NSRect] ()
drawRectSelector = mkSelector "drawRect:"

-- | @Selector@ for @trackMouse:adding:@
trackMouse_addingSelector :: Selector '[Id NSEvent, Bool] Bool
trackMouse_addingSelector = mkSelector "trackMouse:adding:"

-- | @Selector@ for @ruler@
rulerSelector :: Selector '[] (Id NSRulerView)
rulerSelector = mkSelector "ruler"

-- | @Selector@ for @markerLocation@
markerLocationSelector :: Selector '[] CDouble
markerLocationSelector = mkSelector "markerLocation"

-- | @Selector@ for @setMarkerLocation:@
setMarkerLocationSelector :: Selector '[CDouble] ()
setMarkerLocationSelector = mkSelector "setMarkerLocation:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @imageOrigin@
imageOriginSelector :: Selector '[] NSPoint
imageOriginSelector = mkSelector "imageOrigin"

-- | @Selector@ for @setImageOrigin:@
setImageOriginSelector :: Selector '[NSPoint] ()
setImageOriginSelector = mkSelector "setImageOrigin:"

-- | @Selector@ for @movable@
movableSelector :: Selector '[] Bool
movableSelector = mkSelector "movable"

-- | @Selector@ for @setMovable:@
setMovableSelector :: Selector '[Bool] ()
setMovableSelector = mkSelector "setMovable:"

-- | @Selector@ for @removable@
removableSelector :: Selector '[] Bool
removableSelector = mkSelector "removable"

-- | @Selector@ for @setRemovable:@
setRemovableSelector :: Selector '[Bool] ()
setRemovableSelector = mkSelector "setRemovable:"

-- | @Selector@ for @dragging@
draggingSelector :: Selector '[] Bool
draggingSelector = mkSelector "dragging"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector '[] RawId
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector '[RawId] ()
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @imageRectInRuler@
imageRectInRulerSelector :: Selector '[] NSRect
imageRectInRulerSelector = mkSelector "imageRectInRuler"

-- | @Selector@ for @thicknessRequiredInRuler@
thicknessRequiredInRulerSelector :: Selector '[] CDouble
thicknessRequiredInRulerSelector = mkSelector "thicknessRequiredInRuler"

