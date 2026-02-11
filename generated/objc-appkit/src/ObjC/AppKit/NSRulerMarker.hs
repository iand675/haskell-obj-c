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
  , imageRectInRuler
  , thicknessRequiredInRuler
  , initWithRulerView_markerLocation_image_imageOriginSelector
  , initWithCoderSelector
  , initSelector
  , drawRectSelector
  , trackMouse_addingSelector
  , rulerSelector
  , markerLocationSelector
  , setMarkerLocationSelector
  , imageSelector
  , setImageSelector
  , imageOriginSelector
  , setImageOriginSelector
  , movableSelector
  , setMovableSelector
  , removableSelector
  , setRemovableSelector
  , draggingSelector
  , imageRectInRulerSelector
  , thicknessRequiredInRulerSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- initWithRulerView:markerLocation:image:imageOrigin:@
initWithRulerView_markerLocation_image_imageOrigin :: (IsNSRulerMarker nsRulerMarker, IsNSRulerView ruler, IsNSImage image) => nsRulerMarker -> ruler -> CDouble -> image -> NSPoint -> IO (Id NSRulerMarker)
initWithRulerView_markerLocation_image_imageOrigin nsRulerMarker  ruler location image imageOrigin =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr image $ \raw_image ->
      sendMsg nsRulerMarker (mkSelector "initWithRulerView:markerLocation:image:imageOrigin:") (retPtr retVoid) [argPtr (castPtr raw_ruler :: Ptr ()), argCDouble (fromIntegral location), argPtr (castPtr raw_image :: Ptr ()), argNSPoint imageOrigin] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSRulerMarker nsRulerMarker, IsNSCoder coder) => nsRulerMarker -> coder -> IO (Id NSRulerMarker)
initWithCoder nsRulerMarker  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsRulerMarker (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO (Id NSRulerMarker)
init_ nsRulerMarker  =
  sendMsg nsRulerMarker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- drawRect:@
drawRect :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> NSRect -> IO ()
drawRect nsRulerMarker  rect =
  sendMsg nsRulerMarker (mkSelector "drawRect:") retVoid [argNSRect rect]

-- | @- trackMouse:adding:@
trackMouse_adding :: (IsNSRulerMarker nsRulerMarker, IsNSEvent mouseDownEvent) => nsRulerMarker -> mouseDownEvent -> Bool -> IO Bool
trackMouse_adding nsRulerMarker  mouseDownEvent isAdding =
withObjCPtr mouseDownEvent $ \raw_mouseDownEvent ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRulerMarker (mkSelector "trackMouse:adding:") retCULong [argPtr (castPtr raw_mouseDownEvent :: Ptr ()), argCULong (if isAdding then 1 else 0)]

-- | ********************* Query/Set basic attributes **********************
--
-- ObjC selector: @- ruler@
ruler :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO (Id NSRulerView)
ruler nsRulerMarker  =
  sendMsg nsRulerMarker (mkSelector "ruler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- markerLocation@
markerLocation :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO CDouble
markerLocation nsRulerMarker  =
  sendMsg nsRulerMarker (mkSelector "markerLocation") retCDouble []

-- | @- setMarkerLocation:@
setMarkerLocation :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> CDouble -> IO ()
setMarkerLocation nsRulerMarker  value =
  sendMsg nsRulerMarker (mkSelector "setMarkerLocation:") retVoid [argCDouble (fromIntegral value)]

-- | @- image@
image :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO (Id NSImage)
image nsRulerMarker  =
  sendMsg nsRulerMarker (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSRulerMarker nsRulerMarker, IsNSImage value) => nsRulerMarker -> value -> IO ()
setImage nsRulerMarker  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRulerMarker (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageOrigin@
imageOrigin :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO NSPoint
imageOrigin nsRulerMarker  =
  sendMsgStret nsRulerMarker (mkSelector "imageOrigin") retNSPoint []

-- | @- setImageOrigin:@
setImageOrigin :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> NSPoint -> IO ()
setImageOrigin nsRulerMarker  value =
  sendMsg nsRulerMarker (mkSelector "setImageOrigin:") retVoid [argNSPoint value]

-- | @- movable@
movable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO Bool
movable nsRulerMarker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRulerMarker (mkSelector "movable") retCULong []

-- | @- setMovable:@
setMovable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> Bool -> IO ()
setMovable nsRulerMarker  value =
  sendMsg nsRulerMarker (mkSelector "setMovable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- removable@
removable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO Bool
removable nsRulerMarker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRulerMarker (mkSelector "removable") retCULong []

-- | @- setRemovable:@
setRemovable :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> Bool -> IO ()
setRemovable nsRulerMarker  value =
  sendMsg nsRulerMarker (mkSelector "setRemovable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- dragging@
dragging :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO Bool
dragging nsRulerMarker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRulerMarker (mkSelector "dragging") retCULong []

-- | ************************ Ruler facilities *************************
--
-- ObjC selector: @- imageRectInRuler@
imageRectInRuler :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO NSRect
imageRectInRuler nsRulerMarker  =
  sendMsgStret nsRulerMarker (mkSelector "imageRectInRuler") retNSRect []

-- | @- thicknessRequiredInRuler@
thicknessRequiredInRuler :: IsNSRulerMarker nsRulerMarker => nsRulerMarker -> IO CDouble
thicknessRequiredInRuler nsRulerMarker  =
  sendMsg nsRulerMarker (mkSelector "thicknessRequiredInRuler") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRulerView:markerLocation:image:imageOrigin:@
initWithRulerView_markerLocation_image_imageOriginSelector :: Selector
initWithRulerView_markerLocation_image_imageOriginSelector = mkSelector "initWithRulerView:markerLocation:image:imageOrigin:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @drawRect:@
drawRectSelector :: Selector
drawRectSelector = mkSelector "drawRect:"

-- | @Selector@ for @trackMouse:adding:@
trackMouse_addingSelector :: Selector
trackMouse_addingSelector = mkSelector "trackMouse:adding:"

-- | @Selector@ for @ruler@
rulerSelector :: Selector
rulerSelector = mkSelector "ruler"

-- | @Selector@ for @markerLocation@
markerLocationSelector :: Selector
markerLocationSelector = mkSelector "markerLocation"

-- | @Selector@ for @setMarkerLocation:@
setMarkerLocationSelector :: Selector
setMarkerLocationSelector = mkSelector "setMarkerLocation:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @imageOrigin@
imageOriginSelector :: Selector
imageOriginSelector = mkSelector "imageOrigin"

-- | @Selector@ for @setImageOrigin:@
setImageOriginSelector :: Selector
setImageOriginSelector = mkSelector "setImageOrigin:"

-- | @Selector@ for @movable@
movableSelector :: Selector
movableSelector = mkSelector "movable"

-- | @Selector@ for @setMovable:@
setMovableSelector :: Selector
setMovableSelector = mkSelector "setMovable:"

-- | @Selector@ for @removable@
removableSelector :: Selector
removableSelector = mkSelector "removable"

-- | @Selector@ for @setRemovable:@
setRemovableSelector :: Selector
setRemovableSelector = mkSelector "setRemovable:"

-- | @Selector@ for @dragging@
draggingSelector :: Selector
draggingSelector = mkSelector "dragging"

-- | @Selector@ for @imageRectInRuler@
imageRectInRulerSelector :: Selector
imageRectInRulerSelector = mkSelector "imageRectInRuler"

-- | @Selector@ for @thicknessRequiredInRuler@
thicknessRequiredInRulerSelector :: Selector
thicknessRequiredInRulerSelector = mkSelector "thicknessRequiredInRuler"

