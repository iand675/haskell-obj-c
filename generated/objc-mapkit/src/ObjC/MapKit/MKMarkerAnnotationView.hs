{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMarkerAnnotationView@.
module ObjC.MapKit.MKMarkerAnnotationView
  ( MKMarkerAnnotationView
  , IsMKMarkerAnnotationView(..)
  , titleVisibility
  , setTitleVisibility
  , subtitleVisibility
  , setSubtitleVisibility
  , markerTintColor
  , setMarkerTintColor
  , glyphTintColor
  , setGlyphTintColor
  , glyphText
  , setGlyphText
  , glyphImage
  , setGlyphImage
  , selectedGlyphImage
  , setSelectedGlyphImage
  , animatesWhenAdded
  , setAnimatesWhenAdded
  , titleVisibilitySelector
  , setTitleVisibilitySelector
  , subtitleVisibilitySelector
  , setSubtitleVisibilitySelector
  , markerTintColorSelector
  , setMarkerTintColorSelector
  , glyphTintColorSelector
  , setGlyphTintColorSelector
  , glyphTextSelector
  , setGlyphTextSelector
  , glyphImageSelector
  , setGlyphImageSelector
  , selectedGlyphImageSelector
  , setSelectedGlyphImageSelector
  , animatesWhenAddedSelector
  , setAnimatesWhenAddedSelector

  -- * Enum types
  , MKFeatureVisibility(MKFeatureVisibility)
  , pattern MKFeatureVisibilityAdaptive
  , pattern MKFeatureVisibilityHidden
  , pattern MKFeatureVisibilityVisible

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

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- titleVisibility@
titleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO MKFeatureVisibility
titleVisibility mkMarkerAnnotationView  =
  fmap (coerce :: CLong -> MKFeatureVisibility) $ sendMsg mkMarkerAnnotationView (mkSelector "titleVisibility") retCLong []

-- | @- setTitleVisibility:@
setTitleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> MKFeatureVisibility -> IO ()
setTitleVisibility mkMarkerAnnotationView  value =
  sendMsg mkMarkerAnnotationView (mkSelector "setTitleVisibility:") retVoid [argCLong (coerce value)]

-- | @- subtitleVisibility@
subtitleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO MKFeatureVisibility
subtitleVisibility mkMarkerAnnotationView  =
  fmap (coerce :: CLong -> MKFeatureVisibility) $ sendMsg mkMarkerAnnotationView (mkSelector "subtitleVisibility") retCLong []

-- | @- setSubtitleVisibility:@
setSubtitleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> MKFeatureVisibility -> IO ()
setSubtitleVisibility mkMarkerAnnotationView  value =
  sendMsg mkMarkerAnnotationView (mkSelector "setSubtitleVisibility:") retVoid [argCLong (coerce value)]

-- | @- markerTintColor@
markerTintColor :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSColor)
markerTintColor mkMarkerAnnotationView  =
  sendMsg mkMarkerAnnotationView (mkSelector "markerTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarkerTintColor:@
setMarkerTintColor :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSColor value) => mkMarkerAnnotationView -> value -> IO ()
setMarkerTintColor mkMarkerAnnotationView  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMarkerAnnotationView (mkSelector "setMarkerTintColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- glyphTintColor@
glyphTintColor :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSColor)
glyphTintColor mkMarkerAnnotationView  =
  sendMsg mkMarkerAnnotationView (mkSelector "glyphTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGlyphTintColor:@
setGlyphTintColor :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSColor value) => mkMarkerAnnotationView -> value -> IO ()
setGlyphTintColor mkMarkerAnnotationView  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMarkerAnnotationView (mkSelector "setGlyphTintColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- glyphText@
glyphText :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSString)
glyphText mkMarkerAnnotationView  =
  sendMsg mkMarkerAnnotationView (mkSelector "glyphText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGlyphText:@
setGlyphText :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSString value) => mkMarkerAnnotationView -> value -> IO ()
setGlyphText mkMarkerAnnotationView  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMarkerAnnotationView (mkSelector "setGlyphText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- glyphImage@
glyphImage :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSImage)
glyphImage mkMarkerAnnotationView  =
  sendMsg mkMarkerAnnotationView (mkSelector "glyphImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGlyphImage:@
setGlyphImage :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSImage value) => mkMarkerAnnotationView -> value -> IO ()
setGlyphImage mkMarkerAnnotationView  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMarkerAnnotationView (mkSelector "setGlyphImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectedGlyphImage@
selectedGlyphImage :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSImage)
selectedGlyphImage mkMarkerAnnotationView  =
  sendMsg mkMarkerAnnotationView (mkSelector "selectedGlyphImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedGlyphImage:@
setSelectedGlyphImage :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSImage value) => mkMarkerAnnotationView -> value -> IO ()
setSelectedGlyphImage mkMarkerAnnotationView  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMarkerAnnotationView (mkSelector "setSelectedGlyphImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- animatesWhenAdded@
animatesWhenAdded :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO Bool
animatesWhenAdded mkMarkerAnnotationView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMarkerAnnotationView (mkSelector "animatesWhenAdded") retCULong []

-- | @- setAnimatesWhenAdded:@
setAnimatesWhenAdded :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> Bool -> IO ()
setAnimatesWhenAdded mkMarkerAnnotationView  value =
  sendMsg mkMarkerAnnotationView (mkSelector "setAnimatesWhenAdded:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @titleVisibility@
titleVisibilitySelector :: Selector
titleVisibilitySelector = mkSelector "titleVisibility"

-- | @Selector@ for @setTitleVisibility:@
setTitleVisibilitySelector :: Selector
setTitleVisibilitySelector = mkSelector "setTitleVisibility:"

-- | @Selector@ for @subtitleVisibility@
subtitleVisibilitySelector :: Selector
subtitleVisibilitySelector = mkSelector "subtitleVisibility"

-- | @Selector@ for @setSubtitleVisibility:@
setSubtitleVisibilitySelector :: Selector
setSubtitleVisibilitySelector = mkSelector "setSubtitleVisibility:"

-- | @Selector@ for @markerTintColor@
markerTintColorSelector :: Selector
markerTintColorSelector = mkSelector "markerTintColor"

-- | @Selector@ for @setMarkerTintColor:@
setMarkerTintColorSelector :: Selector
setMarkerTintColorSelector = mkSelector "setMarkerTintColor:"

-- | @Selector@ for @glyphTintColor@
glyphTintColorSelector :: Selector
glyphTintColorSelector = mkSelector "glyphTintColor"

-- | @Selector@ for @setGlyphTintColor:@
setGlyphTintColorSelector :: Selector
setGlyphTintColorSelector = mkSelector "setGlyphTintColor:"

-- | @Selector@ for @glyphText@
glyphTextSelector :: Selector
glyphTextSelector = mkSelector "glyphText"

-- | @Selector@ for @setGlyphText:@
setGlyphTextSelector :: Selector
setGlyphTextSelector = mkSelector "setGlyphText:"

-- | @Selector@ for @glyphImage@
glyphImageSelector :: Selector
glyphImageSelector = mkSelector "glyphImage"

-- | @Selector@ for @setGlyphImage:@
setGlyphImageSelector :: Selector
setGlyphImageSelector = mkSelector "setGlyphImage:"

-- | @Selector@ for @selectedGlyphImage@
selectedGlyphImageSelector :: Selector
selectedGlyphImageSelector = mkSelector "selectedGlyphImage"

-- | @Selector@ for @setSelectedGlyphImage:@
setSelectedGlyphImageSelector :: Selector
setSelectedGlyphImageSelector = mkSelector "setSelectedGlyphImage:"

-- | @Selector@ for @animatesWhenAdded@
animatesWhenAddedSelector :: Selector
animatesWhenAddedSelector = mkSelector "animatesWhenAdded"

-- | @Selector@ for @setAnimatesWhenAdded:@
setAnimatesWhenAddedSelector :: Selector
setAnimatesWhenAddedSelector = mkSelector "setAnimatesWhenAdded:"

