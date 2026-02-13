{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , animatesWhenAddedSelector
  , glyphImageSelector
  , glyphTextSelector
  , glyphTintColorSelector
  , markerTintColorSelector
  , selectedGlyphImageSelector
  , setAnimatesWhenAddedSelector
  , setGlyphImageSelector
  , setGlyphTextSelector
  , setGlyphTintColorSelector
  , setMarkerTintColorSelector
  , setSelectedGlyphImageSelector
  , setSubtitleVisibilitySelector
  , setTitleVisibilitySelector
  , subtitleVisibilitySelector
  , titleVisibilitySelector

  -- * Enum types
  , MKFeatureVisibility(MKFeatureVisibility)
  , pattern MKFeatureVisibilityAdaptive
  , pattern MKFeatureVisibilityHidden
  , pattern MKFeatureVisibilityVisible

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- titleVisibility@
titleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO MKFeatureVisibility
titleVisibility mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView titleVisibilitySelector

-- | @- setTitleVisibility:@
setTitleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> MKFeatureVisibility -> IO ()
setTitleVisibility mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setTitleVisibilitySelector value

-- | @- subtitleVisibility@
subtitleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO MKFeatureVisibility
subtitleVisibility mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView subtitleVisibilitySelector

-- | @- setSubtitleVisibility:@
setSubtitleVisibility :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> MKFeatureVisibility -> IO ()
setSubtitleVisibility mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setSubtitleVisibilitySelector value

-- | @- markerTintColor@
markerTintColor :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSColor)
markerTintColor mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView markerTintColorSelector

-- | @- setMarkerTintColor:@
setMarkerTintColor :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSColor value) => mkMarkerAnnotationView -> value -> IO ()
setMarkerTintColor mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setMarkerTintColorSelector (toNSColor value)

-- | @- glyphTintColor@
glyphTintColor :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSColor)
glyphTintColor mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView glyphTintColorSelector

-- | @- setGlyphTintColor:@
setGlyphTintColor :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSColor value) => mkMarkerAnnotationView -> value -> IO ()
setGlyphTintColor mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setGlyphTintColorSelector (toNSColor value)

-- | @- glyphText@
glyphText :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSString)
glyphText mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView glyphTextSelector

-- | @- setGlyphText:@
setGlyphText :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSString value) => mkMarkerAnnotationView -> value -> IO ()
setGlyphText mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setGlyphTextSelector (toNSString value)

-- | @- glyphImage@
glyphImage :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSImage)
glyphImage mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView glyphImageSelector

-- | @- setGlyphImage:@
setGlyphImage :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSImage value) => mkMarkerAnnotationView -> value -> IO ()
setGlyphImage mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setGlyphImageSelector (toNSImage value)

-- | @- selectedGlyphImage@
selectedGlyphImage :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO (Id NSImage)
selectedGlyphImage mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView selectedGlyphImageSelector

-- | @- setSelectedGlyphImage:@
setSelectedGlyphImage :: (IsMKMarkerAnnotationView mkMarkerAnnotationView, IsNSImage value) => mkMarkerAnnotationView -> value -> IO ()
setSelectedGlyphImage mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setSelectedGlyphImageSelector (toNSImage value)

-- | @- animatesWhenAdded@
animatesWhenAdded :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> IO Bool
animatesWhenAdded mkMarkerAnnotationView =
  sendMessage mkMarkerAnnotationView animatesWhenAddedSelector

-- | @- setAnimatesWhenAdded:@
setAnimatesWhenAdded :: IsMKMarkerAnnotationView mkMarkerAnnotationView => mkMarkerAnnotationView -> Bool -> IO ()
setAnimatesWhenAdded mkMarkerAnnotationView value =
  sendMessage mkMarkerAnnotationView setAnimatesWhenAddedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @titleVisibility@
titleVisibilitySelector :: Selector '[] MKFeatureVisibility
titleVisibilitySelector = mkSelector "titleVisibility"

-- | @Selector@ for @setTitleVisibility:@
setTitleVisibilitySelector :: Selector '[MKFeatureVisibility] ()
setTitleVisibilitySelector = mkSelector "setTitleVisibility:"

-- | @Selector@ for @subtitleVisibility@
subtitleVisibilitySelector :: Selector '[] MKFeatureVisibility
subtitleVisibilitySelector = mkSelector "subtitleVisibility"

-- | @Selector@ for @setSubtitleVisibility:@
setSubtitleVisibilitySelector :: Selector '[MKFeatureVisibility] ()
setSubtitleVisibilitySelector = mkSelector "setSubtitleVisibility:"

-- | @Selector@ for @markerTintColor@
markerTintColorSelector :: Selector '[] (Id NSColor)
markerTintColorSelector = mkSelector "markerTintColor"

-- | @Selector@ for @setMarkerTintColor:@
setMarkerTintColorSelector :: Selector '[Id NSColor] ()
setMarkerTintColorSelector = mkSelector "setMarkerTintColor:"

-- | @Selector@ for @glyphTintColor@
glyphTintColorSelector :: Selector '[] (Id NSColor)
glyphTintColorSelector = mkSelector "glyphTintColor"

-- | @Selector@ for @setGlyphTintColor:@
setGlyphTintColorSelector :: Selector '[Id NSColor] ()
setGlyphTintColorSelector = mkSelector "setGlyphTintColor:"

-- | @Selector@ for @glyphText@
glyphTextSelector :: Selector '[] (Id NSString)
glyphTextSelector = mkSelector "glyphText"

-- | @Selector@ for @setGlyphText:@
setGlyphTextSelector :: Selector '[Id NSString] ()
setGlyphTextSelector = mkSelector "setGlyphText:"

-- | @Selector@ for @glyphImage@
glyphImageSelector :: Selector '[] (Id NSImage)
glyphImageSelector = mkSelector "glyphImage"

-- | @Selector@ for @setGlyphImage:@
setGlyphImageSelector :: Selector '[Id NSImage] ()
setGlyphImageSelector = mkSelector "setGlyphImage:"

-- | @Selector@ for @selectedGlyphImage@
selectedGlyphImageSelector :: Selector '[] (Id NSImage)
selectedGlyphImageSelector = mkSelector "selectedGlyphImage"

-- | @Selector@ for @setSelectedGlyphImage:@
setSelectedGlyphImageSelector :: Selector '[Id NSImage] ()
setSelectedGlyphImageSelector = mkSelector "setSelectedGlyphImage:"

-- | @Selector@ for @animatesWhenAdded@
animatesWhenAddedSelector :: Selector '[] Bool
animatesWhenAddedSelector = mkSelector "animatesWhenAdded"

-- | @Selector@ for @setAnimatesWhenAdded:@
setAnimatesWhenAddedSelector :: Selector '[Bool] ()
setAnimatesWhenAddedSelector = mkSelector "setAnimatesWhenAdded:"

