{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNText
--
-- SCNText represents a block of text that has been extruded
--
-- Generated bindings for @SCNText@.
module ObjC.SceneKit.SCNText
  ( SCNText
  , IsSCNText(..)
  , textWithString_extrusionDepth
  , extrusionDepth
  , setExtrusionDepth
  , string
  , setString
  , font
  , setFont
  , wrapped
  , setWrapped
  , truncationMode
  , setTruncationMode
  , alignmentMode
  , setAlignmentMode
  , chamferRadius
  , setChamferRadius
  , chamferSegmentCount
  , setChamferSegmentCount
  , chamferProfile
  , setChamferProfile
  , flatness
  , setFlatness
  , alignmentModeSelector
  , chamferProfileSelector
  , chamferRadiusSelector
  , chamferSegmentCountSelector
  , extrusionDepthSelector
  , flatnessSelector
  , fontSelector
  , setAlignmentModeSelector
  , setChamferProfileSelector
  , setChamferRadiusSelector
  , setChamferSegmentCountSelector
  , setExtrusionDepthSelector
  , setFlatnessSelector
  , setFontSelector
  , setStringSelector
  , setTruncationModeSelector
  , setWrappedSelector
  , stringSelector
  , textWithString_extrusionDepthSelector
  , truncationModeSelector
  , wrappedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | textWithString:extrusionDepth:
--
-- Creates and returns a 3D representation of given text with given extrusion depth.
--
-- @string@ — The text to be represented.
--
-- @extrusionDepth@ — The extrusion depth.
--
-- ObjC selector: @+ textWithString:extrusionDepth:@
textWithString_extrusionDepth :: RawId -> CDouble -> IO (Id SCNText)
textWithString_extrusionDepth string extrusionDepth =
  do
    cls' <- getRequiredClass "SCNText"
    sendClassMessage cls' textWithString_extrusionDepthSelector string extrusionDepth

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the text.
--
-- ObjC selector: @- extrusionDepth@
extrusionDepth :: IsSCNText scnText => scnText -> IO CDouble
extrusionDepth scnText =
  sendMessage scnText extrusionDepthSelector

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the text.
--
-- ObjC selector: @- setExtrusionDepth:@
setExtrusionDepth :: IsSCNText scnText => scnText -> CDouble -> IO ()
setExtrusionDepth scnText value =
  sendMessage scnText setExtrusionDepthSelector value

-- | string
--
-- The text to be represented. The text must be an instance of NSString or NSAttributedString.
--
-- The default value is nil.
--
-- ObjC selector: @- string@
string :: IsSCNText scnText => scnText -> IO RawId
string scnText =
  sendMessage scnText stringSelector

-- | string
--
-- The text to be represented. The text must be an instance of NSString or NSAttributedString.
--
-- The default value is nil.
--
-- ObjC selector: @- setString:@
setString :: IsSCNText scnText => scnText -> RawId -> IO ()
setString scnText value =
  sendMessage scnText setStringSelector value

-- | font
--
-- The font used to represent the text.
--
-- The font property is only used when the string property is not an NSAttributedString. Defaults to the system font (12 point).
--
-- ObjC selector: @- font@
font :: IsSCNText scnText => scnText -> IO (Id NSFont)
font scnText =
  sendMessage scnText fontSelector

-- | font
--
-- The font used to represent the text.
--
-- The font property is only used when the string property is not an NSAttributedString. Defaults to the system font (12 point).
--
-- ObjC selector: @- setFont:@
setFont :: (IsSCNText scnText, IsNSFont value) => scnText -> value -> IO ()
setFont scnText value =
  sendMessage scnText setFontSelector (toNSFont value)

-- | wrapped
--
-- Determines whether the text is wrapped to fit within the bounds.
--
-- For the text to be wrapped you first need to set its bounds, otherwise the text is not wrapped. The default value is NO.
--
-- ObjC selector: @- wrapped@
wrapped :: IsSCNText scnText => scnText -> IO Bool
wrapped scnText =
  sendMessage scnText wrappedSelector

-- | wrapped
--
-- Determines whether the text is wrapped to fit within the bounds.
--
-- For the text to be wrapped you first need to set its bounds, otherwise the text is not wrapped. The default value is NO.
--
-- ObjC selector: @- setWrapped:@
setWrapped :: IsSCNText scnText => scnText -> Bool -> IO ()
setWrapped scnText value =
  sendMessage scnText setWrappedSelector value

-- | truncationMode
--
-- Describes how the text is truncated to fit within the bounds.
--
-- For the text to be truncated you first need to set its bounds, otherwise the text is not truncated. The default value is kCATruncationNone. See truncation modes in CATextLayer.h.
--
-- ObjC selector: @- truncationMode@
truncationMode :: IsSCNText scnText => scnText -> IO (Id NSString)
truncationMode scnText =
  sendMessage scnText truncationModeSelector

-- | truncationMode
--
-- Describes how the text is truncated to fit within the bounds.
--
-- For the text to be truncated you first need to set its bounds, otherwise the text is not truncated. The default value is kCATruncationNone. See truncation modes in CATextLayer.h.
--
-- ObjC selector: @- setTruncationMode:@
setTruncationMode :: (IsSCNText scnText, IsNSString value) => scnText -> value -> IO ()
setTruncationMode scnText value =
  sendMessage scnText setTruncationModeSelector (toNSString value)

-- | alignmentMode
--
-- Determines how individual lines of text are horizontally aligned within the bounds.
--
-- For the text to be aligned you first need to set its bounds, otherwise the text is not aligned. The default value is kCAAlignmentNatural. See alignments in CATextLayer.h.
--
-- ObjC selector: @- alignmentMode@
alignmentMode :: IsSCNText scnText => scnText -> IO (Id NSString)
alignmentMode scnText =
  sendMessage scnText alignmentModeSelector

-- | alignmentMode
--
-- Determines how individual lines of text are horizontally aligned within the bounds.
--
-- For the text to be aligned you first need to set its bounds, otherwise the text is not aligned. The default value is kCAAlignmentNatural. See alignments in CATextLayer.h.
--
-- ObjC selector: @- setAlignmentMode:@
setAlignmentMode :: (IsSCNText scnText, IsNSString value) => scnText -> value -> IO ()
setAlignmentMode scnText value =
  sendMessage scnText setAlignmentModeSelector (toNSString value)

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The actual chamfer radius might be different to the one here specified: large values are clipped to a per-glyph max value. The default value is 0.
--
-- ObjC selector: @- chamferRadius@
chamferRadius :: IsSCNText scnText => scnText -> IO CDouble
chamferRadius scnText =
  sendMessage scnText chamferRadiusSelector

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The actual chamfer radius might be different to the one here specified: large values are clipped to a per-glyph max value. The default value is 0.
--
-- ObjC selector: @- setChamferRadius:@
setChamferRadius :: IsSCNText scnText => scnText -> CDouble -> IO ()
setChamferRadius scnText value =
  sendMessage scnText setChamferRadiusSelector value

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- chamferSegmentCount@
chamferSegmentCount :: IsSCNText scnText => scnText -> IO CLong
chamferSegmentCount scnText =
  sendMessage scnText chamferSegmentCountSelector

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- setChamferSegmentCount:@
setChamferSegmentCount :: IsSCNText scnText => scnText -> CLong -> IO ()
setChamferSegmentCount scnText value =
  sendMessage scnText setChamferSegmentCountSelector value

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- chamferProfile@
chamferProfile :: IsSCNText scnText => scnText -> IO (Id NSBezierPath)
chamferProfile scnText =
  sendMessage scnText chamferProfileSelector

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- setChamferProfile:@
setChamferProfile :: (IsSCNText scnText, IsNSBezierPath value) => scnText -> value -> IO ()
setChamferProfile scnText value =
  sendMessage scnText setChamferProfileSelector (toNSBezierPath value)

-- | flatness
--
-- Specifies the accuracy (or smoothness) with which fonts are rendered.
--
-- Smaller numbers give smoother curves at the expense of more computation and heavier geometries in terms of vertices. The default value is 0.6, which yields smooth curves.
--
-- ObjC selector: @- flatness@
flatness :: IsSCNText scnText => scnText -> IO CDouble
flatness scnText =
  sendMessage scnText flatnessSelector

-- | flatness
--
-- Specifies the accuracy (or smoothness) with which fonts are rendered.
--
-- Smaller numbers give smoother curves at the expense of more computation and heavier geometries in terms of vertices. The default value is 0.6, which yields smooth curves.
--
-- ObjC selector: @- setFlatness:@
setFlatness :: IsSCNText scnText => scnText -> CDouble -> IO ()
setFlatness scnText value =
  sendMessage scnText setFlatnessSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textWithString:extrusionDepth:@
textWithString_extrusionDepthSelector :: Selector '[RawId, CDouble] (Id SCNText)
textWithString_extrusionDepthSelector = mkSelector "textWithString:extrusionDepth:"

-- | @Selector@ for @extrusionDepth@
extrusionDepthSelector :: Selector '[] CDouble
extrusionDepthSelector = mkSelector "extrusionDepth"

-- | @Selector@ for @setExtrusionDepth:@
setExtrusionDepthSelector :: Selector '[CDouble] ()
setExtrusionDepthSelector = mkSelector "setExtrusionDepth:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] RawId
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[RawId] ()
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @wrapped@
wrappedSelector :: Selector '[] Bool
wrappedSelector = mkSelector "wrapped"

-- | @Selector@ for @setWrapped:@
setWrappedSelector :: Selector '[Bool] ()
setWrappedSelector = mkSelector "setWrapped:"

-- | @Selector@ for @truncationMode@
truncationModeSelector :: Selector '[] (Id NSString)
truncationModeSelector = mkSelector "truncationMode"

-- | @Selector@ for @setTruncationMode:@
setTruncationModeSelector :: Selector '[Id NSString] ()
setTruncationModeSelector = mkSelector "setTruncationMode:"

-- | @Selector@ for @alignmentMode@
alignmentModeSelector :: Selector '[] (Id NSString)
alignmentModeSelector = mkSelector "alignmentMode"

-- | @Selector@ for @setAlignmentMode:@
setAlignmentModeSelector :: Selector '[Id NSString] ()
setAlignmentModeSelector = mkSelector "setAlignmentMode:"

-- | @Selector@ for @chamferRadius@
chamferRadiusSelector :: Selector '[] CDouble
chamferRadiusSelector = mkSelector "chamferRadius"

-- | @Selector@ for @setChamferRadius:@
setChamferRadiusSelector :: Selector '[CDouble] ()
setChamferRadiusSelector = mkSelector "setChamferRadius:"

-- | @Selector@ for @chamferSegmentCount@
chamferSegmentCountSelector :: Selector '[] CLong
chamferSegmentCountSelector = mkSelector "chamferSegmentCount"

-- | @Selector@ for @setChamferSegmentCount:@
setChamferSegmentCountSelector :: Selector '[CLong] ()
setChamferSegmentCountSelector = mkSelector "setChamferSegmentCount:"

-- | @Selector@ for @chamferProfile@
chamferProfileSelector :: Selector '[] (Id NSBezierPath)
chamferProfileSelector = mkSelector "chamferProfile"

-- | @Selector@ for @setChamferProfile:@
setChamferProfileSelector :: Selector '[Id NSBezierPath] ()
setChamferProfileSelector = mkSelector "setChamferProfile:"

-- | @Selector@ for @flatness@
flatnessSelector :: Selector '[] CDouble
flatnessSelector = mkSelector "flatness"

-- | @Selector@ for @setFlatness:@
setFlatnessSelector :: Selector '[CDouble] ()
setFlatnessSelector = mkSelector "setFlatness:"

