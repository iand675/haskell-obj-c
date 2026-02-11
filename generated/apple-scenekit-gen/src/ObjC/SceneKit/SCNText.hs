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
  , textWithString_extrusionDepthSelector
  , extrusionDepthSelector
  , setExtrusionDepthSelector
  , stringSelector
  , setStringSelector
  , fontSelector
  , setFontSelector
  , wrappedSelector
  , setWrappedSelector
  , truncationModeSelector
  , setTruncationModeSelector
  , alignmentModeSelector
  , setAlignmentModeSelector
  , chamferRadiusSelector
  , setChamferRadiusSelector
  , chamferSegmentCountSelector
  , setChamferSegmentCountSelector
  , chamferProfileSelector
  , setChamferProfileSelector
  , flatnessSelector
  , setFlatnessSelector


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
    sendClassMsg cls' (mkSelector "textWithString:extrusionDepth:") (retPtr retVoid) [argPtr (castPtr (unRawId string) :: Ptr ()), argCDouble extrusionDepth] >>= retainedObject . castPtr

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the text.
--
-- ObjC selector: @- extrusionDepth@
extrusionDepth :: IsSCNText scnText => scnText -> IO CDouble
extrusionDepth scnText  =
    sendMsg scnText (mkSelector "extrusionDepth") retCDouble []

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the text.
--
-- ObjC selector: @- setExtrusionDepth:@
setExtrusionDepth :: IsSCNText scnText => scnText -> CDouble -> IO ()
setExtrusionDepth scnText  value =
    sendMsg scnText (mkSelector "setExtrusionDepth:") retVoid [argCDouble value]

-- | string
--
-- The text to be represented. The text must be an instance of NSString or NSAttributedString.
--
-- The default value is nil.
--
-- ObjC selector: @- string@
string :: IsSCNText scnText => scnText -> IO RawId
string scnText  =
    fmap (RawId . castPtr) $ sendMsg scnText (mkSelector "string") (retPtr retVoid) []

-- | string
--
-- The text to be represented. The text must be an instance of NSString or NSAttributedString.
--
-- The default value is nil.
--
-- ObjC selector: @- setString:@
setString :: IsSCNText scnText => scnText -> RawId -> IO ()
setString scnText  value =
    sendMsg scnText (mkSelector "setString:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | font
--
-- The font used to represent the text.
--
-- The font property is only used when the string property is not an NSAttributedString. Defaults to the system font (12 point).
--
-- ObjC selector: @- font@
font :: IsSCNText scnText => scnText -> IO (Id NSFont)
font scnText  =
    sendMsg scnText (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | font
--
-- The font used to represent the text.
--
-- The font property is only used when the string property is not an NSAttributedString. Defaults to the system font (12 point).
--
-- ObjC selector: @- setFont:@
setFont :: (IsSCNText scnText, IsNSFont value) => scnText -> value -> IO ()
setFont scnText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnText (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | wrapped
--
-- Determines whether the text is wrapped to fit within the bounds.
--
-- For the text to be wrapped you first need to set its bounds, otherwise the text is not wrapped. The default value is NO.
--
-- ObjC selector: @- wrapped@
wrapped :: IsSCNText scnText => scnText -> IO Bool
wrapped scnText  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnText (mkSelector "wrapped") retCULong []

-- | wrapped
--
-- Determines whether the text is wrapped to fit within the bounds.
--
-- For the text to be wrapped you first need to set its bounds, otherwise the text is not wrapped. The default value is NO.
--
-- ObjC selector: @- setWrapped:@
setWrapped :: IsSCNText scnText => scnText -> Bool -> IO ()
setWrapped scnText  value =
    sendMsg scnText (mkSelector "setWrapped:") retVoid [argCULong (if value then 1 else 0)]

-- | truncationMode
--
-- Describes how the text is truncated to fit within the bounds.
--
-- For the text to be truncated you first need to set its bounds, otherwise the text is not truncated. The default value is kCATruncationNone. See truncation modes in CATextLayer.h.
--
-- ObjC selector: @- truncationMode@
truncationMode :: IsSCNText scnText => scnText -> IO (Id NSString)
truncationMode scnText  =
    sendMsg scnText (mkSelector "truncationMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | truncationMode
--
-- Describes how the text is truncated to fit within the bounds.
--
-- For the text to be truncated you first need to set its bounds, otherwise the text is not truncated. The default value is kCATruncationNone. See truncation modes in CATextLayer.h.
--
-- ObjC selector: @- setTruncationMode:@
setTruncationMode :: (IsSCNText scnText, IsNSString value) => scnText -> value -> IO ()
setTruncationMode scnText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnText (mkSelector "setTruncationMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | alignmentMode
--
-- Determines how individual lines of text are horizontally aligned within the bounds.
--
-- For the text to be aligned you first need to set its bounds, otherwise the text is not aligned. The default value is kCAAlignmentNatural. See alignments in CATextLayer.h.
--
-- ObjC selector: @- alignmentMode@
alignmentMode :: IsSCNText scnText => scnText -> IO (Id NSString)
alignmentMode scnText  =
    sendMsg scnText (mkSelector "alignmentMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | alignmentMode
--
-- Determines how individual lines of text are horizontally aligned within the bounds.
--
-- For the text to be aligned you first need to set its bounds, otherwise the text is not aligned. The default value is kCAAlignmentNatural. See alignments in CATextLayer.h.
--
-- ObjC selector: @- setAlignmentMode:@
setAlignmentMode :: (IsSCNText scnText, IsNSString value) => scnText -> value -> IO ()
setAlignmentMode scnText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnText (mkSelector "setAlignmentMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The actual chamfer radius might be different to the one here specified: large values are clipped to a per-glyph max value. The default value is 0.
--
-- ObjC selector: @- chamferRadius@
chamferRadius :: IsSCNText scnText => scnText -> IO CDouble
chamferRadius scnText  =
    sendMsg scnText (mkSelector "chamferRadius") retCDouble []

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The actual chamfer radius might be different to the one here specified: large values are clipped to a per-glyph max value. The default value is 0.
--
-- ObjC selector: @- setChamferRadius:@
setChamferRadius :: IsSCNText scnText => scnText -> CDouble -> IO ()
setChamferRadius scnText  value =
    sendMsg scnText (mkSelector "setChamferRadius:") retVoid [argCDouble value]

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- chamferSegmentCount@
chamferSegmentCount :: IsSCNText scnText => scnText -> IO CLong
chamferSegmentCount scnText  =
    sendMsg scnText (mkSelector "chamferSegmentCount") retCLong []

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- setChamferSegmentCount:@
setChamferSegmentCount :: IsSCNText scnText => scnText -> CLong -> IO ()
setChamferSegmentCount scnText  value =
    sendMsg scnText (mkSelector "setChamferSegmentCount:") retVoid [argCLong value]

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- chamferProfile@
chamferProfile :: IsSCNText scnText => scnText -> IO (Id NSBezierPath)
chamferProfile scnText  =
    sendMsg scnText (mkSelector "chamferProfile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- setChamferProfile:@
setChamferProfile :: (IsSCNText scnText, IsNSBezierPath value) => scnText -> value -> IO ()
setChamferProfile scnText  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnText (mkSelector "setChamferProfile:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | flatness
--
-- Specifies the accuracy (or smoothness) with which fonts are rendered.
--
-- Smaller numbers give smoother curves at the expense of more computation and heavier geometries in terms of vertices. The default value is 0.6, which yields smooth curves.
--
-- ObjC selector: @- flatness@
flatness :: IsSCNText scnText => scnText -> IO CDouble
flatness scnText  =
    sendMsg scnText (mkSelector "flatness") retCDouble []

-- | flatness
--
-- Specifies the accuracy (or smoothness) with which fonts are rendered.
--
-- Smaller numbers give smoother curves at the expense of more computation and heavier geometries in terms of vertices. The default value is 0.6, which yields smooth curves.
--
-- ObjC selector: @- setFlatness:@
setFlatness :: IsSCNText scnText => scnText -> CDouble -> IO ()
setFlatness scnText  value =
    sendMsg scnText (mkSelector "setFlatness:") retVoid [argCDouble value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textWithString:extrusionDepth:@
textWithString_extrusionDepthSelector :: Selector
textWithString_extrusionDepthSelector = mkSelector "textWithString:extrusionDepth:"

-- | @Selector@ for @extrusionDepth@
extrusionDepthSelector :: Selector
extrusionDepthSelector = mkSelector "extrusionDepth"

-- | @Selector@ for @setExtrusionDepth:@
setExtrusionDepthSelector :: Selector
setExtrusionDepthSelector = mkSelector "setExtrusionDepth:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @wrapped@
wrappedSelector :: Selector
wrappedSelector = mkSelector "wrapped"

-- | @Selector@ for @setWrapped:@
setWrappedSelector :: Selector
setWrappedSelector = mkSelector "setWrapped:"

-- | @Selector@ for @truncationMode@
truncationModeSelector :: Selector
truncationModeSelector = mkSelector "truncationMode"

-- | @Selector@ for @setTruncationMode:@
setTruncationModeSelector :: Selector
setTruncationModeSelector = mkSelector "setTruncationMode:"

-- | @Selector@ for @alignmentMode@
alignmentModeSelector :: Selector
alignmentModeSelector = mkSelector "alignmentMode"

-- | @Selector@ for @setAlignmentMode:@
setAlignmentModeSelector :: Selector
setAlignmentModeSelector = mkSelector "setAlignmentMode:"

-- | @Selector@ for @chamferRadius@
chamferRadiusSelector :: Selector
chamferRadiusSelector = mkSelector "chamferRadius"

-- | @Selector@ for @setChamferRadius:@
setChamferRadiusSelector :: Selector
setChamferRadiusSelector = mkSelector "setChamferRadius:"

-- | @Selector@ for @chamferSegmentCount@
chamferSegmentCountSelector :: Selector
chamferSegmentCountSelector = mkSelector "chamferSegmentCount"

-- | @Selector@ for @setChamferSegmentCount:@
setChamferSegmentCountSelector :: Selector
setChamferSegmentCountSelector = mkSelector "setChamferSegmentCount:"

-- | @Selector@ for @chamferProfile@
chamferProfileSelector :: Selector
chamferProfileSelector = mkSelector "chamferProfile"

-- | @Selector@ for @setChamferProfile:@
setChamferProfileSelector :: Selector
setChamferProfileSelector = mkSelector "setChamferProfile:"

-- | @Selector@ for @flatness@
flatnessSelector :: Selector
flatnessSelector = mkSelector "flatness"

-- | @Selector@ for @setFlatness:@
setFlatnessSelector :: Selector
setFlatnessSelector = mkSelector "setFlatness:"

