{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBezierPath@.
module ObjC.AppKit.NSBezierPath
  ( NSBezierPath
  , IsNSBezierPath(..)
  , bezierPath
  , bezierPathWithRect
  , bezierPathWithOvalInRect
  , bezierPathWithRoundedRect_xRadius_yRadius
  , bezierPathWithCGPath
  , fillRect
  , strokeRect
  , clipRect
  , strokeLineFromPoint_toPoint
  , drawPackedGlyphs_atPoint
  , moveToPoint
  , lineToPoint
  , curveToPoint_controlPoint1_controlPoint2
  , curveToPoint_controlPoint
  , closePath
  , removeAllPoints
  , relativeMoveToPoint
  , relativeLineToPoint
  , relativeCurveToPoint_controlPoint1_controlPoint2
  , relativeCurveToPoint_controlPoint
  , getLineDash_count_phase
  , setLineDash_count_phase
  , stroke
  , fill
  , addClip
  , setClip
  , transformUsingAffineTransform
  , elementAtIndex_associatedPoints
  , elementAtIndex
  , setAssociatedPoints_atIndex
  , appendBezierPath
  , appendBezierPathWithRect
  , appendBezierPathWithPoints_count
  , appendBezierPathWithOvalInRect
  , appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise
  , appendBezierPathWithArcWithCenter_radius_startAngle_endAngle
  , appendBezierPathWithArcFromPoint_toPoint_radius
  , appendBezierPathWithCGGlyph_inFont
  , appendBezierPathWithCGGlyphs_count_inFont
  , appendBezierPathWithRoundedRect_xRadius_yRadius
  , containsPoint
  , cachesBezierPath
  , setCachesBezierPath
  , appendBezierPathWithGlyph_inFont
  , appendBezierPathWithGlyphs_count_inFont
  , appendBezierPathWithPackedGlyphs
  , cgPath
  , setCGPath
  , defaultMiterLimit
  , setDefaultMiterLimit
  , defaultFlatness
  , setDefaultFlatness
  , defaultWindingRule
  , setDefaultWindingRule
  , defaultLineCapStyle
  , setDefaultLineCapStyle
  , defaultLineJoinStyle
  , setDefaultLineJoinStyle
  , defaultLineWidth
  , setDefaultLineWidth
  , lineWidth
  , setLineWidth
  , lineCapStyle
  , setLineCapStyle
  , lineJoinStyle
  , setLineJoinStyle
  , windingRule
  , setWindingRule
  , miterLimit
  , setMiterLimit
  , flatness
  , setFlatness
  , bezierPathByFlatteningPath
  , bezierPathByReversingPath
  , empty
  , currentPoint
  , controlPointBounds
  , bounds
  , elementCount
  , addClipSelector
  , appendBezierPathSelector
  , appendBezierPathWithArcFromPoint_toPoint_radiusSelector
  , appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector
  , appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector
  , appendBezierPathWithCGGlyph_inFontSelector
  , appendBezierPathWithCGGlyphs_count_inFontSelector
  , appendBezierPathWithGlyph_inFontSelector
  , appendBezierPathWithGlyphs_count_inFontSelector
  , appendBezierPathWithOvalInRectSelector
  , appendBezierPathWithPackedGlyphsSelector
  , appendBezierPathWithPoints_countSelector
  , appendBezierPathWithRectSelector
  , appendBezierPathWithRoundedRect_xRadius_yRadiusSelector
  , bezierPathByFlatteningPathSelector
  , bezierPathByReversingPathSelector
  , bezierPathSelector
  , bezierPathWithCGPathSelector
  , bezierPathWithOvalInRectSelector
  , bezierPathWithRectSelector
  , bezierPathWithRoundedRect_xRadius_yRadiusSelector
  , boundsSelector
  , cachesBezierPathSelector
  , cgPathSelector
  , clipRectSelector
  , closePathSelector
  , containsPointSelector
  , controlPointBoundsSelector
  , currentPointSelector
  , curveToPoint_controlPoint1_controlPoint2Selector
  , curveToPoint_controlPointSelector
  , defaultFlatnessSelector
  , defaultLineCapStyleSelector
  , defaultLineJoinStyleSelector
  , defaultLineWidthSelector
  , defaultMiterLimitSelector
  , defaultWindingRuleSelector
  , drawPackedGlyphs_atPointSelector
  , elementAtIndexSelector
  , elementAtIndex_associatedPointsSelector
  , elementCountSelector
  , emptySelector
  , fillRectSelector
  , fillSelector
  , flatnessSelector
  , getLineDash_count_phaseSelector
  , lineCapStyleSelector
  , lineJoinStyleSelector
  , lineToPointSelector
  , lineWidthSelector
  , miterLimitSelector
  , moveToPointSelector
  , relativeCurveToPoint_controlPoint1_controlPoint2Selector
  , relativeCurveToPoint_controlPointSelector
  , relativeLineToPointSelector
  , relativeMoveToPointSelector
  , removeAllPointsSelector
  , setAssociatedPoints_atIndexSelector
  , setCGPathSelector
  , setCachesBezierPathSelector
  , setClipSelector
  , setDefaultFlatnessSelector
  , setDefaultLineCapStyleSelector
  , setDefaultLineJoinStyleSelector
  , setDefaultLineWidthSelector
  , setDefaultMiterLimitSelector
  , setDefaultWindingRuleSelector
  , setFlatnessSelector
  , setLineCapStyleSelector
  , setLineDash_count_phaseSelector
  , setLineJoinStyleSelector
  , setLineWidthSelector
  , setMiterLimitSelector
  , setWindingRuleSelector
  , strokeLineFromPoint_toPointSelector
  , strokeRectSelector
  , strokeSelector
  , transformUsingAffineTransformSelector
  , windingRuleSelector

  -- * Enum types
  , NSBezierPathElement(NSBezierPathElement)
  , pattern NSBezierPathElementMoveTo
  , pattern NSBezierPathElementLineTo
  , pattern NSBezierPathElementCubicCurveTo
  , pattern NSBezierPathElementClosePath
  , pattern NSBezierPathElementQuadraticCurveTo
  , pattern NSBezierPathElementCurveTo
  , NSLineCapStyle(NSLineCapStyle)
  , pattern NSLineCapStyleButt
  , pattern NSLineCapStyleRound
  , pattern NSLineCapStyleSquare
  , NSLineJoinStyle(NSLineJoinStyle)
  , pattern NSLineJoinStyleMiter
  , pattern NSLineJoinStyleRound
  , pattern NSLineJoinStyleBevel
  , NSWindingRule(NSWindingRule)
  , pattern NSWindingRuleNonZero
  , pattern NSWindingRuleEvenOdd

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ bezierPath@
bezierPath :: IO (Id NSBezierPath)
bezierPath  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' bezierPathSelector

-- | @+ bezierPathWithRect:@
bezierPathWithRect :: NSRect -> IO (Id NSBezierPath)
bezierPathWithRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' bezierPathWithRectSelector rect

-- | @+ bezierPathWithOvalInRect:@
bezierPathWithOvalInRect :: NSRect -> IO (Id NSBezierPath)
bezierPathWithOvalInRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' bezierPathWithOvalInRectSelector rect

-- | @+ bezierPathWithRoundedRect:xRadius:yRadius:@
bezierPathWithRoundedRect_xRadius_yRadius :: NSRect -> CDouble -> CDouble -> IO (Id NSBezierPath)
bezierPathWithRoundedRect_xRadius_yRadius rect xRadius yRadius =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' bezierPathWithRoundedRect_xRadius_yRadiusSelector rect xRadius yRadius

-- | @+ bezierPathWithCGPath:@
bezierPathWithCGPath :: RawId -> IO (Id NSBezierPath)
bezierPathWithCGPath cgPath =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' bezierPathWithCGPathSelector cgPath

-- | @+ fillRect:@
fillRect :: NSRect -> IO ()
fillRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' fillRectSelector rect

-- | @+ strokeRect:@
strokeRect :: NSRect -> IO ()
strokeRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' strokeRectSelector rect

-- | @+ clipRect:@
clipRect :: NSRect -> IO ()
clipRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' clipRectSelector rect

-- | @+ strokeLineFromPoint:toPoint:@
strokeLineFromPoint_toPoint :: NSPoint -> NSPoint -> IO ()
strokeLineFromPoint_toPoint point1 point2 =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' strokeLineFromPoint_toPointSelector point1 point2

-- | @+ drawPackedGlyphs:atPoint:@
drawPackedGlyphs_atPoint :: Const (Ptr CChar) -> NSPoint -> IO ()
drawPackedGlyphs_atPoint packedGlyphs point =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' drawPackedGlyphs_atPointSelector packedGlyphs point

-- | @- moveToPoint:@
moveToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
moveToPoint nsBezierPath point =
  sendMessage nsBezierPath moveToPointSelector point

-- | @- lineToPoint:@
lineToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
lineToPoint nsBezierPath point =
  sendMessage nsBezierPath lineToPointSelector point

-- | @- curveToPoint:controlPoint1:controlPoint2:@
curveToPoint_controlPoint1_controlPoint2 :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> NSPoint -> IO ()
curveToPoint_controlPoint1_controlPoint2 nsBezierPath endPoint controlPoint1 controlPoint2 =
  sendMessage nsBezierPath curveToPoint_controlPoint1_controlPoint2Selector endPoint controlPoint1 controlPoint2

-- | @- curveToPoint:controlPoint:@
curveToPoint_controlPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> IO ()
curveToPoint_controlPoint nsBezierPath endPoint controlPoint =
  sendMessage nsBezierPath curveToPoint_controlPointSelector endPoint controlPoint

-- | @- closePath@
closePath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
closePath nsBezierPath =
  sendMessage nsBezierPath closePathSelector

-- | @- removeAllPoints@
removeAllPoints :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
removeAllPoints nsBezierPath =
  sendMessage nsBezierPath removeAllPointsSelector

-- | @- relativeMoveToPoint:@
relativeMoveToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
relativeMoveToPoint nsBezierPath point =
  sendMessage nsBezierPath relativeMoveToPointSelector point

-- | @- relativeLineToPoint:@
relativeLineToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
relativeLineToPoint nsBezierPath point =
  sendMessage nsBezierPath relativeLineToPointSelector point

-- | @- relativeCurveToPoint:controlPoint1:controlPoint2:@
relativeCurveToPoint_controlPoint1_controlPoint2 :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> NSPoint -> IO ()
relativeCurveToPoint_controlPoint1_controlPoint2 nsBezierPath endPoint controlPoint1 controlPoint2 =
  sendMessage nsBezierPath relativeCurveToPoint_controlPoint1_controlPoint2Selector endPoint controlPoint1 controlPoint2

-- | @- relativeCurveToPoint:controlPoint:@
relativeCurveToPoint_controlPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> IO ()
relativeCurveToPoint_controlPoint nsBezierPath endPoint controlPoint =
  sendMessage nsBezierPath relativeCurveToPoint_controlPointSelector endPoint controlPoint

-- | @- getLineDash:count:phase:@
getLineDash_count_phase :: IsNSBezierPath nsBezierPath => nsBezierPath -> Ptr CDouble -> Ptr CLong -> Ptr CDouble -> IO ()
getLineDash_count_phase nsBezierPath pattern_ count phase =
  sendMessage nsBezierPath getLineDash_count_phaseSelector pattern_ count phase

-- | @- setLineDash:count:phase:@
setLineDash_count_phase :: IsNSBezierPath nsBezierPath => nsBezierPath -> Const (Ptr CDouble) -> CLong -> CDouble -> IO ()
setLineDash_count_phase nsBezierPath pattern_ count phase =
  sendMessage nsBezierPath setLineDash_count_phaseSelector pattern_ count phase

-- | @- stroke@
stroke :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
stroke nsBezierPath =
  sendMessage nsBezierPath strokeSelector

-- | @- fill@
fill :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
fill nsBezierPath =
  sendMessage nsBezierPath fillSelector

-- | @- addClip@
addClip :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
addClip nsBezierPath =
  sendMessage nsBezierPath addClipSelector

-- | @- setClip@
setClip :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
setClip nsBezierPath =
  sendMessage nsBezierPath setClipSelector

-- | @- transformUsingAffineTransform:@
transformUsingAffineTransform :: (IsNSBezierPath nsBezierPath, IsNSAffineTransform transform) => nsBezierPath -> transform -> IO ()
transformUsingAffineTransform nsBezierPath transform =
  sendMessage nsBezierPath transformUsingAffineTransformSelector (toNSAffineTransform transform)

-- | @- elementAtIndex:associatedPoints:@
elementAtIndex_associatedPoints :: IsNSBezierPath nsBezierPath => nsBezierPath -> CLong -> Ptr NSPoint -> IO NSBezierPathElement
elementAtIndex_associatedPoints nsBezierPath index points =
  sendMessage nsBezierPath elementAtIndex_associatedPointsSelector index points

-- | @- elementAtIndex:@
elementAtIndex :: IsNSBezierPath nsBezierPath => nsBezierPath -> CLong -> IO NSBezierPathElement
elementAtIndex nsBezierPath index =
  sendMessage nsBezierPath elementAtIndexSelector index

-- | @- setAssociatedPoints:atIndex:@
setAssociatedPoints_atIndex :: IsNSBezierPath nsBezierPath => nsBezierPath -> Ptr NSPoint -> CLong -> IO ()
setAssociatedPoints_atIndex nsBezierPath points index =
  sendMessage nsBezierPath setAssociatedPoints_atIndexSelector points index

-- | @- appendBezierPath:@
appendBezierPath :: (IsNSBezierPath nsBezierPath, IsNSBezierPath path) => nsBezierPath -> path -> IO ()
appendBezierPath nsBezierPath path =
  sendMessage nsBezierPath appendBezierPathSelector (toNSBezierPath path)

-- | @- appendBezierPathWithRect:@
appendBezierPathWithRect :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSRect -> IO ()
appendBezierPathWithRect nsBezierPath rect =
  sendMessage nsBezierPath appendBezierPathWithRectSelector rect

-- | @- appendBezierPathWithPoints:count:@
appendBezierPathWithPoints_count :: IsNSBezierPath nsBezierPath => nsBezierPath -> Ptr NSPoint -> CLong -> IO ()
appendBezierPathWithPoints_count nsBezierPath points count =
  sendMessage nsBezierPath appendBezierPathWithPoints_countSelector points count

-- | @- appendBezierPathWithOvalInRect:@
appendBezierPathWithOvalInRect :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSRect -> IO ()
appendBezierPathWithOvalInRect nsBezierPath rect =
  sendMessage nsBezierPath appendBezierPathWithOvalInRectSelector rect

-- | @- appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> CDouble -> CDouble -> CDouble -> Bool -> IO ()
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise nsBezierPath center radius startAngle endAngle clockwise =
  sendMessage nsBezierPath appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector center radius startAngle endAngle clockwise

-- | @- appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> CDouble -> CDouble -> CDouble -> IO ()
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle nsBezierPath center radius startAngle endAngle =
  sendMessage nsBezierPath appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector center radius startAngle endAngle

-- | @- appendBezierPathWithArcFromPoint:toPoint:radius:@
appendBezierPathWithArcFromPoint_toPoint_radius :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> CDouble -> IO ()
appendBezierPathWithArcFromPoint_toPoint_radius nsBezierPath point1 point2 radius =
  sendMessage nsBezierPath appendBezierPathWithArcFromPoint_toPoint_radiusSelector point1 point2 radius

-- | @- appendBezierPathWithCGGlyph:inFont:@
appendBezierPathWithCGGlyph_inFont :: (IsNSBezierPath nsBezierPath, IsNSFont font) => nsBezierPath -> CUShort -> font -> IO ()
appendBezierPathWithCGGlyph_inFont nsBezierPath glyph font =
  sendMessage nsBezierPath appendBezierPathWithCGGlyph_inFontSelector glyph (toNSFont font)

-- | @- appendBezierPathWithCGGlyphs:count:inFont:@
appendBezierPathWithCGGlyphs_count_inFont :: (IsNSBezierPath nsBezierPath, IsNSFont font) => nsBezierPath -> Const RawId -> CLong -> font -> IO ()
appendBezierPathWithCGGlyphs_count_inFont nsBezierPath glyphs count font =
  sendMessage nsBezierPath appendBezierPathWithCGGlyphs_count_inFontSelector glyphs count (toNSFont font)

-- | @- appendBezierPathWithRoundedRect:xRadius:yRadius:@
appendBezierPathWithRoundedRect_xRadius_yRadius :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSRect -> CDouble -> CDouble -> IO ()
appendBezierPathWithRoundedRect_xRadius_yRadius nsBezierPath rect xRadius yRadius =
  sendMessage nsBezierPath appendBezierPathWithRoundedRect_xRadius_yRadiusSelector rect xRadius yRadius

-- | @- containsPoint:@
containsPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO Bool
containsPoint nsBezierPath point =
  sendMessage nsBezierPath containsPointSelector point

-- | @- cachesBezierPath@
cachesBezierPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO Bool
cachesBezierPath nsBezierPath =
  sendMessage nsBezierPath cachesBezierPathSelector

-- | @- setCachesBezierPath:@
setCachesBezierPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> Bool -> IO ()
setCachesBezierPath nsBezierPath flag =
  sendMessage nsBezierPath setCachesBezierPathSelector flag

-- | @- appendBezierPathWithGlyph:inFont:@
appendBezierPathWithGlyph_inFont :: (IsNSBezierPath nsBezierPath, IsNSFont font) => nsBezierPath -> CUInt -> font -> IO ()
appendBezierPathWithGlyph_inFont nsBezierPath glyph font =
  sendMessage nsBezierPath appendBezierPathWithGlyph_inFontSelector glyph (toNSFont font)

-- | @- appendBezierPathWithGlyphs:count:inFont:@
appendBezierPathWithGlyphs_count_inFont :: (IsNSBezierPath nsBezierPath, IsNSFont font) => nsBezierPath -> RawId -> CLong -> font -> IO ()
appendBezierPathWithGlyphs_count_inFont nsBezierPath glyphs count font =
  sendMessage nsBezierPath appendBezierPathWithGlyphs_count_inFontSelector glyphs count (toNSFont font)

-- | @- appendBezierPathWithPackedGlyphs:@
appendBezierPathWithPackedGlyphs :: IsNSBezierPath nsBezierPath => nsBezierPath -> Const (Ptr CChar) -> IO ()
appendBezierPathWithPackedGlyphs nsBezierPath packedGlyphs =
  sendMessage nsBezierPath appendBezierPathWithPackedGlyphsSelector packedGlyphs

-- | @- CGPath@
cgPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO RawId
cgPath nsBezierPath =
  sendMessage nsBezierPath cgPathSelector

-- | @- setCGPath:@
setCGPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> RawId -> IO ()
setCGPath nsBezierPath value =
  sendMessage nsBezierPath setCGPathSelector value

-- | @+ defaultMiterLimit@
defaultMiterLimit :: IO CDouble
defaultMiterLimit  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' defaultMiterLimitSelector

-- | @+ setDefaultMiterLimit:@
setDefaultMiterLimit :: CDouble -> IO ()
setDefaultMiterLimit value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' setDefaultMiterLimitSelector value

-- | @+ defaultFlatness@
defaultFlatness :: IO CDouble
defaultFlatness  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' defaultFlatnessSelector

-- | @+ setDefaultFlatness:@
setDefaultFlatness :: CDouble -> IO ()
setDefaultFlatness value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' setDefaultFlatnessSelector value

-- | @+ defaultWindingRule@
defaultWindingRule :: IO NSWindingRule
defaultWindingRule  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' defaultWindingRuleSelector

-- | @+ setDefaultWindingRule:@
setDefaultWindingRule :: NSWindingRule -> IO ()
setDefaultWindingRule value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' setDefaultWindingRuleSelector value

-- | @+ defaultLineCapStyle@
defaultLineCapStyle :: IO NSLineCapStyle
defaultLineCapStyle  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' defaultLineCapStyleSelector

-- | @+ setDefaultLineCapStyle:@
setDefaultLineCapStyle :: NSLineCapStyle -> IO ()
setDefaultLineCapStyle value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' setDefaultLineCapStyleSelector value

-- | @+ defaultLineJoinStyle@
defaultLineJoinStyle :: IO NSLineJoinStyle
defaultLineJoinStyle  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' defaultLineJoinStyleSelector

-- | @+ setDefaultLineJoinStyle:@
setDefaultLineJoinStyle :: NSLineJoinStyle -> IO ()
setDefaultLineJoinStyle value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' setDefaultLineJoinStyleSelector value

-- | @+ defaultLineWidth@
defaultLineWidth :: IO CDouble
defaultLineWidth  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' defaultLineWidthSelector

-- | @+ setDefaultLineWidth:@
setDefaultLineWidth :: CDouble -> IO ()
setDefaultLineWidth value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMessage cls' setDefaultLineWidthSelector value

-- | @- lineWidth@
lineWidth :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CDouble
lineWidth nsBezierPath =
  sendMessage nsBezierPath lineWidthSelector

-- | @- setLineWidth:@
setLineWidth :: IsNSBezierPath nsBezierPath => nsBezierPath -> CDouble -> IO ()
setLineWidth nsBezierPath value =
  sendMessage nsBezierPath setLineWidthSelector value

-- | @- lineCapStyle@
lineCapStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSLineCapStyle
lineCapStyle nsBezierPath =
  sendMessage nsBezierPath lineCapStyleSelector

-- | @- setLineCapStyle:@
setLineCapStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSLineCapStyle -> IO ()
setLineCapStyle nsBezierPath value =
  sendMessage nsBezierPath setLineCapStyleSelector value

-- | @- lineJoinStyle@
lineJoinStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSLineJoinStyle
lineJoinStyle nsBezierPath =
  sendMessage nsBezierPath lineJoinStyleSelector

-- | @- setLineJoinStyle:@
setLineJoinStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSLineJoinStyle -> IO ()
setLineJoinStyle nsBezierPath value =
  sendMessage nsBezierPath setLineJoinStyleSelector value

-- | @- windingRule@
windingRule :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSWindingRule
windingRule nsBezierPath =
  sendMessage nsBezierPath windingRuleSelector

-- | @- setWindingRule:@
setWindingRule :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSWindingRule -> IO ()
setWindingRule nsBezierPath value =
  sendMessage nsBezierPath setWindingRuleSelector value

-- | @- miterLimit@
miterLimit :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CDouble
miterLimit nsBezierPath =
  sendMessage nsBezierPath miterLimitSelector

-- | @- setMiterLimit:@
setMiterLimit :: IsNSBezierPath nsBezierPath => nsBezierPath -> CDouble -> IO ()
setMiterLimit nsBezierPath value =
  sendMessage nsBezierPath setMiterLimitSelector value

-- | @- flatness@
flatness :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CDouble
flatness nsBezierPath =
  sendMessage nsBezierPath flatnessSelector

-- | @- setFlatness:@
setFlatness :: IsNSBezierPath nsBezierPath => nsBezierPath -> CDouble -> IO ()
setFlatness nsBezierPath value =
  sendMessage nsBezierPath setFlatnessSelector value

-- | @- bezierPathByFlatteningPath@
bezierPathByFlatteningPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO (Id NSBezierPath)
bezierPathByFlatteningPath nsBezierPath =
  sendMessage nsBezierPath bezierPathByFlatteningPathSelector

-- | @- bezierPathByReversingPath@
bezierPathByReversingPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO (Id NSBezierPath)
bezierPathByReversingPath nsBezierPath =
  sendMessage nsBezierPath bezierPathByReversingPathSelector

-- | @- empty@
empty :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO Bool
empty nsBezierPath =
  sendMessage nsBezierPath emptySelector

-- | @- currentPoint@
currentPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSPoint
currentPoint nsBezierPath =
  sendMessage nsBezierPath currentPointSelector

-- | @- controlPointBounds@
controlPointBounds :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSRect
controlPointBounds nsBezierPath =
  sendMessage nsBezierPath controlPointBoundsSelector

-- | @- bounds@
bounds :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSRect
bounds nsBezierPath =
  sendMessage nsBezierPath boundsSelector

-- | @- elementCount@
elementCount :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CLong
elementCount nsBezierPath =
  sendMessage nsBezierPath elementCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bezierPath@
bezierPathSelector :: Selector '[] (Id NSBezierPath)
bezierPathSelector = mkSelector "bezierPath"

-- | @Selector@ for @bezierPathWithRect:@
bezierPathWithRectSelector :: Selector '[NSRect] (Id NSBezierPath)
bezierPathWithRectSelector = mkSelector "bezierPathWithRect:"

-- | @Selector@ for @bezierPathWithOvalInRect:@
bezierPathWithOvalInRectSelector :: Selector '[NSRect] (Id NSBezierPath)
bezierPathWithOvalInRectSelector = mkSelector "bezierPathWithOvalInRect:"

-- | @Selector@ for @bezierPathWithRoundedRect:xRadius:yRadius:@
bezierPathWithRoundedRect_xRadius_yRadiusSelector :: Selector '[NSRect, CDouble, CDouble] (Id NSBezierPath)
bezierPathWithRoundedRect_xRadius_yRadiusSelector = mkSelector "bezierPathWithRoundedRect:xRadius:yRadius:"

-- | @Selector@ for @bezierPathWithCGPath:@
bezierPathWithCGPathSelector :: Selector '[RawId] (Id NSBezierPath)
bezierPathWithCGPathSelector = mkSelector "bezierPathWithCGPath:"

-- | @Selector@ for @fillRect:@
fillRectSelector :: Selector '[NSRect] ()
fillRectSelector = mkSelector "fillRect:"

-- | @Selector@ for @strokeRect:@
strokeRectSelector :: Selector '[NSRect] ()
strokeRectSelector = mkSelector "strokeRect:"

-- | @Selector@ for @clipRect:@
clipRectSelector :: Selector '[NSRect] ()
clipRectSelector = mkSelector "clipRect:"

-- | @Selector@ for @strokeLineFromPoint:toPoint:@
strokeLineFromPoint_toPointSelector :: Selector '[NSPoint, NSPoint] ()
strokeLineFromPoint_toPointSelector = mkSelector "strokeLineFromPoint:toPoint:"

-- | @Selector@ for @drawPackedGlyphs:atPoint:@
drawPackedGlyphs_atPointSelector :: Selector '[Const (Ptr CChar), NSPoint] ()
drawPackedGlyphs_atPointSelector = mkSelector "drawPackedGlyphs:atPoint:"

-- | @Selector@ for @moveToPoint:@
moveToPointSelector :: Selector '[NSPoint] ()
moveToPointSelector = mkSelector "moveToPoint:"

-- | @Selector@ for @lineToPoint:@
lineToPointSelector :: Selector '[NSPoint] ()
lineToPointSelector = mkSelector "lineToPoint:"

-- | @Selector@ for @curveToPoint:controlPoint1:controlPoint2:@
curveToPoint_controlPoint1_controlPoint2Selector :: Selector '[NSPoint, NSPoint, NSPoint] ()
curveToPoint_controlPoint1_controlPoint2Selector = mkSelector "curveToPoint:controlPoint1:controlPoint2:"

-- | @Selector@ for @curveToPoint:controlPoint:@
curveToPoint_controlPointSelector :: Selector '[NSPoint, NSPoint] ()
curveToPoint_controlPointSelector = mkSelector "curveToPoint:controlPoint:"

-- | @Selector@ for @closePath@
closePathSelector :: Selector '[] ()
closePathSelector = mkSelector "closePath"

-- | @Selector@ for @removeAllPoints@
removeAllPointsSelector :: Selector '[] ()
removeAllPointsSelector = mkSelector "removeAllPoints"

-- | @Selector@ for @relativeMoveToPoint:@
relativeMoveToPointSelector :: Selector '[NSPoint] ()
relativeMoveToPointSelector = mkSelector "relativeMoveToPoint:"

-- | @Selector@ for @relativeLineToPoint:@
relativeLineToPointSelector :: Selector '[NSPoint] ()
relativeLineToPointSelector = mkSelector "relativeLineToPoint:"

-- | @Selector@ for @relativeCurveToPoint:controlPoint1:controlPoint2:@
relativeCurveToPoint_controlPoint1_controlPoint2Selector :: Selector '[NSPoint, NSPoint, NSPoint] ()
relativeCurveToPoint_controlPoint1_controlPoint2Selector = mkSelector "relativeCurveToPoint:controlPoint1:controlPoint2:"

-- | @Selector@ for @relativeCurveToPoint:controlPoint:@
relativeCurveToPoint_controlPointSelector :: Selector '[NSPoint, NSPoint] ()
relativeCurveToPoint_controlPointSelector = mkSelector "relativeCurveToPoint:controlPoint:"

-- | @Selector@ for @getLineDash:count:phase:@
getLineDash_count_phaseSelector :: Selector '[Ptr CDouble, Ptr CLong, Ptr CDouble] ()
getLineDash_count_phaseSelector = mkSelector "getLineDash:count:phase:"

-- | @Selector@ for @setLineDash:count:phase:@
setLineDash_count_phaseSelector :: Selector '[Const (Ptr CDouble), CLong, CDouble] ()
setLineDash_count_phaseSelector = mkSelector "setLineDash:count:phase:"

-- | @Selector@ for @stroke@
strokeSelector :: Selector '[] ()
strokeSelector = mkSelector "stroke"

-- | @Selector@ for @fill@
fillSelector :: Selector '[] ()
fillSelector = mkSelector "fill"

-- | @Selector@ for @addClip@
addClipSelector :: Selector '[] ()
addClipSelector = mkSelector "addClip"

-- | @Selector@ for @setClip@
setClipSelector :: Selector '[] ()
setClipSelector = mkSelector "setClip"

-- | @Selector@ for @transformUsingAffineTransform:@
transformUsingAffineTransformSelector :: Selector '[Id NSAffineTransform] ()
transformUsingAffineTransformSelector = mkSelector "transformUsingAffineTransform:"

-- | @Selector@ for @elementAtIndex:associatedPoints:@
elementAtIndex_associatedPointsSelector :: Selector '[CLong, Ptr NSPoint] NSBezierPathElement
elementAtIndex_associatedPointsSelector = mkSelector "elementAtIndex:associatedPoints:"

-- | @Selector@ for @elementAtIndex:@
elementAtIndexSelector :: Selector '[CLong] NSBezierPathElement
elementAtIndexSelector = mkSelector "elementAtIndex:"

-- | @Selector@ for @setAssociatedPoints:atIndex:@
setAssociatedPoints_atIndexSelector :: Selector '[Ptr NSPoint, CLong] ()
setAssociatedPoints_atIndexSelector = mkSelector "setAssociatedPoints:atIndex:"

-- | @Selector@ for @appendBezierPath:@
appendBezierPathSelector :: Selector '[Id NSBezierPath] ()
appendBezierPathSelector = mkSelector "appendBezierPath:"

-- | @Selector@ for @appendBezierPathWithRect:@
appendBezierPathWithRectSelector :: Selector '[NSRect] ()
appendBezierPathWithRectSelector = mkSelector "appendBezierPathWithRect:"

-- | @Selector@ for @appendBezierPathWithPoints:count:@
appendBezierPathWithPoints_countSelector :: Selector '[Ptr NSPoint, CLong] ()
appendBezierPathWithPoints_countSelector = mkSelector "appendBezierPathWithPoints:count:"

-- | @Selector@ for @appendBezierPathWithOvalInRect:@
appendBezierPathWithOvalInRectSelector :: Selector '[NSRect] ()
appendBezierPathWithOvalInRectSelector = mkSelector "appendBezierPathWithOvalInRect:"

-- | @Selector@ for @appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector :: Selector '[NSPoint, CDouble, CDouble, CDouble, Bool] ()
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector = mkSelector "appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:"

-- | @Selector@ for @appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector :: Selector '[NSPoint, CDouble, CDouble, CDouble] ()
appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector = mkSelector "appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:"

-- | @Selector@ for @appendBezierPathWithArcFromPoint:toPoint:radius:@
appendBezierPathWithArcFromPoint_toPoint_radiusSelector :: Selector '[NSPoint, NSPoint, CDouble] ()
appendBezierPathWithArcFromPoint_toPoint_radiusSelector = mkSelector "appendBezierPathWithArcFromPoint:toPoint:radius:"

-- | @Selector@ for @appendBezierPathWithCGGlyph:inFont:@
appendBezierPathWithCGGlyph_inFontSelector :: Selector '[CUShort, Id NSFont] ()
appendBezierPathWithCGGlyph_inFontSelector = mkSelector "appendBezierPathWithCGGlyph:inFont:"

-- | @Selector@ for @appendBezierPathWithCGGlyphs:count:inFont:@
appendBezierPathWithCGGlyphs_count_inFontSelector :: Selector '[Const RawId, CLong, Id NSFont] ()
appendBezierPathWithCGGlyphs_count_inFontSelector = mkSelector "appendBezierPathWithCGGlyphs:count:inFont:"

-- | @Selector@ for @appendBezierPathWithRoundedRect:xRadius:yRadius:@
appendBezierPathWithRoundedRect_xRadius_yRadiusSelector :: Selector '[NSRect, CDouble, CDouble] ()
appendBezierPathWithRoundedRect_xRadius_yRadiusSelector = mkSelector "appendBezierPathWithRoundedRect:xRadius:yRadius:"

-- | @Selector@ for @containsPoint:@
containsPointSelector :: Selector '[NSPoint] Bool
containsPointSelector = mkSelector "containsPoint:"

-- | @Selector@ for @cachesBezierPath@
cachesBezierPathSelector :: Selector '[] Bool
cachesBezierPathSelector = mkSelector "cachesBezierPath"

-- | @Selector@ for @setCachesBezierPath:@
setCachesBezierPathSelector :: Selector '[Bool] ()
setCachesBezierPathSelector = mkSelector "setCachesBezierPath:"

-- | @Selector@ for @appendBezierPathWithGlyph:inFont:@
appendBezierPathWithGlyph_inFontSelector :: Selector '[CUInt, Id NSFont] ()
appendBezierPathWithGlyph_inFontSelector = mkSelector "appendBezierPathWithGlyph:inFont:"

-- | @Selector@ for @appendBezierPathWithGlyphs:count:inFont:@
appendBezierPathWithGlyphs_count_inFontSelector :: Selector '[RawId, CLong, Id NSFont] ()
appendBezierPathWithGlyphs_count_inFontSelector = mkSelector "appendBezierPathWithGlyphs:count:inFont:"

-- | @Selector@ for @appendBezierPathWithPackedGlyphs:@
appendBezierPathWithPackedGlyphsSelector :: Selector '[Const (Ptr CChar)] ()
appendBezierPathWithPackedGlyphsSelector = mkSelector "appendBezierPathWithPackedGlyphs:"

-- | @Selector@ for @CGPath@
cgPathSelector :: Selector '[] RawId
cgPathSelector = mkSelector "CGPath"

-- | @Selector@ for @setCGPath:@
setCGPathSelector :: Selector '[RawId] ()
setCGPathSelector = mkSelector "setCGPath:"

-- | @Selector@ for @defaultMiterLimit@
defaultMiterLimitSelector :: Selector '[] CDouble
defaultMiterLimitSelector = mkSelector "defaultMiterLimit"

-- | @Selector@ for @setDefaultMiterLimit:@
setDefaultMiterLimitSelector :: Selector '[CDouble] ()
setDefaultMiterLimitSelector = mkSelector "setDefaultMiterLimit:"

-- | @Selector@ for @defaultFlatness@
defaultFlatnessSelector :: Selector '[] CDouble
defaultFlatnessSelector = mkSelector "defaultFlatness"

-- | @Selector@ for @setDefaultFlatness:@
setDefaultFlatnessSelector :: Selector '[CDouble] ()
setDefaultFlatnessSelector = mkSelector "setDefaultFlatness:"

-- | @Selector@ for @defaultWindingRule@
defaultWindingRuleSelector :: Selector '[] NSWindingRule
defaultWindingRuleSelector = mkSelector "defaultWindingRule"

-- | @Selector@ for @setDefaultWindingRule:@
setDefaultWindingRuleSelector :: Selector '[NSWindingRule] ()
setDefaultWindingRuleSelector = mkSelector "setDefaultWindingRule:"

-- | @Selector@ for @defaultLineCapStyle@
defaultLineCapStyleSelector :: Selector '[] NSLineCapStyle
defaultLineCapStyleSelector = mkSelector "defaultLineCapStyle"

-- | @Selector@ for @setDefaultLineCapStyle:@
setDefaultLineCapStyleSelector :: Selector '[NSLineCapStyle] ()
setDefaultLineCapStyleSelector = mkSelector "setDefaultLineCapStyle:"

-- | @Selector@ for @defaultLineJoinStyle@
defaultLineJoinStyleSelector :: Selector '[] NSLineJoinStyle
defaultLineJoinStyleSelector = mkSelector "defaultLineJoinStyle"

-- | @Selector@ for @setDefaultLineJoinStyle:@
setDefaultLineJoinStyleSelector :: Selector '[NSLineJoinStyle] ()
setDefaultLineJoinStyleSelector = mkSelector "setDefaultLineJoinStyle:"

-- | @Selector@ for @defaultLineWidth@
defaultLineWidthSelector :: Selector '[] CDouble
defaultLineWidthSelector = mkSelector "defaultLineWidth"

-- | @Selector@ for @setDefaultLineWidth:@
setDefaultLineWidthSelector :: Selector '[CDouble] ()
setDefaultLineWidthSelector = mkSelector "setDefaultLineWidth:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector '[] CDouble
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector '[CDouble] ()
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @lineCapStyle@
lineCapStyleSelector :: Selector '[] NSLineCapStyle
lineCapStyleSelector = mkSelector "lineCapStyle"

-- | @Selector@ for @setLineCapStyle:@
setLineCapStyleSelector :: Selector '[NSLineCapStyle] ()
setLineCapStyleSelector = mkSelector "setLineCapStyle:"

-- | @Selector@ for @lineJoinStyle@
lineJoinStyleSelector :: Selector '[] NSLineJoinStyle
lineJoinStyleSelector = mkSelector "lineJoinStyle"

-- | @Selector@ for @setLineJoinStyle:@
setLineJoinStyleSelector :: Selector '[NSLineJoinStyle] ()
setLineJoinStyleSelector = mkSelector "setLineJoinStyle:"

-- | @Selector@ for @windingRule@
windingRuleSelector :: Selector '[] NSWindingRule
windingRuleSelector = mkSelector "windingRule"

-- | @Selector@ for @setWindingRule:@
setWindingRuleSelector :: Selector '[NSWindingRule] ()
setWindingRuleSelector = mkSelector "setWindingRule:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector '[] CDouble
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector '[CDouble] ()
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @flatness@
flatnessSelector :: Selector '[] CDouble
flatnessSelector = mkSelector "flatness"

-- | @Selector@ for @setFlatness:@
setFlatnessSelector :: Selector '[CDouble] ()
setFlatnessSelector = mkSelector "setFlatness:"

-- | @Selector@ for @bezierPathByFlatteningPath@
bezierPathByFlatteningPathSelector :: Selector '[] (Id NSBezierPath)
bezierPathByFlatteningPathSelector = mkSelector "bezierPathByFlatteningPath"

-- | @Selector@ for @bezierPathByReversingPath@
bezierPathByReversingPathSelector :: Selector '[] (Id NSBezierPath)
bezierPathByReversingPathSelector = mkSelector "bezierPathByReversingPath"

-- | @Selector@ for @empty@
emptySelector :: Selector '[] Bool
emptySelector = mkSelector "empty"

-- | @Selector@ for @currentPoint@
currentPointSelector :: Selector '[] NSPoint
currentPointSelector = mkSelector "currentPoint"

-- | @Selector@ for @controlPointBounds@
controlPointBoundsSelector :: Selector '[] NSRect
controlPointBoundsSelector = mkSelector "controlPointBounds"

-- | @Selector@ for @bounds@
boundsSelector :: Selector '[] NSRect
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CLong
elementCountSelector = mkSelector "elementCount"

