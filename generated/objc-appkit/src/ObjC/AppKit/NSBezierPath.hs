{-# LANGUAGE PatternSynonyms #-}
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
  , appendBezierPathWithRoundedRect_xRadius_yRadius
  , containsPoint
  , cachesBezierPath
  , setCachesBezierPath
  , appendBezierPathWithGlyph_inFont
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
  , bezierPathSelector
  , bezierPathWithRectSelector
  , bezierPathWithOvalInRectSelector
  , bezierPathWithRoundedRect_xRadius_yRadiusSelector
  , bezierPathWithCGPathSelector
  , fillRectSelector
  , strokeRectSelector
  , clipRectSelector
  , strokeLineFromPoint_toPointSelector
  , drawPackedGlyphs_atPointSelector
  , moveToPointSelector
  , lineToPointSelector
  , curveToPoint_controlPoint1_controlPoint2Selector
  , curveToPoint_controlPointSelector
  , closePathSelector
  , removeAllPointsSelector
  , relativeMoveToPointSelector
  , relativeLineToPointSelector
  , relativeCurveToPoint_controlPoint1_controlPoint2Selector
  , relativeCurveToPoint_controlPointSelector
  , getLineDash_count_phaseSelector
  , setLineDash_count_phaseSelector
  , strokeSelector
  , fillSelector
  , addClipSelector
  , setClipSelector
  , transformUsingAffineTransformSelector
  , elementAtIndex_associatedPointsSelector
  , elementAtIndexSelector
  , setAssociatedPoints_atIndexSelector
  , appendBezierPathSelector
  , appendBezierPathWithRectSelector
  , appendBezierPathWithPoints_countSelector
  , appendBezierPathWithOvalInRectSelector
  , appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector
  , appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector
  , appendBezierPathWithArcFromPoint_toPoint_radiusSelector
  , appendBezierPathWithCGGlyph_inFontSelector
  , appendBezierPathWithRoundedRect_xRadius_yRadiusSelector
  , containsPointSelector
  , cachesBezierPathSelector
  , setCachesBezierPathSelector
  , appendBezierPathWithGlyph_inFontSelector
  , appendBezierPathWithPackedGlyphsSelector
  , cgPathSelector
  , setCGPathSelector
  , defaultMiterLimitSelector
  , setDefaultMiterLimitSelector
  , defaultFlatnessSelector
  , setDefaultFlatnessSelector
  , defaultWindingRuleSelector
  , setDefaultWindingRuleSelector
  , defaultLineCapStyleSelector
  , setDefaultLineCapStyleSelector
  , defaultLineJoinStyleSelector
  , setDefaultLineJoinStyleSelector
  , defaultLineWidthSelector
  , setDefaultLineWidthSelector
  , lineWidthSelector
  , setLineWidthSelector
  , lineCapStyleSelector
  , setLineCapStyleSelector
  , lineJoinStyleSelector
  , setLineJoinStyleSelector
  , windingRuleSelector
  , setWindingRuleSelector
  , miterLimitSelector
  , setMiterLimitSelector
  , flatnessSelector
  , setFlatnessSelector
  , bezierPathByFlatteningPathSelector
  , bezierPathByReversingPathSelector
  , emptySelector
  , currentPointSelector
  , controlPointBoundsSelector
  , boundsSelector
  , elementCountSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ bezierPath@
bezierPath :: IO (Id NSBezierPath)
bezierPath  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "bezierPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ bezierPathWithRect:@
bezierPathWithRect :: NSRect -> IO (Id NSBezierPath)
bezierPathWithRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "bezierPathWithRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @+ bezierPathWithOvalInRect:@
bezierPathWithOvalInRect :: NSRect -> IO (Id NSBezierPath)
bezierPathWithOvalInRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "bezierPathWithOvalInRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @+ bezierPathWithRoundedRect:xRadius:yRadius:@
bezierPathWithRoundedRect_xRadius_yRadius :: NSRect -> CDouble -> CDouble -> IO (Id NSBezierPath)
bezierPathWithRoundedRect_xRadius_yRadius rect xRadius yRadius =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "bezierPathWithRoundedRect:xRadius:yRadius:") (retPtr retVoid) [argNSRect rect, argCDouble (fromIntegral xRadius), argCDouble (fromIntegral yRadius)] >>= retainedObject . castPtr

-- | @+ bezierPathWithCGPath:@
bezierPathWithCGPath :: RawId -> IO (Id NSBezierPath)
bezierPathWithCGPath cgPath =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "bezierPathWithCGPath:") (retPtr retVoid) [argPtr (castPtr (unRawId cgPath) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fillRect:@
fillRect :: NSRect -> IO ()
fillRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "fillRect:") retVoid [argNSRect rect]

-- | @+ strokeRect:@
strokeRect :: NSRect -> IO ()
strokeRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "strokeRect:") retVoid [argNSRect rect]

-- | @+ clipRect:@
clipRect :: NSRect -> IO ()
clipRect rect =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "clipRect:") retVoid [argNSRect rect]

-- | @+ strokeLineFromPoint:toPoint:@
strokeLineFromPoint_toPoint :: NSPoint -> NSPoint -> IO ()
strokeLineFromPoint_toPoint point1 point2 =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "strokeLineFromPoint:toPoint:") retVoid [argNSPoint point1, argNSPoint point2]

-- | @+ drawPackedGlyphs:atPoint:@
drawPackedGlyphs_atPoint :: Const (Ptr CChar) -> NSPoint -> IO ()
drawPackedGlyphs_atPoint packedGlyphs point =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "drawPackedGlyphs:atPoint:") retVoid [argPtr (unConst packedGlyphs), argNSPoint point]

-- | @- moveToPoint:@
moveToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
moveToPoint nsBezierPath  point =
  sendMsg nsBezierPath (mkSelector "moveToPoint:") retVoid [argNSPoint point]

-- | @- lineToPoint:@
lineToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
lineToPoint nsBezierPath  point =
  sendMsg nsBezierPath (mkSelector "lineToPoint:") retVoid [argNSPoint point]

-- | @- curveToPoint:controlPoint1:controlPoint2:@
curveToPoint_controlPoint1_controlPoint2 :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> NSPoint -> IO ()
curveToPoint_controlPoint1_controlPoint2 nsBezierPath  endPoint controlPoint1 controlPoint2 =
  sendMsg nsBezierPath (mkSelector "curveToPoint:controlPoint1:controlPoint2:") retVoid [argNSPoint endPoint, argNSPoint controlPoint1, argNSPoint controlPoint2]

-- | @- curveToPoint:controlPoint:@
curveToPoint_controlPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> IO ()
curveToPoint_controlPoint nsBezierPath  endPoint controlPoint =
  sendMsg nsBezierPath (mkSelector "curveToPoint:controlPoint:") retVoid [argNSPoint endPoint, argNSPoint controlPoint]

-- | @- closePath@
closePath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
closePath nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "closePath") retVoid []

-- | @- removeAllPoints@
removeAllPoints :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
removeAllPoints nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "removeAllPoints") retVoid []

-- | @- relativeMoveToPoint:@
relativeMoveToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
relativeMoveToPoint nsBezierPath  point =
  sendMsg nsBezierPath (mkSelector "relativeMoveToPoint:") retVoid [argNSPoint point]

-- | @- relativeLineToPoint:@
relativeLineToPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO ()
relativeLineToPoint nsBezierPath  point =
  sendMsg nsBezierPath (mkSelector "relativeLineToPoint:") retVoid [argNSPoint point]

-- | @- relativeCurveToPoint:controlPoint1:controlPoint2:@
relativeCurveToPoint_controlPoint1_controlPoint2 :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> NSPoint -> IO ()
relativeCurveToPoint_controlPoint1_controlPoint2 nsBezierPath  endPoint controlPoint1 controlPoint2 =
  sendMsg nsBezierPath (mkSelector "relativeCurveToPoint:controlPoint1:controlPoint2:") retVoid [argNSPoint endPoint, argNSPoint controlPoint1, argNSPoint controlPoint2]

-- | @- relativeCurveToPoint:controlPoint:@
relativeCurveToPoint_controlPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> IO ()
relativeCurveToPoint_controlPoint nsBezierPath  endPoint controlPoint =
  sendMsg nsBezierPath (mkSelector "relativeCurveToPoint:controlPoint:") retVoid [argNSPoint endPoint, argNSPoint controlPoint]

-- | @- getLineDash:count:phase:@
getLineDash_count_phase :: IsNSBezierPath nsBezierPath => nsBezierPath -> Ptr CDouble -> Ptr CLong -> Ptr CDouble -> IO ()
getLineDash_count_phase nsBezierPath  pattern_ count phase =
  sendMsg nsBezierPath (mkSelector "getLineDash:count:phase:") retVoid [argPtr pattern_, argPtr count, argPtr phase]

-- | @- setLineDash:count:phase:@
setLineDash_count_phase :: IsNSBezierPath nsBezierPath => nsBezierPath -> Const (Ptr CDouble) -> CLong -> CDouble -> IO ()
setLineDash_count_phase nsBezierPath  pattern_ count phase =
  sendMsg nsBezierPath (mkSelector "setLineDash:count:phase:") retVoid [argPtr (unConst pattern_), argCLong (fromIntegral count), argCDouble (fromIntegral phase)]

-- | @- stroke@
stroke :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
stroke nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "stroke") retVoid []

-- | @- fill@
fill :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
fill nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "fill") retVoid []

-- | @- addClip@
addClip :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
addClip nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "addClip") retVoid []

-- | @- setClip@
setClip :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO ()
setClip nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "setClip") retVoid []

-- | @- transformUsingAffineTransform:@
transformUsingAffineTransform :: (IsNSBezierPath nsBezierPath, IsNSAffineTransform transform) => nsBezierPath -> transform -> IO ()
transformUsingAffineTransform nsBezierPath  transform =
withObjCPtr transform $ \raw_transform ->
    sendMsg nsBezierPath (mkSelector "transformUsingAffineTransform:") retVoid [argPtr (castPtr raw_transform :: Ptr ())]

-- | @- elementAtIndex:associatedPoints:@
elementAtIndex_associatedPoints :: IsNSBezierPath nsBezierPath => nsBezierPath -> CLong -> Ptr NSPoint -> IO NSBezierPathElement
elementAtIndex_associatedPoints nsBezierPath  index points =
  fmap (coerce :: CULong -> NSBezierPathElement) $ sendMsg nsBezierPath (mkSelector "elementAtIndex:associatedPoints:") retCULong [argCLong (fromIntegral index), argPtr points]

-- | @- elementAtIndex:@
elementAtIndex :: IsNSBezierPath nsBezierPath => nsBezierPath -> CLong -> IO NSBezierPathElement
elementAtIndex nsBezierPath  index =
  fmap (coerce :: CULong -> NSBezierPathElement) $ sendMsg nsBezierPath (mkSelector "elementAtIndex:") retCULong [argCLong (fromIntegral index)]

-- | @- setAssociatedPoints:atIndex:@
setAssociatedPoints_atIndex :: IsNSBezierPath nsBezierPath => nsBezierPath -> Ptr NSPoint -> CLong -> IO ()
setAssociatedPoints_atIndex nsBezierPath  points index =
  sendMsg nsBezierPath (mkSelector "setAssociatedPoints:atIndex:") retVoid [argPtr points, argCLong (fromIntegral index)]

-- | @- appendBezierPath:@
appendBezierPath :: (IsNSBezierPath nsBezierPath, IsNSBezierPath path) => nsBezierPath -> path -> IO ()
appendBezierPath nsBezierPath  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsBezierPath (mkSelector "appendBezierPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- appendBezierPathWithRect:@
appendBezierPathWithRect :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSRect -> IO ()
appendBezierPathWithRect nsBezierPath  rect =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithRect:") retVoid [argNSRect rect]

-- | @- appendBezierPathWithPoints:count:@
appendBezierPathWithPoints_count :: IsNSBezierPath nsBezierPath => nsBezierPath -> Ptr NSPoint -> CLong -> IO ()
appendBezierPathWithPoints_count nsBezierPath  points count =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithPoints:count:") retVoid [argPtr points, argCLong (fromIntegral count)]

-- | @- appendBezierPathWithOvalInRect:@
appendBezierPathWithOvalInRect :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSRect -> IO ()
appendBezierPathWithOvalInRect nsBezierPath  rect =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithOvalInRect:") retVoid [argNSRect rect]

-- | @- appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> CDouble -> CDouble -> CDouble -> Bool -> IO ()
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise nsBezierPath  center radius startAngle endAngle clockwise =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:") retVoid [argNSPoint center, argCDouble (fromIntegral radius), argCDouble (fromIntegral startAngle), argCDouble (fromIntegral endAngle), argCULong (if clockwise then 1 else 0)]

-- | @- appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> CDouble -> CDouble -> CDouble -> IO ()
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle nsBezierPath  center radius startAngle endAngle =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:") retVoid [argNSPoint center, argCDouble (fromIntegral radius), argCDouble (fromIntegral startAngle), argCDouble (fromIntegral endAngle)]

-- | @- appendBezierPathWithArcFromPoint:toPoint:radius:@
appendBezierPathWithArcFromPoint_toPoint_radius :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> NSPoint -> CDouble -> IO ()
appendBezierPathWithArcFromPoint_toPoint_radius nsBezierPath  point1 point2 radius =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithArcFromPoint:toPoint:radius:") retVoid [argNSPoint point1, argNSPoint point2, argCDouble (fromIntegral radius)]

-- | @- appendBezierPathWithCGGlyph:inFont:@
appendBezierPathWithCGGlyph_inFont :: (IsNSBezierPath nsBezierPath, IsNSFont font) => nsBezierPath -> CUShort -> font -> IO ()
appendBezierPathWithCGGlyph_inFont nsBezierPath  glyph font =
withObjCPtr font $ \raw_font ->
    sendMsg nsBezierPath (mkSelector "appendBezierPathWithCGGlyph:inFont:") retVoid [argCUInt (fromIntegral glyph), argPtr (castPtr raw_font :: Ptr ())]

-- | @- appendBezierPathWithRoundedRect:xRadius:yRadius:@
appendBezierPathWithRoundedRect_xRadius_yRadius :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSRect -> CDouble -> CDouble -> IO ()
appendBezierPathWithRoundedRect_xRadius_yRadius nsBezierPath  rect xRadius yRadius =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithRoundedRect:xRadius:yRadius:") retVoid [argNSRect rect, argCDouble (fromIntegral xRadius), argCDouble (fromIntegral yRadius)]

-- | @- containsPoint:@
containsPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSPoint -> IO Bool
containsPoint nsBezierPath  point =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBezierPath (mkSelector "containsPoint:") retCULong [argNSPoint point]

-- | @- cachesBezierPath@
cachesBezierPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO Bool
cachesBezierPath nsBezierPath  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBezierPath (mkSelector "cachesBezierPath") retCULong []

-- | @- setCachesBezierPath:@
setCachesBezierPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> Bool -> IO ()
setCachesBezierPath nsBezierPath  flag =
  sendMsg nsBezierPath (mkSelector "setCachesBezierPath:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- appendBezierPathWithGlyph:inFont:@
appendBezierPathWithGlyph_inFont :: (IsNSBezierPath nsBezierPath, IsNSFont font) => nsBezierPath -> CUInt -> font -> IO ()
appendBezierPathWithGlyph_inFont nsBezierPath  glyph font =
withObjCPtr font $ \raw_font ->
    sendMsg nsBezierPath (mkSelector "appendBezierPathWithGlyph:inFont:") retVoid [argCUInt (fromIntegral glyph), argPtr (castPtr raw_font :: Ptr ())]

-- | @- appendBezierPathWithPackedGlyphs:@
appendBezierPathWithPackedGlyphs :: IsNSBezierPath nsBezierPath => nsBezierPath -> Const (Ptr CChar) -> IO ()
appendBezierPathWithPackedGlyphs nsBezierPath  packedGlyphs =
  sendMsg nsBezierPath (mkSelector "appendBezierPathWithPackedGlyphs:") retVoid [argPtr (unConst packedGlyphs)]

-- | @- CGPath@
cgPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO RawId
cgPath nsBezierPath  =
  fmap (RawId . castPtr) $ sendMsg nsBezierPath (mkSelector "CGPath") (retPtr retVoid) []

-- | @- setCGPath:@
setCGPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> RawId -> IO ()
setCGPath nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setCGPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @+ defaultMiterLimit@
defaultMiterLimit :: IO CDouble
defaultMiterLimit  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "defaultMiterLimit") retCDouble []

-- | @+ setDefaultMiterLimit:@
setDefaultMiterLimit :: CDouble -> IO ()
setDefaultMiterLimit value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "setDefaultMiterLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @+ defaultFlatness@
defaultFlatness :: IO CDouble
defaultFlatness  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "defaultFlatness") retCDouble []

-- | @+ setDefaultFlatness:@
setDefaultFlatness :: CDouble -> IO ()
setDefaultFlatness value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "setDefaultFlatness:") retVoid [argCDouble (fromIntegral value)]

-- | @+ defaultWindingRule@
defaultWindingRule :: IO NSWindingRule
defaultWindingRule  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    fmap (coerce :: CULong -> NSWindingRule) $ sendClassMsg cls' (mkSelector "defaultWindingRule") retCULong []

-- | @+ setDefaultWindingRule:@
setDefaultWindingRule :: NSWindingRule -> IO ()
setDefaultWindingRule value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "setDefaultWindingRule:") retVoid [argCULong (coerce value)]

-- | @+ defaultLineCapStyle@
defaultLineCapStyle :: IO NSLineCapStyle
defaultLineCapStyle  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    fmap (coerce :: CULong -> NSLineCapStyle) $ sendClassMsg cls' (mkSelector "defaultLineCapStyle") retCULong []

-- | @+ setDefaultLineCapStyle:@
setDefaultLineCapStyle :: NSLineCapStyle -> IO ()
setDefaultLineCapStyle value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "setDefaultLineCapStyle:") retVoid [argCULong (coerce value)]

-- | @+ defaultLineJoinStyle@
defaultLineJoinStyle :: IO NSLineJoinStyle
defaultLineJoinStyle  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    fmap (coerce :: CULong -> NSLineJoinStyle) $ sendClassMsg cls' (mkSelector "defaultLineJoinStyle") retCULong []

-- | @+ setDefaultLineJoinStyle:@
setDefaultLineJoinStyle :: NSLineJoinStyle -> IO ()
setDefaultLineJoinStyle value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "setDefaultLineJoinStyle:") retVoid [argCULong (coerce value)]

-- | @+ defaultLineWidth@
defaultLineWidth :: IO CDouble
defaultLineWidth  =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "defaultLineWidth") retCDouble []

-- | @+ setDefaultLineWidth:@
setDefaultLineWidth :: CDouble -> IO ()
setDefaultLineWidth value =
  do
    cls' <- getRequiredClass "NSBezierPath"
    sendClassMsg cls' (mkSelector "setDefaultLineWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineWidth@
lineWidth :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CDouble
lineWidth nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "lineWidth") retCDouble []

-- | @- setLineWidth:@
setLineWidth :: IsNSBezierPath nsBezierPath => nsBezierPath -> CDouble -> IO ()
setLineWidth nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setLineWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineCapStyle@
lineCapStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSLineCapStyle
lineCapStyle nsBezierPath  =
  fmap (coerce :: CULong -> NSLineCapStyle) $ sendMsg nsBezierPath (mkSelector "lineCapStyle") retCULong []

-- | @- setLineCapStyle:@
setLineCapStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSLineCapStyle -> IO ()
setLineCapStyle nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setLineCapStyle:") retVoid [argCULong (coerce value)]

-- | @- lineJoinStyle@
lineJoinStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSLineJoinStyle
lineJoinStyle nsBezierPath  =
  fmap (coerce :: CULong -> NSLineJoinStyle) $ sendMsg nsBezierPath (mkSelector "lineJoinStyle") retCULong []

-- | @- setLineJoinStyle:@
setLineJoinStyle :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSLineJoinStyle -> IO ()
setLineJoinStyle nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setLineJoinStyle:") retVoid [argCULong (coerce value)]

-- | @- windingRule@
windingRule :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSWindingRule
windingRule nsBezierPath  =
  fmap (coerce :: CULong -> NSWindingRule) $ sendMsg nsBezierPath (mkSelector "windingRule") retCULong []

-- | @- setWindingRule:@
setWindingRule :: IsNSBezierPath nsBezierPath => nsBezierPath -> NSWindingRule -> IO ()
setWindingRule nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setWindingRule:") retVoid [argCULong (coerce value)]

-- | @- miterLimit@
miterLimit :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CDouble
miterLimit nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "miterLimit") retCDouble []

-- | @- setMiterLimit:@
setMiterLimit :: IsNSBezierPath nsBezierPath => nsBezierPath -> CDouble -> IO ()
setMiterLimit nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setMiterLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- flatness@
flatness :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CDouble
flatness nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "flatness") retCDouble []

-- | @- setFlatness:@
setFlatness :: IsNSBezierPath nsBezierPath => nsBezierPath -> CDouble -> IO ()
setFlatness nsBezierPath  value =
  sendMsg nsBezierPath (mkSelector "setFlatness:") retVoid [argCDouble (fromIntegral value)]

-- | @- bezierPathByFlatteningPath@
bezierPathByFlatteningPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO (Id NSBezierPath)
bezierPathByFlatteningPath nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "bezierPathByFlatteningPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bezierPathByReversingPath@
bezierPathByReversingPath :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO (Id NSBezierPath)
bezierPathByReversingPath nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "bezierPathByReversingPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- empty@
empty :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO Bool
empty nsBezierPath  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBezierPath (mkSelector "empty") retCULong []

-- | @- currentPoint@
currentPoint :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSPoint
currentPoint nsBezierPath  =
  sendMsgStret nsBezierPath (mkSelector "currentPoint") retNSPoint []

-- | @- controlPointBounds@
controlPointBounds :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSRect
controlPointBounds nsBezierPath  =
  sendMsgStret nsBezierPath (mkSelector "controlPointBounds") retNSRect []

-- | @- bounds@
bounds :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO NSRect
bounds nsBezierPath  =
  sendMsgStret nsBezierPath (mkSelector "bounds") retNSRect []

-- | @- elementCount@
elementCount :: IsNSBezierPath nsBezierPath => nsBezierPath -> IO CLong
elementCount nsBezierPath  =
  sendMsg nsBezierPath (mkSelector "elementCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bezierPath@
bezierPathSelector :: Selector
bezierPathSelector = mkSelector "bezierPath"

-- | @Selector@ for @bezierPathWithRect:@
bezierPathWithRectSelector :: Selector
bezierPathWithRectSelector = mkSelector "bezierPathWithRect:"

-- | @Selector@ for @bezierPathWithOvalInRect:@
bezierPathWithOvalInRectSelector :: Selector
bezierPathWithOvalInRectSelector = mkSelector "bezierPathWithOvalInRect:"

-- | @Selector@ for @bezierPathWithRoundedRect:xRadius:yRadius:@
bezierPathWithRoundedRect_xRadius_yRadiusSelector :: Selector
bezierPathWithRoundedRect_xRadius_yRadiusSelector = mkSelector "bezierPathWithRoundedRect:xRadius:yRadius:"

-- | @Selector@ for @bezierPathWithCGPath:@
bezierPathWithCGPathSelector :: Selector
bezierPathWithCGPathSelector = mkSelector "bezierPathWithCGPath:"

-- | @Selector@ for @fillRect:@
fillRectSelector :: Selector
fillRectSelector = mkSelector "fillRect:"

-- | @Selector@ for @strokeRect:@
strokeRectSelector :: Selector
strokeRectSelector = mkSelector "strokeRect:"

-- | @Selector@ for @clipRect:@
clipRectSelector :: Selector
clipRectSelector = mkSelector "clipRect:"

-- | @Selector@ for @strokeLineFromPoint:toPoint:@
strokeLineFromPoint_toPointSelector :: Selector
strokeLineFromPoint_toPointSelector = mkSelector "strokeLineFromPoint:toPoint:"

-- | @Selector@ for @drawPackedGlyphs:atPoint:@
drawPackedGlyphs_atPointSelector :: Selector
drawPackedGlyphs_atPointSelector = mkSelector "drawPackedGlyphs:atPoint:"

-- | @Selector@ for @moveToPoint:@
moveToPointSelector :: Selector
moveToPointSelector = mkSelector "moveToPoint:"

-- | @Selector@ for @lineToPoint:@
lineToPointSelector :: Selector
lineToPointSelector = mkSelector "lineToPoint:"

-- | @Selector@ for @curveToPoint:controlPoint1:controlPoint2:@
curveToPoint_controlPoint1_controlPoint2Selector :: Selector
curveToPoint_controlPoint1_controlPoint2Selector = mkSelector "curveToPoint:controlPoint1:controlPoint2:"

-- | @Selector@ for @curveToPoint:controlPoint:@
curveToPoint_controlPointSelector :: Selector
curveToPoint_controlPointSelector = mkSelector "curveToPoint:controlPoint:"

-- | @Selector@ for @closePath@
closePathSelector :: Selector
closePathSelector = mkSelector "closePath"

-- | @Selector@ for @removeAllPoints@
removeAllPointsSelector :: Selector
removeAllPointsSelector = mkSelector "removeAllPoints"

-- | @Selector@ for @relativeMoveToPoint:@
relativeMoveToPointSelector :: Selector
relativeMoveToPointSelector = mkSelector "relativeMoveToPoint:"

-- | @Selector@ for @relativeLineToPoint:@
relativeLineToPointSelector :: Selector
relativeLineToPointSelector = mkSelector "relativeLineToPoint:"

-- | @Selector@ for @relativeCurveToPoint:controlPoint1:controlPoint2:@
relativeCurveToPoint_controlPoint1_controlPoint2Selector :: Selector
relativeCurveToPoint_controlPoint1_controlPoint2Selector = mkSelector "relativeCurveToPoint:controlPoint1:controlPoint2:"

-- | @Selector@ for @relativeCurveToPoint:controlPoint:@
relativeCurveToPoint_controlPointSelector :: Selector
relativeCurveToPoint_controlPointSelector = mkSelector "relativeCurveToPoint:controlPoint:"

-- | @Selector@ for @getLineDash:count:phase:@
getLineDash_count_phaseSelector :: Selector
getLineDash_count_phaseSelector = mkSelector "getLineDash:count:phase:"

-- | @Selector@ for @setLineDash:count:phase:@
setLineDash_count_phaseSelector :: Selector
setLineDash_count_phaseSelector = mkSelector "setLineDash:count:phase:"

-- | @Selector@ for @stroke@
strokeSelector :: Selector
strokeSelector = mkSelector "stroke"

-- | @Selector@ for @fill@
fillSelector :: Selector
fillSelector = mkSelector "fill"

-- | @Selector@ for @addClip@
addClipSelector :: Selector
addClipSelector = mkSelector "addClip"

-- | @Selector@ for @setClip@
setClipSelector :: Selector
setClipSelector = mkSelector "setClip"

-- | @Selector@ for @transformUsingAffineTransform:@
transformUsingAffineTransformSelector :: Selector
transformUsingAffineTransformSelector = mkSelector "transformUsingAffineTransform:"

-- | @Selector@ for @elementAtIndex:associatedPoints:@
elementAtIndex_associatedPointsSelector :: Selector
elementAtIndex_associatedPointsSelector = mkSelector "elementAtIndex:associatedPoints:"

-- | @Selector@ for @elementAtIndex:@
elementAtIndexSelector :: Selector
elementAtIndexSelector = mkSelector "elementAtIndex:"

-- | @Selector@ for @setAssociatedPoints:atIndex:@
setAssociatedPoints_atIndexSelector :: Selector
setAssociatedPoints_atIndexSelector = mkSelector "setAssociatedPoints:atIndex:"

-- | @Selector@ for @appendBezierPath:@
appendBezierPathSelector :: Selector
appendBezierPathSelector = mkSelector "appendBezierPath:"

-- | @Selector@ for @appendBezierPathWithRect:@
appendBezierPathWithRectSelector :: Selector
appendBezierPathWithRectSelector = mkSelector "appendBezierPathWithRect:"

-- | @Selector@ for @appendBezierPathWithPoints:count:@
appendBezierPathWithPoints_countSelector :: Selector
appendBezierPathWithPoints_countSelector = mkSelector "appendBezierPathWithPoints:count:"

-- | @Selector@ for @appendBezierPathWithOvalInRect:@
appendBezierPathWithOvalInRectSelector :: Selector
appendBezierPathWithOvalInRectSelector = mkSelector "appendBezierPathWithOvalInRect:"

-- | @Selector@ for @appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector :: Selector
appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwiseSelector = mkSelector "appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:clockwise:"

-- | @Selector@ for @appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:@
appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector :: Selector
appendBezierPathWithArcWithCenter_radius_startAngle_endAngleSelector = mkSelector "appendBezierPathWithArcWithCenter:radius:startAngle:endAngle:"

-- | @Selector@ for @appendBezierPathWithArcFromPoint:toPoint:radius:@
appendBezierPathWithArcFromPoint_toPoint_radiusSelector :: Selector
appendBezierPathWithArcFromPoint_toPoint_radiusSelector = mkSelector "appendBezierPathWithArcFromPoint:toPoint:radius:"

-- | @Selector@ for @appendBezierPathWithCGGlyph:inFont:@
appendBezierPathWithCGGlyph_inFontSelector :: Selector
appendBezierPathWithCGGlyph_inFontSelector = mkSelector "appendBezierPathWithCGGlyph:inFont:"

-- | @Selector@ for @appendBezierPathWithRoundedRect:xRadius:yRadius:@
appendBezierPathWithRoundedRect_xRadius_yRadiusSelector :: Selector
appendBezierPathWithRoundedRect_xRadius_yRadiusSelector = mkSelector "appendBezierPathWithRoundedRect:xRadius:yRadius:"

-- | @Selector@ for @containsPoint:@
containsPointSelector :: Selector
containsPointSelector = mkSelector "containsPoint:"

-- | @Selector@ for @cachesBezierPath@
cachesBezierPathSelector :: Selector
cachesBezierPathSelector = mkSelector "cachesBezierPath"

-- | @Selector@ for @setCachesBezierPath:@
setCachesBezierPathSelector :: Selector
setCachesBezierPathSelector = mkSelector "setCachesBezierPath:"

-- | @Selector@ for @appendBezierPathWithGlyph:inFont:@
appendBezierPathWithGlyph_inFontSelector :: Selector
appendBezierPathWithGlyph_inFontSelector = mkSelector "appendBezierPathWithGlyph:inFont:"

-- | @Selector@ for @appendBezierPathWithPackedGlyphs:@
appendBezierPathWithPackedGlyphsSelector :: Selector
appendBezierPathWithPackedGlyphsSelector = mkSelector "appendBezierPathWithPackedGlyphs:"

-- | @Selector@ for @CGPath@
cgPathSelector :: Selector
cgPathSelector = mkSelector "CGPath"

-- | @Selector@ for @setCGPath:@
setCGPathSelector :: Selector
setCGPathSelector = mkSelector "setCGPath:"

-- | @Selector@ for @defaultMiterLimit@
defaultMiterLimitSelector :: Selector
defaultMiterLimitSelector = mkSelector "defaultMiterLimit"

-- | @Selector@ for @setDefaultMiterLimit:@
setDefaultMiterLimitSelector :: Selector
setDefaultMiterLimitSelector = mkSelector "setDefaultMiterLimit:"

-- | @Selector@ for @defaultFlatness@
defaultFlatnessSelector :: Selector
defaultFlatnessSelector = mkSelector "defaultFlatness"

-- | @Selector@ for @setDefaultFlatness:@
setDefaultFlatnessSelector :: Selector
setDefaultFlatnessSelector = mkSelector "setDefaultFlatness:"

-- | @Selector@ for @defaultWindingRule@
defaultWindingRuleSelector :: Selector
defaultWindingRuleSelector = mkSelector "defaultWindingRule"

-- | @Selector@ for @setDefaultWindingRule:@
setDefaultWindingRuleSelector :: Selector
setDefaultWindingRuleSelector = mkSelector "setDefaultWindingRule:"

-- | @Selector@ for @defaultLineCapStyle@
defaultLineCapStyleSelector :: Selector
defaultLineCapStyleSelector = mkSelector "defaultLineCapStyle"

-- | @Selector@ for @setDefaultLineCapStyle:@
setDefaultLineCapStyleSelector :: Selector
setDefaultLineCapStyleSelector = mkSelector "setDefaultLineCapStyle:"

-- | @Selector@ for @defaultLineJoinStyle@
defaultLineJoinStyleSelector :: Selector
defaultLineJoinStyleSelector = mkSelector "defaultLineJoinStyle"

-- | @Selector@ for @setDefaultLineJoinStyle:@
setDefaultLineJoinStyleSelector :: Selector
setDefaultLineJoinStyleSelector = mkSelector "setDefaultLineJoinStyle:"

-- | @Selector@ for @defaultLineWidth@
defaultLineWidthSelector :: Selector
defaultLineWidthSelector = mkSelector "defaultLineWidth"

-- | @Selector@ for @setDefaultLineWidth:@
setDefaultLineWidthSelector :: Selector
setDefaultLineWidthSelector = mkSelector "setDefaultLineWidth:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @lineCapStyle@
lineCapStyleSelector :: Selector
lineCapStyleSelector = mkSelector "lineCapStyle"

-- | @Selector@ for @setLineCapStyle:@
setLineCapStyleSelector :: Selector
setLineCapStyleSelector = mkSelector "setLineCapStyle:"

-- | @Selector@ for @lineJoinStyle@
lineJoinStyleSelector :: Selector
lineJoinStyleSelector = mkSelector "lineJoinStyle"

-- | @Selector@ for @setLineJoinStyle:@
setLineJoinStyleSelector :: Selector
setLineJoinStyleSelector = mkSelector "setLineJoinStyle:"

-- | @Selector@ for @windingRule@
windingRuleSelector :: Selector
windingRuleSelector = mkSelector "windingRule"

-- | @Selector@ for @setWindingRule:@
setWindingRuleSelector :: Selector
setWindingRuleSelector = mkSelector "setWindingRule:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @flatness@
flatnessSelector :: Selector
flatnessSelector = mkSelector "flatness"

-- | @Selector@ for @setFlatness:@
setFlatnessSelector :: Selector
setFlatnessSelector = mkSelector "setFlatness:"

-- | @Selector@ for @bezierPathByFlatteningPath@
bezierPathByFlatteningPathSelector :: Selector
bezierPathByFlatteningPathSelector = mkSelector "bezierPathByFlatteningPath"

-- | @Selector@ for @bezierPathByReversingPath@
bezierPathByReversingPathSelector :: Selector
bezierPathByReversingPathSelector = mkSelector "bezierPathByReversingPath"

-- | @Selector@ for @empty@
emptySelector :: Selector
emptySelector = mkSelector "empty"

-- | @Selector@ for @currentPoint@
currentPointSelector :: Selector
currentPointSelector = mkSelector "currentPoint"

-- | @Selector@ for @controlPointBounds@
controlPointBoundsSelector :: Selector
controlPointBoundsSelector = mkSelector "controlPointBounds"

-- | @Selector@ for @bounds@
boundsSelector :: Selector
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

