{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A SpriteKit Node used to stroke or fill a shape. CGPaths are used to supply the path.
--
-- See CGPath reference pages for details on how to construct a CGPath.
--
-- Generated bindings for @SKShapeNode@.
module ObjC.SpriteKit.SKShapeNode
  ( SKShapeNode
  , IsSKShapeNode(..)
  , shapeNodeWithPath
  , shapeNodeWithPath_centered
  , shapeNodeWithCircleOfRadius
  , shapeNodeWithPoints_count
  , shapeNodeWithSplinePoints_count
  , valueForAttributeNamed
  , setValue_forAttributeNamed
  , path
  , setPath
  , strokeColor
  , setStrokeColor
  , fillColor
  , setFillColor
  , blendMode
  , setBlendMode
  , antialiased
  , setAntialiased
  , lineWidth
  , setLineWidth
  , glowWidth
  , setGlowWidth
  , lineCap
  , setLineCap
  , lineJoin
  , setLineJoin
  , miterLimit
  , setMiterLimit
  , lineLength
  , fillTexture
  , setFillTexture
  , fillShader
  , setFillShader
  , strokeTexture
  , setStrokeTexture
  , strokeShader
  , setStrokeShader
  , attributeValues
  , setAttributeValues
  , antialiasedSelector
  , attributeValuesSelector
  , blendModeSelector
  , fillColorSelector
  , fillShaderSelector
  , fillTextureSelector
  , glowWidthSelector
  , lineCapSelector
  , lineJoinSelector
  , lineLengthSelector
  , lineWidthSelector
  , miterLimitSelector
  , pathSelector
  , setAntialiasedSelector
  , setAttributeValuesSelector
  , setBlendModeSelector
  , setFillColorSelector
  , setFillShaderSelector
  , setFillTextureSelector
  , setGlowWidthSelector
  , setLineCapSelector
  , setLineJoinSelector
  , setLineWidthSelector
  , setMiterLimitSelector
  , setPathSelector
  , setStrokeColorSelector
  , setStrokeShaderSelector
  , setStrokeTextureSelector
  , setValue_forAttributeNamedSelector
  , shapeNodeWithCircleOfRadiusSelector
  , shapeNodeWithPathSelector
  , shapeNodeWithPath_centeredSelector
  , shapeNodeWithPoints_countSelector
  , shapeNodeWithSplinePoints_countSelector
  , strokeColorSelector
  , strokeShaderSelector
  , strokeTextureSelector
  , valueForAttributeNamedSelector

  -- * Enum types
  , SKBlendMode(SKBlendMode)
  , pattern SKBlendModeAlpha
  , pattern SKBlendModeAdd
  , pattern SKBlendModeSubtract
  , pattern SKBlendModeMultiply
  , pattern SKBlendModeMultiplyX2
  , pattern SKBlendModeScreen
  , pattern SKBlendModeReplace
  , pattern SKBlendModeMultiplyAlpha

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ shapeNodeWithPath:@
shapeNodeWithPath :: RawId -> IO (Id SKShapeNode)
shapeNodeWithPath path =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMessage cls' shapeNodeWithPathSelector path

-- | @+ shapeNodeWithPath:centered:@
shapeNodeWithPath_centered :: RawId -> Bool -> IO (Id SKShapeNode)
shapeNodeWithPath_centered path centered =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMessage cls' shapeNodeWithPath_centeredSelector path centered

-- | @+ shapeNodeWithCircleOfRadius:@
shapeNodeWithCircleOfRadius :: CDouble -> IO (Id SKShapeNode)
shapeNodeWithCircleOfRadius radius =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMessage cls' shapeNodeWithCircleOfRadiusSelector radius

-- | @+ shapeNodeWithPoints:count:@
shapeNodeWithPoints_count :: RawId -> CULong -> IO (Id SKShapeNode)
shapeNodeWithPoints_count points numPoints =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMessage cls' shapeNodeWithPoints_countSelector points numPoints

-- | @+ shapeNodeWithSplinePoints:count:@
shapeNodeWithSplinePoints_count :: RawId -> CULong -> IO (Id SKShapeNode)
shapeNodeWithSplinePoints_count points numPoints =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMessage cls' shapeNodeWithSplinePoints_countSelector points numPoints

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKShapeNode skShapeNode, IsNSString key) => skShapeNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skShapeNode key =
  sendMessage skShapeNode valueForAttributeNamedSelector (toNSString key)

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKShapeNode skShapeNode, IsSKAttributeValue value, IsNSString key) => skShapeNode -> value -> key -> IO ()
setValue_forAttributeNamed skShapeNode value key =
  sendMessage skShapeNode setValue_forAttributeNamedSelector (toSKAttributeValue value) (toNSString key)

-- | The CGPath to be drawn (in the Node's coordinate space)
--
-- ObjC selector: @- path@
path :: IsSKShapeNode skShapeNode => skShapeNode -> IO RawId
path skShapeNode =
  sendMessage skShapeNode pathSelector

-- | The CGPath to be drawn (in the Node's coordinate space)
--
-- ObjC selector: @- setPath:@
setPath :: IsSKShapeNode skShapeNode => skShapeNode -> RawId -> IO ()
setPath skShapeNode value =
  sendMessage skShapeNode setPathSelector value

-- | The color to draw the path with. (for no stroke use [SKColor clearColor]). Defaults to [SKColor whiteColor].
--
-- ObjC selector: @- strokeColor@
strokeColor :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id NSColor)
strokeColor skShapeNode =
  sendMessage skShapeNode strokeColorSelector

-- | The color to draw the path with. (for no stroke use [SKColor clearColor]). Defaults to [SKColor whiteColor].
--
-- ObjC selector: @- setStrokeColor:@
setStrokeColor :: (IsSKShapeNode skShapeNode, IsNSColor value) => skShapeNode -> value -> IO ()
setStrokeColor skShapeNode value =
  sendMessage skShapeNode setStrokeColorSelector (toNSColor value)

-- | The color to fill the path with. Defaults to [SKColor clearColor] (no fill).
--
-- ObjC selector: @- fillColor@
fillColor :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id NSColor)
fillColor skShapeNode =
  sendMessage skShapeNode fillColorSelector

-- | The color to fill the path with. Defaults to [SKColor clearColor] (no fill).
--
-- ObjC selector: @- setFillColor:@
setFillColor :: (IsSKShapeNode skShapeNode, IsNSColor value) => skShapeNode -> value -> IO ()
setFillColor skShapeNode value =
  sendMessage skShapeNode setFillColorSelector (toNSColor value)

-- | Sets the blend mode to use when composing the shape with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKShapeNode skShapeNode => skShapeNode -> IO SKBlendMode
blendMode skShapeNode =
  sendMessage skShapeNode blendModeSelector

-- | Sets the blend mode to use when composing the shape with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKShapeNode skShapeNode => skShapeNode -> SKBlendMode -> IO ()
setBlendMode skShapeNode value =
  sendMessage skShapeNode setBlendModeSelector value

-- | If set to YES, the path stroke edges and caps is smoothed (antialiased) when drawn.
--
-- ObjC selector: @- antialiased@
antialiased :: IsSKShapeNode skShapeNode => skShapeNode -> IO Bool
antialiased skShapeNode =
  sendMessage skShapeNode antialiasedSelector

-- | If set to YES, the path stroke edges and caps is smoothed (antialiased) when drawn.
--
-- ObjC selector: @- setAntialiased:@
setAntialiased :: IsSKShapeNode skShapeNode => skShapeNode -> Bool -> IO ()
setAntialiased skShapeNode value =
  sendMessage skShapeNode setAntialiasedSelector value

-- | The width used to stroke the path. Widths larger than 2.0 may result in artifacts. Defaults to 1.0.
--
-- ObjC selector: @- lineWidth@
lineWidth :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
lineWidth skShapeNode =
  sendMessage skShapeNode lineWidthSelector

-- | The width used to stroke the path. Widths larger than 2.0 may result in artifacts. Defaults to 1.0.
--
-- ObjC selector: @- setLineWidth:@
setLineWidth :: IsSKShapeNode skShapeNode => skShapeNode -> CDouble -> IO ()
setLineWidth skShapeNode value =
  sendMessage skShapeNode setLineWidthSelector value

-- | Add a glow to the path stroke of the specified width. Defaults to 0.0 (no glow)
--
-- ObjC selector: @- glowWidth@
glowWidth :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
glowWidth skShapeNode =
  sendMessage skShapeNode glowWidthSelector

-- | Add a glow to the path stroke of the specified width. Defaults to 0.0 (no glow)
--
-- ObjC selector: @- setGlowWidth:@
setGlowWidth :: IsSKShapeNode skShapeNode => skShapeNode -> CDouble -> IO ()
setGlowWidth skShapeNode value =
  sendMessage skShapeNode setGlowWidthSelector value

-- | The cap type that should be used when stroking a non-closed path
--
-- ObjC selector: @- lineCap@
lineCap :: IsSKShapeNode skShapeNode => skShapeNode -> IO CInt
lineCap skShapeNode =
  sendMessage skShapeNode lineCapSelector

-- | The cap type that should be used when stroking a non-closed path
--
-- ObjC selector: @- setLineCap:@
setLineCap :: IsSKShapeNode skShapeNode => skShapeNode -> CInt -> IO ()
setLineCap skShapeNode value =
  sendMessage skShapeNode setLineCapSelector value

-- | The join type that should be used when stroking a path
--
-- ObjC selector: @- lineJoin@
lineJoin :: IsSKShapeNode skShapeNode => skShapeNode -> IO CInt
lineJoin skShapeNode =
  sendMessage skShapeNode lineJoinSelector

-- | The join type that should be used when stroking a path
--
-- ObjC selector: @- setLineJoin:@
setLineJoin :: IsSKShapeNode skShapeNode => skShapeNode -> CInt -> IO ()
setLineJoin skShapeNode value =
  sendMessage skShapeNode setLineJoinSelector value

-- | When a miter join is used, the maximum ratio of miter length to line with to be used
--
-- ObjC selector: @- miterLimit@
miterLimit :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
miterLimit skShapeNode =
  sendMessage skShapeNode miterLimitSelector

-- | When a miter join is used, the maximum ratio of miter length to line with to be used
--
-- ObjC selector: @- setMiterLimit:@
setMiterLimit :: IsSKShapeNode skShapeNode => skShapeNode -> CDouble -> IO ()
setMiterLimit skShapeNode value =
  sendMessage skShapeNode setMiterLimitSelector value

-- | The length of the node's path if it were to be stroked
--
-- ObjC selector: @- lineLength@
lineLength :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
lineLength skShapeNode =
  sendMessage skShapeNode lineLengthSelector

-- | @- fillTexture@
fillTexture :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKTexture)
fillTexture skShapeNode =
  sendMessage skShapeNode fillTextureSelector

-- | @- setFillTexture:@
setFillTexture :: (IsSKShapeNode skShapeNode, IsSKTexture value) => skShapeNode -> value -> IO ()
setFillTexture skShapeNode value =
  sendMessage skShapeNode setFillTextureSelector (toSKTexture value)

-- | @- fillShader@
fillShader :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKShader)
fillShader skShapeNode =
  sendMessage skShapeNode fillShaderSelector

-- | @- setFillShader:@
setFillShader :: (IsSKShapeNode skShapeNode, IsSKShader value) => skShapeNode -> value -> IO ()
setFillShader skShapeNode value =
  sendMessage skShapeNode setFillShaderSelector (toSKShader value)

-- | @- strokeTexture@
strokeTexture :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKTexture)
strokeTexture skShapeNode =
  sendMessage skShapeNode strokeTextureSelector

-- | @- setStrokeTexture:@
setStrokeTexture :: (IsSKShapeNode skShapeNode, IsSKTexture value) => skShapeNode -> value -> IO ()
setStrokeTexture skShapeNode value =
  sendMessage skShapeNode setStrokeTextureSelector (toSKTexture value)

-- | @- strokeShader@
strokeShader :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKShader)
strokeShader skShapeNode =
  sendMessage skShapeNode strokeShaderSelector

-- | @- setStrokeShader:@
setStrokeShader :: (IsSKShapeNode skShapeNode, IsSKShader value) => skShapeNode -> value -> IO ()
setStrokeShader skShapeNode value =
  sendMessage skShapeNode setStrokeShaderSelector (toSKShader value)

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id NSDictionary)
attributeValues skShapeNode =
  sendMessage skShapeNode attributeValuesSelector

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKShapeNode skShapeNode, IsNSDictionary value) => skShapeNode -> value -> IO ()
setAttributeValues skShapeNode value =
  sendMessage skShapeNode setAttributeValuesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shapeNodeWithPath:@
shapeNodeWithPathSelector :: Selector '[RawId] (Id SKShapeNode)
shapeNodeWithPathSelector = mkSelector "shapeNodeWithPath:"

-- | @Selector@ for @shapeNodeWithPath:centered:@
shapeNodeWithPath_centeredSelector :: Selector '[RawId, Bool] (Id SKShapeNode)
shapeNodeWithPath_centeredSelector = mkSelector "shapeNodeWithPath:centered:"

-- | @Selector@ for @shapeNodeWithCircleOfRadius:@
shapeNodeWithCircleOfRadiusSelector :: Selector '[CDouble] (Id SKShapeNode)
shapeNodeWithCircleOfRadiusSelector = mkSelector "shapeNodeWithCircleOfRadius:"

-- | @Selector@ for @shapeNodeWithPoints:count:@
shapeNodeWithPoints_countSelector :: Selector '[RawId, CULong] (Id SKShapeNode)
shapeNodeWithPoints_countSelector = mkSelector "shapeNodeWithPoints:count:"

-- | @Selector@ for @shapeNodeWithSplinePoints:count:@
shapeNodeWithSplinePoints_countSelector :: Selector '[RawId, CULong] (Id SKShapeNode)
shapeNodeWithSplinePoints_countSelector = mkSelector "shapeNodeWithSplinePoints:count:"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector '[Id NSString] (Id SKAttributeValue)
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector '[Id SKAttributeValue, Id NSString] ()
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] RawId
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[RawId] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @strokeColor@
strokeColorSelector :: Selector '[] (Id NSColor)
strokeColorSelector = mkSelector "strokeColor"

-- | @Selector@ for @setStrokeColor:@
setStrokeColorSelector :: Selector '[Id NSColor] ()
setStrokeColorSelector = mkSelector "setStrokeColor:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector '[] (Id NSColor)
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector '[Id NSColor] ()
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] SKBlendMode
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[SKBlendMode] ()
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @antialiased@
antialiasedSelector :: Selector '[] Bool
antialiasedSelector = mkSelector "antialiased"

-- | @Selector@ for @setAntialiased:@
setAntialiasedSelector :: Selector '[Bool] ()
setAntialiasedSelector = mkSelector "setAntialiased:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector '[] CDouble
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector '[CDouble] ()
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @glowWidth@
glowWidthSelector :: Selector '[] CDouble
glowWidthSelector = mkSelector "glowWidth"

-- | @Selector@ for @setGlowWidth:@
setGlowWidthSelector :: Selector '[CDouble] ()
setGlowWidthSelector = mkSelector "setGlowWidth:"

-- | @Selector@ for @lineCap@
lineCapSelector :: Selector '[] CInt
lineCapSelector = mkSelector "lineCap"

-- | @Selector@ for @setLineCap:@
setLineCapSelector :: Selector '[CInt] ()
setLineCapSelector = mkSelector "setLineCap:"

-- | @Selector@ for @lineJoin@
lineJoinSelector :: Selector '[] CInt
lineJoinSelector = mkSelector "lineJoin"

-- | @Selector@ for @setLineJoin:@
setLineJoinSelector :: Selector '[CInt] ()
setLineJoinSelector = mkSelector "setLineJoin:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector '[] CDouble
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector '[CDouble] ()
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @lineLength@
lineLengthSelector :: Selector '[] CDouble
lineLengthSelector = mkSelector "lineLength"

-- | @Selector@ for @fillTexture@
fillTextureSelector :: Selector '[] (Id SKTexture)
fillTextureSelector = mkSelector "fillTexture"

-- | @Selector@ for @setFillTexture:@
setFillTextureSelector :: Selector '[Id SKTexture] ()
setFillTextureSelector = mkSelector "setFillTexture:"

-- | @Selector@ for @fillShader@
fillShaderSelector :: Selector '[] (Id SKShader)
fillShaderSelector = mkSelector "fillShader"

-- | @Selector@ for @setFillShader:@
setFillShaderSelector :: Selector '[Id SKShader] ()
setFillShaderSelector = mkSelector "setFillShader:"

-- | @Selector@ for @strokeTexture@
strokeTextureSelector :: Selector '[] (Id SKTexture)
strokeTextureSelector = mkSelector "strokeTexture"

-- | @Selector@ for @setStrokeTexture:@
setStrokeTextureSelector :: Selector '[Id SKTexture] ()
setStrokeTextureSelector = mkSelector "setStrokeTexture:"

-- | @Selector@ for @strokeShader@
strokeShaderSelector :: Selector '[] (Id SKShader)
strokeShaderSelector = mkSelector "strokeShader"

-- | @Selector@ for @setStrokeShader:@
setStrokeShaderSelector :: Selector '[Id SKShader] ()
setStrokeShaderSelector = mkSelector "setStrokeShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector '[] (Id NSDictionary)
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector '[Id NSDictionary] ()
setAttributeValuesSelector = mkSelector "setAttributeValues:"

