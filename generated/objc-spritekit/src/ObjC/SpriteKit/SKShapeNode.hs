{-# LANGUAGE PatternSynonyms #-}
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
  , shapeNodeWithPathSelector
  , shapeNodeWithPath_centeredSelector
  , shapeNodeWithCircleOfRadiusSelector
  , valueForAttributeNamedSelector
  , setValue_forAttributeNamedSelector
  , pathSelector
  , setPathSelector
  , strokeColorSelector
  , setStrokeColorSelector
  , fillColorSelector
  , setFillColorSelector
  , blendModeSelector
  , setBlendModeSelector
  , antialiasedSelector
  , setAntialiasedSelector
  , lineWidthSelector
  , setLineWidthSelector
  , glowWidthSelector
  , setGlowWidthSelector
  , lineCapSelector
  , setLineCapSelector
  , lineJoinSelector
  , setLineJoinSelector
  , miterLimitSelector
  , setMiterLimitSelector
  , lineLengthSelector
  , fillTextureSelector
  , setFillTextureSelector
  , fillShaderSelector
  , setFillShaderSelector
  , strokeTextureSelector
  , setStrokeTextureSelector
  , strokeShaderSelector
  , setStrokeShaderSelector
  , attributeValuesSelector
  , setAttributeValuesSelector

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

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ shapeNodeWithPath:@
shapeNodeWithPath :: RawId -> IO (Id SKShapeNode)
shapeNodeWithPath path =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMsg cls' (mkSelector "shapeNodeWithPath:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ shapeNodeWithPath:centered:@
shapeNodeWithPath_centered :: RawId -> Bool -> IO (Id SKShapeNode)
shapeNodeWithPath_centered path centered =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMsg cls' (mkSelector "shapeNodeWithPath:centered:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ()), argCULong (if centered then 1 else 0)] >>= retainedObject . castPtr

-- | @+ shapeNodeWithCircleOfRadius:@
shapeNodeWithCircleOfRadius :: CDouble -> IO (Id SKShapeNode)
shapeNodeWithCircleOfRadius radius =
  do
    cls' <- getRequiredClass "SKShapeNode"
    sendClassMsg cls' (mkSelector "shapeNodeWithCircleOfRadius:") (retPtr retVoid) [argCDouble (fromIntegral radius)] >>= retainedObject . castPtr

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKShapeNode skShapeNode, IsNSString key) => skShapeNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skShapeNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skShapeNode (mkSelector "valueForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKShapeNode skShapeNode, IsSKAttributeValue value, IsNSString key) => skShapeNode -> value -> key -> IO ()
setValue_forAttributeNamed skShapeNode  value key =
withObjCPtr value $ \raw_value ->
  withObjCPtr key $ \raw_key ->
      sendMsg skShapeNode (mkSelector "setValue:forAttributeNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | The CGPath to be drawn (in the Node's coordinate space)
--
-- ObjC selector: @- path@
path :: IsSKShapeNode skShapeNode => skShapeNode -> IO RawId
path skShapeNode  =
  fmap (RawId . castPtr) $ sendMsg skShapeNode (mkSelector "path") (retPtr retVoid) []

-- | The CGPath to be drawn (in the Node's coordinate space)
--
-- ObjC selector: @- setPath:@
setPath :: IsSKShapeNode skShapeNode => skShapeNode -> RawId -> IO ()
setPath skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The color to draw the path with. (for no stroke use [SKColor clearColor]). Defaults to [SKColor whiteColor].
--
-- ObjC selector: @- strokeColor@
strokeColor :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id NSColor)
strokeColor skShapeNode  =
  sendMsg skShapeNode (mkSelector "strokeColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The color to draw the path with. (for no stroke use [SKColor clearColor]). Defaults to [SKColor whiteColor].
--
-- ObjC selector: @- setStrokeColor:@
setStrokeColor :: (IsSKShapeNode skShapeNode, IsNSColor value) => skShapeNode -> value -> IO ()
setStrokeColor skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setStrokeColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The color to fill the path with. Defaults to [SKColor clearColor] (no fill).
--
-- ObjC selector: @- fillColor@
fillColor :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id NSColor)
fillColor skShapeNode  =
  sendMsg skShapeNode (mkSelector "fillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The color to fill the path with. Defaults to [SKColor clearColor] (no fill).
--
-- ObjC selector: @- setFillColor:@
setFillColor :: (IsSKShapeNode skShapeNode, IsNSColor value) => skShapeNode -> value -> IO ()
setFillColor skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setFillColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the blend mode to use when composing the shape with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKShapeNode skShapeNode => skShapeNode -> IO SKBlendMode
blendMode skShapeNode  =
  fmap (coerce :: CLong -> SKBlendMode) $ sendMsg skShapeNode (mkSelector "blendMode") retCLong []

-- | Sets the blend mode to use when composing the shape with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKShapeNode skShapeNode => skShapeNode -> SKBlendMode -> IO ()
setBlendMode skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- | If set to YES, the path stroke edges and caps is smoothed (antialiased) when drawn.
--
-- ObjC selector: @- antialiased@
antialiased :: IsSKShapeNode skShapeNode => skShapeNode -> IO Bool
antialiased skShapeNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skShapeNode (mkSelector "antialiased") retCULong []

-- | If set to YES, the path stroke edges and caps is smoothed (antialiased) when drawn.
--
-- ObjC selector: @- setAntialiased:@
setAntialiased :: IsSKShapeNode skShapeNode => skShapeNode -> Bool -> IO ()
setAntialiased skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setAntialiased:") retVoid [argCULong (if value then 1 else 0)]

-- | The width used to stroke the path. Widths larger than 2.0 may result in artifacts. Defaults to 1.0.
--
-- ObjC selector: @- lineWidth@
lineWidth :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
lineWidth skShapeNode  =
  sendMsg skShapeNode (mkSelector "lineWidth") retCDouble []

-- | The width used to stroke the path. Widths larger than 2.0 may result in artifacts. Defaults to 1.0.
--
-- ObjC selector: @- setLineWidth:@
setLineWidth :: IsSKShapeNode skShapeNode => skShapeNode -> CDouble -> IO ()
setLineWidth skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setLineWidth:") retVoid [argCDouble (fromIntegral value)]

-- | Add a glow to the path stroke of the specified width. Defaults to 0.0 (no glow)
--
-- ObjC selector: @- glowWidth@
glowWidth :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
glowWidth skShapeNode  =
  sendMsg skShapeNode (mkSelector "glowWidth") retCDouble []

-- | Add a glow to the path stroke of the specified width. Defaults to 0.0 (no glow)
--
-- ObjC selector: @- setGlowWidth:@
setGlowWidth :: IsSKShapeNode skShapeNode => skShapeNode -> CDouble -> IO ()
setGlowWidth skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setGlowWidth:") retVoid [argCDouble (fromIntegral value)]

-- | The cap type that should be used when stroking a non-closed path
--
-- ObjC selector: @- lineCap@
lineCap :: IsSKShapeNode skShapeNode => skShapeNode -> IO CGLineCap
lineCap skShapeNode  =
  fmap (coerce :: CInt -> CGLineCap) $ sendMsg skShapeNode (mkSelector "lineCap") retCInt []

-- | The cap type that should be used when stroking a non-closed path
--
-- ObjC selector: @- setLineCap:@
setLineCap :: IsSKShapeNode skShapeNode => skShapeNode -> CGLineCap -> IO ()
setLineCap skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setLineCap:") retVoid [argCInt (coerce value)]

-- | The join type that should be used when stroking a path
--
-- ObjC selector: @- lineJoin@
lineJoin :: IsSKShapeNode skShapeNode => skShapeNode -> IO CGLineJoin
lineJoin skShapeNode  =
  fmap (coerce :: CInt -> CGLineJoin) $ sendMsg skShapeNode (mkSelector "lineJoin") retCInt []

-- | The join type that should be used when stroking a path
--
-- ObjC selector: @- setLineJoin:@
setLineJoin :: IsSKShapeNode skShapeNode => skShapeNode -> CGLineJoin -> IO ()
setLineJoin skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setLineJoin:") retVoid [argCInt (coerce value)]

-- | When a miter join is used, the maximum ratio of miter length to line with to be used
--
-- ObjC selector: @- miterLimit@
miterLimit :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
miterLimit skShapeNode  =
  sendMsg skShapeNode (mkSelector "miterLimit") retCDouble []

-- | When a miter join is used, the maximum ratio of miter length to line with to be used
--
-- ObjC selector: @- setMiterLimit:@
setMiterLimit :: IsSKShapeNode skShapeNode => skShapeNode -> CDouble -> IO ()
setMiterLimit skShapeNode  value =
  sendMsg skShapeNode (mkSelector "setMiterLimit:") retVoid [argCDouble (fromIntegral value)]

-- | The length of the node's path if it were to be stroked
--
-- ObjC selector: @- lineLength@
lineLength :: IsSKShapeNode skShapeNode => skShapeNode -> IO CDouble
lineLength skShapeNode  =
  sendMsg skShapeNode (mkSelector "lineLength") retCDouble []

-- | @- fillTexture@
fillTexture :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKTexture)
fillTexture skShapeNode  =
  sendMsg skShapeNode (mkSelector "fillTexture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFillTexture:@
setFillTexture :: (IsSKShapeNode skShapeNode, IsSKTexture value) => skShapeNode -> value -> IO ()
setFillTexture skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setFillTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fillShader@
fillShader :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKShader)
fillShader skShapeNode  =
  sendMsg skShapeNode (mkSelector "fillShader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFillShader:@
setFillShader :: (IsSKShapeNode skShapeNode, IsSKShader value) => skShapeNode -> value -> IO ()
setFillShader skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setFillShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- strokeTexture@
strokeTexture :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKTexture)
strokeTexture skShapeNode  =
  sendMsg skShapeNode (mkSelector "strokeTexture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStrokeTexture:@
setStrokeTexture :: (IsSKShapeNode skShapeNode, IsSKTexture value) => skShapeNode -> value -> IO ()
setStrokeTexture skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setStrokeTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- strokeShader@
strokeShader :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id SKShader)
strokeShader skShapeNode  =
  sendMsg skShapeNode (mkSelector "strokeShader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStrokeShader:@
setStrokeShader :: (IsSKShapeNode skShapeNode, IsSKShader value) => skShapeNode -> value -> IO ()
setStrokeShader skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setStrokeShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKShapeNode skShapeNode => skShapeNode -> IO (Id NSDictionary)
attributeValues skShapeNode  =
  sendMsg skShapeNode (mkSelector "attributeValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKShapeNode skShapeNode, IsNSDictionary value) => skShapeNode -> value -> IO ()
setAttributeValues skShapeNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShapeNode (mkSelector "setAttributeValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shapeNodeWithPath:@
shapeNodeWithPathSelector :: Selector
shapeNodeWithPathSelector = mkSelector "shapeNodeWithPath:"

-- | @Selector@ for @shapeNodeWithPath:centered:@
shapeNodeWithPath_centeredSelector :: Selector
shapeNodeWithPath_centeredSelector = mkSelector "shapeNodeWithPath:centered:"

-- | @Selector@ for @shapeNodeWithCircleOfRadius:@
shapeNodeWithCircleOfRadiusSelector :: Selector
shapeNodeWithCircleOfRadiusSelector = mkSelector "shapeNodeWithCircleOfRadius:"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @strokeColor@
strokeColorSelector :: Selector
strokeColorSelector = mkSelector "strokeColor"

-- | @Selector@ for @setStrokeColor:@
setStrokeColorSelector :: Selector
setStrokeColorSelector = mkSelector "setStrokeColor:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @antialiased@
antialiasedSelector :: Selector
antialiasedSelector = mkSelector "antialiased"

-- | @Selector@ for @setAntialiased:@
setAntialiasedSelector :: Selector
setAntialiasedSelector = mkSelector "setAntialiased:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @glowWidth@
glowWidthSelector :: Selector
glowWidthSelector = mkSelector "glowWidth"

-- | @Selector@ for @setGlowWidth:@
setGlowWidthSelector :: Selector
setGlowWidthSelector = mkSelector "setGlowWidth:"

-- | @Selector@ for @lineCap@
lineCapSelector :: Selector
lineCapSelector = mkSelector "lineCap"

-- | @Selector@ for @setLineCap:@
setLineCapSelector :: Selector
setLineCapSelector = mkSelector "setLineCap:"

-- | @Selector@ for @lineJoin@
lineJoinSelector :: Selector
lineJoinSelector = mkSelector "lineJoin"

-- | @Selector@ for @setLineJoin:@
setLineJoinSelector :: Selector
setLineJoinSelector = mkSelector "setLineJoin:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @lineLength@
lineLengthSelector :: Selector
lineLengthSelector = mkSelector "lineLength"

-- | @Selector@ for @fillTexture@
fillTextureSelector :: Selector
fillTextureSelector = mkSelector "fillTexture"

-- | @Selector@ for @setFillTexture:@
setFillTextureSelector :: Selector
setFillTextureSelector = mkSelector "setFillTexture:"

-- | @Selector@ for @fillShader@
fillShaderSelector :: Selector
fillShaderSelector = mkSelector "fillShader"

-- | @Selector@ for @setFillShader:@
setFillShaderSelector :: Selector
setFillShaderSelector = mkSelector "setFillShader:"

-- | @Selector@ for @strokeTexture@
strokeTextureSelector :: Selector
strokeTextureSelector = mkSelector "strokeTexture"

-- | @Selector@ for @setStrokeTexture:@
setStrokeTextureSelector :: Selector
setStrokeTextureSelector = mkSelector "setStrokeTexture:"

-- | @Selector@ for @strokeShader@
strokeShaderSelector :: Selector
strokeShaderSelector = mkSelector "strokeShader"

-- | @Selector@ for @setStrokeShader:@
setStrokeShaderSelector :: Selector
setStrokeShaderSelector = mkSelector "setStrokeShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector
setAttributeValuesSelector = mkSelector "setAttributeValues:"

