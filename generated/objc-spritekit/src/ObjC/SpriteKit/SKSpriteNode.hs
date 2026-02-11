{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Sprite is a textured 2D node. It can be placed, rotated, scaled and animated like any other node except it draws a textured rectangle specified by the bounds and anchor point.
--
-- Sprites are used to define quad primitives with color and/or textures applied to them. See wiki for a definition of a Sprite.
--
-- Generated bindings for @SKSpriteNode@.
module ObjC.SpriteKit.SKSpriteNode
  ( SKSpriteNode
  , IsSKSpriteNode(..)
  , spriteNodeWithTexture
  , spriteNodeWithTexture_normalMap
  , spriteNodeWithImageNamed
  , spriteNodeWithImageNamed_normalMapped
  , initWithTexture
  , initWithImageNamed
  , initWithCoder
  , valueForAttributeNamed
  , setValue_forAttributeNamed
  , texture
  , setTexture
  , normalTexture
  , setNormalTexture
  , lightingBitMask
  , setLightingBitMask
  , shadowCastBitMask
  , setShadowCastBitMask
  , shadowedBitMask
  , setShadowedBitMask
  , colorBlendFactor
  , setColorBlendFactor
  , color
  , setColor
  , blendMode
  , setBlendMode
  , shader
  , setShader
  , attributeValues
  , setAttributeValues
  , spriteNodeWithTextureSelector
  , spriteNodeWithTexture_normalMapSelector
  , spriteNodeWithImageNamedSelector
  , spriteNodeWithImageNamed_normalMappedSelector
  , initWithTextureSelector
  , initWithImageNamedSelector
  , initWithCoderSelector
  , valueForAttributeNamedSelector
  , setValue_forAttributeNamedSelector
  , textureSelector
  , setTextureSelector
  , normalTextureSelector
  , setNormalTextureSelector
  , lightingBitMaskSelector
  , setLightingBitMaskSelector
  , shadowCastBitMaskSelector
  , setShadowCastBitMaskSelector
  , shadowedBitMaskSelector
  , setShadowedBitMaskSelector
  , colorBlendFactorSelector
  , setColorBlendFactorSelector
  , colorSelector
  , setColorSelector
  , blendModeSelector
  , setBlendModeSelector
  , shaderSelector
  , setShaderSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a sprite with an SKTexture and set its size to the SKTexture's pixel width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @+ spriteNodeWithTexture:@
spriteNodeWithTexture :: IsSKTexture texture => texture -> IO (Id SKSpriteNode)
spriteNodeWithTexture texture =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    withObjCPtr texture $ \raw_texture ->
      sendClassMsg cls' (mkSelector "spriteNodeWithTexture:") (retPtr retVoid) [argPtr (castPtr raw_texture :: Ptr ())] >>= retainedObject . castPtr

-- | @+ spriteNodeWithTexture:normalMap:@
spriteNodeWithTexture_normalMap :: (IsSKTexture texture, IsSKTexture normalMap) => texture -> normalMap -> IO (Id SKSpriteNode)
spriteNodeWithTexture_normalMap texture normalMap =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    withObjCPtr texture $ \raw_texture ->
      withObjCPtr normalMap $ \raw_normalMap ->
        sendClassMsg cls' (mkSelector "spriteNodeWithTexture:normalMap:") (retPtr retVoid) [argPtr (castPtr raw_texture :: Ptr ()), argPtr (castPtr raw_normalMap :: Ptr ())] >>= retainedObject . castPtr

-- | Create a sprite with an image from your app bundle (An SKTexture is created for the image and set on the sprite. Its size is set to the SKTexture's pixel width/height) The position of the sprite is (0, 0) and the texture anchored at (0.5, 0.5), so that it is offset by half the width and half the height. Thus the sprite has the texture centered about the position. If you wish to have the texture anchored at a different offset set the anchorPoint to another pair of values in the interval from 0.0 up to and including 1.0.
--
-- @name@ — is the name of an image file stored in the app bundle.
--
-- ObjC selector: @+ spriteNodeWithImageNamed:@
spriteNodeWithImageNamed :: IsNSString name => name -> IO (Id SKSpriteNode)
spriteNodeWithImageNamed name =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "spriteNodeWithImageNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ spriteNodeWithImageNamed:normalMapped:@
spriteNodeWithImageNamed_normalMapped :: IsNSString name => name -> Bool -> IO (Id SKSpriteNode)
spriteNodeWithImageNamed_normalMapped name generateNormalMap =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "spriteNodeWithImageNamed:normalMapped:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (if generateNormalMap then 1 else 0)] >>= retainedObject . castPtr

-- | Initialize a sprite with an SKTexture and set its size to the SKTexture's width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @- initWithTexture:@
initWithTexture :: (IsSKSpriteNode skSpriteNode, IsSKTexture texture) => skSpriteNode -> texture -> IO (Id SKSpriteNode)
initWithTexture skSpriteNode  texture =
withObjCPtr texture $ \raw_texture ->
    sendMsg skSpriteNode (mkSelector "initWithTexture:") (retPtr retVoid) [argPtr (castPtr raw_texture :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a sprite with an image from your app bundle (An SKTexture is created for the image and set on the sprite. Its size is set to the SKTexture's pixel width/height) The position of the sprite is (0, 0) and the texture anchored at (0.5, 0.5), so that it is offset by half the width and half the height. Thus the sprite has the texture centered about the position. If you wish to have the texture anchored at a different offset set the anchorPoint to another pair of values in the interval from 0.0 up to and including 1.0.
--
-- @name@ — the name or path of the image to load.
--
-- ObjC selector: @- initWithImageNamed:@
initWithImageNamed :: (IsSKSpriteNode skSpriteNode, IsNSString name) => skSpriteNode -> name -> IO (Id SKSpriteNode)
initWithImageNamed skSpriteNode  name =
withObjCPtr name $ \raw_name ->
    sendMsg skSpriteNode (mkSelector "initWithImageNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKSpriteNode skSpriteNode, IsNSCoder aDecoder) => skSpriteNode -> aDecoder -> IO (Id SKSpriteNode)
initWithCoder skSpriteNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skSpriteNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKSpriteNode skSpriteNode, IsNSString key) => skSpriteNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skSpriteNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skSpriteNode (mkSelector "valueForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKSpriteNode skSpriteNode, IsSKAttributeValue value, IsNSString key) => skSpriteNode -> value -> key -> IO ()
setValue_forAttributeNamed skSpriteNode  value key =
withObjCPtr value $ \raw_value ->
  withObjCPtr key $ \raw_key ->
      sendMsg skSpriteNode (mkSelector "setValue:forAttributeNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | Texture to be drawn (is stretched to fill the sprite)
--
-- ObjC selector: @- texture@
texture :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id SKTexture)
texture skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "texture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Texture to be drawn (is stretched to fill the sprite)
--
-- ObjC selector: @- setTexture:@
setTexture :: (IsSKSpriteNode skSpriteNode, IsSKTexture value) => skSpriteNode -> value -> IO ()
setTexture skSpriteNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skSpriteNode (mkSelector "setTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Texture to use for generating normals that lights use to light this sprite.
--
-- This will only be used if the sprite is lit by at least one light.
--
-- See: SKLightNode
--
-- See: lightingBitMask
--
-- ObjC selector: @- normalTexture@
normalTexture :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id SKTexture)
normalTexture skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "normalTexture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Texture to use for generating normals that lights use to light this sprite.
--
-- This will only be used if the sprite is lit by at least one light.
--
-- See: SKLightNode
--
-- See: lightingBitMask
--
-- ObjC selector: @- setNormalTexture:@
setNormalTexture :: (IsSKSpriteNode skSpriteNode, IsSKTexture value) => skSpriteNode -> value -> IO ()
setNormalTexture skSpriteNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skSpriteNode (mkSelector "setNormalTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- lightingBitMask@
lightingBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CUInt
lightingBitMask skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "lightingBitMask") retCUInt []

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- setLightingBitMask:@
setLightingBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CUInt -> IO ()
setLightingBitMask skSpriteNode  value =
  sendMsg skSpriteNode (mkSelector "setLightingBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | @- shadowCastBitMask@
shadowCastBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CUInt
shadowCastBitMask skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "shadowCastBitMask") retCUInt []

-- | @- setShadowCastBitMask:@
setShadowCastBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CUInt -> IO ()
setShadowCastBitMask skSpriteNode  value =
  sendMsg skSpriteNode (mkSelector "setShadowCastBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | @- shadowedBitMask@
shadowedBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CUInt
shadowedBitMask skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "shadowedBitMask") retCUInt []

-- | @- setShadowedBitMask:@
setShadowedBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CUInt -> IO ()
setShadowedBitMask skSpriteNode  value =
  sendMsg skSpriteNode (mkSelector "setShadowedBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | Controls the blending between the texture and the sprite's color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- colorBlendFactor@
colorBlendFactor :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CDouble
colorBlendFactor skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "colorBlendFactor") retCDouble []

-- | Controls the blending between the texture and the sprite's color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- setColorBlendFactor:@
setColorBlendFactor :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CDouble -> IO ()
setColorBlendFactor skSpriteNode  value =
  sendMsg skSpriteNode (mkSelector "setColorBlendFactor:") retVoid [argCDouble (fromIntegral value)]

-- | Base color for the sprite (If no texture is present, the color still is drawn)
--
-- ObjC selector: @- color@
color :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id NSColor)
color skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Base color for the sprite (If no texture is present, the color still is drawn)
--
-- ObjC selector: @- setColor:@
setColor :: (IsSKSpriteNode skSpriteNode, IsNSColor value) => skSpriteNode -> value -> IO ()
setColor skSpriteNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skSpriteNode (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO SKBlendMode
blendMode skSpriteNode  =
  fmap (coerce :: CLong -> SKBlendMode) $ sendMsg skSpriteNode (mkSelector "blendMode") retCLong []

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKSpriteNode skSpriteNode => skSpriteNode -> SKBlendMode -> IO ()
setBlendMode skSpriteNode  value =
  sendMsg skSpriteNode (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- | @- shader@
shader :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id SKShader)
shader skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "shader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShader:@
setShader :: (IsSKSpriteNode skSpriteNode, IsSKShader value) => skSpriteNode -> value -> IO ()
setShader skSpriteNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skSpriteNode (mkSelector "setShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id NSDictionary)
attributeValues skSpriteNode  =
  sendMsg skSpriteNode (mkSelector "attributeValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKSpriteNode skSpriteNode, IsNSDictionary value) => skSpriteNode -> value -> IO ()
setAttributeValues skSpriteNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skSpriteNode (mkSelector "setAttributeValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @spriteNodeWithTexture:@
spriteNodeWithTextureSelector :: Selector
spriteNodeWithTextureSelector = mkSelector "spriteNodeWithTexture:"

-- | @Selector@ for @spriteNodeWithTexture:normalMap:@
spriteNodeWithTexture_normalMapSelector :: Selector
spriteNodeWithTexture_normalMapSelector = mkSelector "spriteNodeWithTexture:normalMap:"

-- | @Selector@ for @spriteNodeWithImageNamed:@
spriteNodeWithImageNamedSelector :: Selector
spriteNodeWithImageNamedSelector = mkSelector "spriteNodeWithImageNamed:"

-- | @Selector@ for @spriteNodeWithImageNamed:normalMapped:@
spriteNodeWithImageNamed_normalMappedSelector :: Selector
spriteNodeWithImageNamed_normalMappedSelector = mkSelector "spriteNodeWithImageNamed:normalMapped:"

-- | @Selector@ for @initWithTexture:@
initWithTextureSelector :: Selector
initWithTextureSelector = mkSelector "initWithTexture:"

-- | @Selector@ for @initWithImageNamed:@
initWithImageNamedSelector :: Selector
initWithImageNamedSelector = mkSelector "initWithImageNamed:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @texture@
textureSelector :: Selector
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector
setTextureSelector = mkSelector "setTexture:"

-- | @Selector@ for @normalTexture@
normalTextureSelector :: Selector
normalTextureSelector = mkSelector "normalTexture"

-- | @Selector@ for @setNormalTexture:@
setNormalTextureSelector :: Selector
setNormalTextureSelector = mkSelector "setNormalTexture:"

-- | @Selector@ for @lightingBitMask@
lightingBitMaskSelector :: Selector
lightingBitMaskSelector = mkSelector "lightingBitMask"

-- | @Selector@ for @setLightingBitMask:@
setLightingBitMaskSelector :: Selector
setLightingBitMaskSelector = mkSelector "setLightingBitMask:"

-- | @Selector@ for @shadowCastBitMask@
shadowCastBitMaskSelector :: Selector
shadowCastBitMaskSelector = mkSelector "shadowCastBitMask"

-- | @Selector@ for @setShadowCastBitMask:@
setShadowCastBitMaskSelector :: Selector
setShadowCastBitMaskSelector = mkSelector "setShadowCastBitMask:"

-- | @Selector@ for @shadowedBitMask@
shadowedBitMaskSelector :: Selector
shadowedBitMaskSelector = mkSelector "shadowedBitMask"

-- | @Selector@ for @setShadowedBitMask:@
setShadowedBitMaskSelector :: Selector
setShadowedBitMaskSelector = mkSelector "setShadowedBitMask:"

-- | @Selector@ for @colorBlendFactor@
colorBlendFactorSelector :: Selector
colorBlendFactorSelector = mkSelector "colorBlendFactor"

-- | @Selector@ for @setColorBlendFactor:@
setColorBlendFactorSelector :: Selector
setColorBlendFactorSelector = mkSelector "setColorBlendFactor:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @shader@
shaderSelector :: Selector
shaderSelector = mkSelector "shader"

-- | @Selector@ for @setShader:@
setShaderSelector :: Selector
setShaderSelector = mkSelector "setShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector
setAttributeValuesSelector = mkSelector "setAttributeValues:"

