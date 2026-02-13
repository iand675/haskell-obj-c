{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attributeValuesSelector
  , blendModeSelector
  , colorBlendFactorSelector
  , colorSelector
  , initWithCoderSelector
  , initWithImageNamedSelector
  , initWithTextureSelector
  , lightingBitMaskSelector
  , normalTextureSelector
  , setAttributeValuesSelector
  , setBlendModeSelector
  , setColorBlendFactorSelector
  , setColorSelector
  , setLightingBitMaskSelector
  , setNormalTextureSelector
  , setShaderSelector
  , setShadowCastBitMaskSelector
  , setShadowedBitMaskSelector
  , setTextureSelector
  , setValue_forAttributeNamedSelector
  , shaderSelector
  , shadowCastBitMaskSelector
  , shadowedBitMaskSelector
  , spriteNodeWithImageNamedSelector
  , spriteNodeWithImageNamed_normalMappedSelector
  , spriteNodeWithTextureSelector
  , spriteNodeWithTexture_normalMapSelector
  , textureSelector
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

-- | Create a sprite with an SKTexture and set its size to the SKTexture's pixel width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @+ spriteNodeWithTexture:@
spriteNodeWithTexture :: IsSKTexture texture => texture -> IO (Id SKSpriteNode)
spriteNodeWithTexture texture =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    sendClassMessage cls' spriteNodeWithTextureSelector (toSKTexture texture)

-- | @+ spriteNodeWithTexture:normalMap:@
spriteNodeWithTexture_normalMap :: (IsSKTexture texture, IsSKTexture normalMap) => texture -> normalMap -> IO (Id SKSpriteNode)
spriteNodeWithTexture_normalMap texture normalMap =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    sendClassMessage cls' spriteNodeWithTexture_normalMapSelector (toSKTexture texture) (toSKTexture normalMap)

-- | Create a sprite with an image from your app bundle (An SKTexture is created for the image and set on the sprite. Its size is set to the SKTexture's pixel width/height) The position of the sprite is (0, 0) and the texture anchored at (0.5, 0.5), so that it is offset by half the width and half the height. Thus the sprite has the texture centered about the position. If you wish to have the texture anchored at a different offset set the anchorPoint to another pair of values in the interval from 0.0 up to and including 1.0.
--
-- @name@ — is the name of an image file stored in the app bundle.
--
-- ObjC selector: @+ spriteNodeWithImageNamed:@
spriteNodeWithImageNamed :: IsNSString name => name -> IO (Id SKSpriteNode)
spriteNodeWithImageNamed name =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    sendClassMessage cls' spriteNodeWithImageNamedSelector (toNSString name)

-- | @+ spriteNodeWithImageNamed:normalMapped:@
spriteNodeWithImageNamed_normalMapped :: IsNSString name => name -> Bool -> IO (Id SKSpriteNode)
spriteNodeWithImageNamed_normalMapped name generateNormalMap =
  do
    cls' <- getRequiredClass "SKSpriteNode"
    sendClassMessage cls' spriteNodeWithImageNamed_normalMappedSelector (toNSString name) generateNormalMap

-- | Initialize a sprite with an SKTexture and set its size to the SKTexture's width/height.
--
-- @texture@ — the texture to reference for size and content
--
-- ObjC selector: @- initWithTexture:@
initWithTexture :: (IsSKSpriteNode skSpriteNode, IsSKTexture texture) => skSpriteNode -> texture -> IO (Id SKSpriteNode)
initWithTexture skSpriteNode texture =
  sendOwnedMessage skSpriteNode initWithTextureSelector (toSKTexture texture)

-- | Initialize a sprite with an image from your app bundle (An SKTexture is created for the image and set on the sprite. Its size is set to the SKTexture's pixel width/height) The position of the sprite is (0, 0) and the texture anchored at (0.5, 0.5), so that it is offset by half the width and half the height. Thus the sprite has the texture centered about the position. If you wish to have the texture anchored at a different offset set the anchorPoint to another pair of values in the interval from 0.0 up to and including 1.0.
--
-- @name@ — the name or path of the image to load.
--
-- ObjC selector: @- initWithImageNamed:@
initWithImageNamed :: (IsSKSpriteNode skSpriteNode, IsNSString name) => skSpriteNode -> name -> IO (Id SKSpriteNode)
initWithImageNamed skSpriteNode name =
  sendOwnedMessage skSpriteNode initWithImageNamedSelector (toNSString name)

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKSpriteNode skSpriteNode, IsNSCoder aDecoder) => skSpriteNode -> aDecoder -> IO (Id SKSpriteNode)
initWithCoder skSpriteNode aDecoder =
  sendOwnedMessage skSpriteNode initWithCoderSelector (toNSCoder aDecoder)

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKSpriteNode skSpriteNode, IsNSString key) => skSpriteNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skSpriteNode key =
  sendMessage skSpriteNode valueForAttributeNamedSelector (toNSString key)

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKSpriteNode skSpriteNode, IsSKAttributeValue value, IsNSString key) => skSpriteNode -> value -> key -> IO ()
setValue_forAttributeNamed skSpriteNode value key =
  sendMessage skSpriteNode setValue_forAttributeNamedSelector (toSKAttributeValue value) (toNSString key)

-- | Texture to be drawn (is stretched to fill the sprite)
--
-- ObjC selector: @- texture@
texture :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id SKTexture)
texture skSpriteNode =
  sendMessage skSpriteNode textureSelector

-- | Texture to be drawn (is stretched to fill the sprite)
--
-- ObjC selector: @- setTexture:@
setTexture :: (IsSKSpriteNode skSpriteNode, IsSKTexture value) => skSpriteNode -> value -> IO ()
setTexture skSpriteNode value =
  sendMessage skSpriteNode setTextureSelector (toSKTexture value)

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
normalTexture skSpriteNode =
  sendMessage skSpriteNode normalTextureSelector

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
setNormalTexture skSpriteNode value =
  sendMessage skSpriteNode setNormalTextureSelector (toSKTexture value)

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- lightingBitMask@
lightingBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CUInt
lightingBitMask skSpriteNode =
  sendMessage skSpriteNode lightingBitMaskSelector

-- | Bitmask to indicate being lit by a set of lights using overlapping lighting categories.
--
-- A light whose category is set to a value that masks to non-zero using this mask will apply light to this sprite.
--
-- When used together with a normal texture, complex lighting effects can be used.
--
-- ObjC selector: @- setLightingBitMask:@
setLightingBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CUInt -> IO ()
setLightingBitMask skSpriteNode value =
  sendMessage skSpriteNode setLightingBitMaskSelector value

-- | @- shadowCastBitMask@
shadowCastBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CUInt
shadowCastBitMask skSpriteNode =
  sendMessage skSpriteNode shadowCastBitMaskSelector

-- | @- setShadowCastBitMask:@
setShadowCastBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CUInt -> IO ()
setShadowCastBitMask skSpriteNode value =
  sendMessage skSpriteNode setShadowCastBitMaskSelector value

-- | @- shadowedBitMask@
shadowedBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CUInt
shadowedBitMask skSpriteNode =
  sendMessage skSpriteNode shadowedBitMaskSelector

-- | @- setShadowedBitMask:@
setShadowedBitMask :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CUInt -> IO ()
setShadowedBitMask skSpriteNode value =
  sendMessage skSpriteNode setShadowedBitMaskSelector value

-- | Controls the blending between the texture and the sprite's color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- colorBlendFactor@
colorBlendFactor :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO CDouble
colorBlendFactor skSpriteNode =
  sendMessage skSpriteNode colorBlendFactorSelector

-- | Controls the blending between the texture and the sprite's color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- setColorBlendFactor:@
setColorBlendFactor :: IsSKSpriteNode skSpriteNode => skSpriteNode -> CDouble -> IO ()
setColorBlendFactor skSpriteNode value =
  sendMessage skSpriteNode setColorBlendFactorSelector value

-- | Base color for the sprite (If no texture is present, the color still is drawn)
--
-- ObjC selector: @- color@
color :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id NSColor)
color skSpriteNode =
  sendMessage skSpriteNode colorSelector

-- | Base color for the sprite (If no texture is present, the color still is drawn)
--
-- ObjC selector: @- setColor:@
setColor :: (IsSKSpriteNode skSpriteNode, IsNSColor value) => skSpriteNode -> value -> IO ()
setColor skSpriteNode value =
  sendMessage skSpriteNode setColorSelector (toNSColor value)

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO SKBlendMode
blendMode skSpriteNode =
  sendMessage skSpriteNode blendModeSelector

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKSpriteNode skSpriteNode => skSpriteNode -> SKBlendMode -> IO ()
setBlendMode skSpriteNode value =
  sendMessage skSpriteNode setBlendModeSelector value

-- | @- shader@
shader :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id SKShader)
shader skSpriteNode =
  sendMessage skSpriteNode shaderSelector

-- | @- setShader:@
setShader :: (IsSKSpriteNode skSpriteNode, IsSKShader value) => skSpriteNode -> value -> IO ()
setShader skSpriteNode value =
  sendMessage skSpriteNode setShaderSelector (toSKShader value)

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKSpriteNode skSpriteNode => skSpriteNode -> IO (Id NSDictionary)
attributeValues skSpriteNode =
  sendMessage skSpriteNode attributeValuesSelector

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKSpriteNode skSpriteNode, IsNSDictionary value) => skSpriteNode -> value -> IO ()
setAttributeValues skSpriteNode value =
  sendMessage skSpriteNode setAttributeValuesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @spriteNodeWithTexture:@
spriteNodeWithTextureSelector :: Selector '[Id SKTexture] (Id SKSpriteNode)
spriteNodeWithTextureSelector = mkSelector "spriteNodeWithTexture:"

-- | @Selector@ for @spriteNodeWithTexture:normalMap:@
spriteNodeWithTexture_normalMapSelector :: Selector '[Id SKTexture, Id SKTexture] (Id SKSpriteNode)
spriteNodeWithTexture_normalMapSelector = mkSelector "spriteNodeWithTexture:normalMap:"

-- | @Selector@ for @spriteNodeWithImageNamed:@
spriteNodeWithImageNamedSelector :: Selector '[Id NSString] (Id SKSpriteNode)
spriteNodeWithImageNamedSelector = mkSelector "spriteNodeWithImageNamed:"

-- | @Selector@ for @spriteNodeWithImageNamed:normalMapped:@
spriteNodeWithImageNamed_normalMappedSelector :: Selector '[Id NSString, Bool] (Id SKSpriteNode)
spriteNodeWithImageNamed_normalMappedSelector = mkSelector "spriteNodeWithImageNamed:normalMapped:"

-- | @Selector@ for @initWithTexture:@
initWithTextureSelector :: Selector '[Id SKTexture] (Id SKSpriteNode)
initWithTextureSelector = mkSelector "initWithTexture:"

-- | @Selector@ for @initWithImageNamed:@
initWithImageNamedSelector :: Selector '[Id NSString] (Id SKSpriteNode)
initWithImageNamedSelector = mkSelector "initWithImageNamed:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKSpriteNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector '[Id NSString] (Id SKAttributeValue)
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector '[Id SKAttributeValue, Id NSString] ()
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @texture@
textureSelector :: Selector '[] (Id SKTexture)
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector '[Id SKTexture] ()
setTextureSelector = mkSelector "setTexture:"

-- | @Selector@ for @normalTexture@
normalTextureSelector :: Selector '[] (Id SKTexture)
normalTextureSelector = mkSelector "normalTexture"

-- | @Selector@ for @setNormalTexture:@
setNormalTextureSelector :: Selector '[Id SKTexture] ()
setNormalTextureSelector = mkSelector "setNormalTexture:"

-- | @Selector@ for @lightingBitMask@
lightingBitMaskSelector :: Selector '[] CUInt
lightingBitMaskSelector = mkSelector "lightingBitMask"

-- | @Selector@ for @setLightingBitMask:@
setLightingBitMaskSelector :: Selector '[CUInt] ()
setLightingBitMaskSelector = mkSelector "setLightingBitMask:"

-- | @Selector@ for @shadowCastBitMask@
shadowCastBitMaskSelector :: Selector '[] CUInt
shadowCastBitMaskSelector = mkSelector "shadowCastBitMask"

-- | @Selector@ for @setShadowCastBitMask:@
setShadowCastBitMaskSelector :: Selector '[CUInt] ()
setShadowCastBitMaskSelector = mkSelector "setShadowCastBitMask:"

-- | @Selector@ for @shadowedBitMask@
shadowedBitMaskSelector :: Selector '[] CUInt
shadowedBitMaskSelector = mkSelector "shadowedBitMask"

-- | @Selector@ for @setShadowedBitMask:@
setShadowedBitMaskSelector :: Selector '[CUInt] ()
setShadowedBitMaskSelector = mkSelector "setShadowedBitMask:"

-- | @Selector@ for @colorBlendFactor@
colorBlendFactorSelector :: Selector '[] CDouble
colorBlendFactorSelector = mkSelector "colorBlendFactor"

-- | @Selector@ for @setColorBlendFactor:@
setColorBlendFactorSelector :: Selector '[CDouble] ()
setColorBlendFactorSelector = mkSelector "setColorBlendFactor:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] SKBlendMode
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[SKBlendMode] ()
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @shader@
shaderSelector :: Selector '[] (Id SKShader)
shaderSelector = mkSelector "shader"

-- | @Selector@ for @setShader:@
setShaderSelector :: Selector '[Id SKShader] ()
setShaderSelector = mkSelector "setShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector '[] (Id NSDictionary)
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector '[Id NSDictionary] ()
setAttributeValuesSelector = mkSelector "setAttributeValues:"

