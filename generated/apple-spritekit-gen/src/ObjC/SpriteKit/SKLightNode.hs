{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKLightNode@.
module ObjC.SpriteKit.SKLightNode
  ( SKLightNode
  , IsSKLightNode(..)
  , enabled
  , setEnabled
  , lightColor
  , setLightColor
  , ambientColor
  , setAmbientColor
  , shadowColor
  , setShadowColor
  , falloff
  , setFalloff
  , categoryBitMask
  , setCategoryBitMask
  , ambientColorSelector
  , categoryBitMaskSelector
  , enabledSelector
  , falloffSelector
  , lightColorSelector
  , setAmbientColorSelector
  , setCategoryBitMaskSelector
  , setEnabledSelector
  , setFalloffSelector
  , setLightColorSelector
  , setShadowColorSelector
  , shadowColorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Enables or disables lighting contribution from this light node.
--
-- Set to YES; sprites using this light will be lit with the ambient color and the light color, with a falloff in intensity according to the falloff property.
--
-- Set to NO; this light does not contribute any lighting.
--
-- If no lights are active on a sprite it will be drawn normally, as if not lit.
--
-- The default value is YES.
--
-- See: lightColor
--
-- See: falloff
--
-- See: categoryBitMask
--
-- ObjC selector: @- enabled@
enabled :: IsSKLightNode skLightNode => skLightNode -> IO Bool
enabled skLightNode =
  sendMessage skLightNode enabledSelector

-- | Enables or disables lighting contribution from this light node.
--
-- Set to YES; sprites using this light will be lit with the ambient color and the light color, with a falloff in intensity according to the falloff property.
--
-- Set to NO; this light does not contribute any lighting.
--
-- If no lights are active on a sprite it will be drawn normally, as if not lit.
--
-- The default value is YES.
--
-- See: lightColor
--
-- See: falloff
--
-- See: categoryBitMask
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSKLightNode skLightNode => skLightNode -> Bool -> IO ()
setEnabled skLightNode value =
  sendMessage skLightNode setEnabledSelector value

-- | Diffuse and Specular color of the light source, defaults to opaque white.
--
-- The alpha component of the color is ignored.
--
-- If using shaders bind a uniform to this property to use scene based custom lighting.
--
-- See: SKUniform
--
-- See: falloff
--
-- ObjC selector: @- lightColor@
lightColor :: IsSKLightNode skLightNode => skLightNode -> IO (Id NSColor)
lightColor skLightNode =
  sendMessage skLightNode lightColorSelector

-- | Diffuse and Specular color of the light source, defaults to opaque white.
--
-- The alpha component of the color is ignored.
--
-- If using shaders bind a uniform to this property to use scene based custom lighting.
--
-- See: SKUniform
--
-- See: falloff
--
-- ObjC selector: @- setLightColor:@
setLightColor :: (IsSKLightNode skLightNode, IsNSColor value) => skLightNode -> value -> IO ()
setLightColor skLightNode value =
  sendMessage skLightNode setLightColorSelector (toNSColor value)

-- | Ambient color of the light source, defaults to black.
--
-- If you had only a single light in the scene with an ambient color of opaque white and a light color of black, it would appear as if the scene was rendered without lighting.
--
-- The alpha component of the color is ignored. The color is not affected by falloff or surface normals.
--
-- See: lightColor
--
-- ObjC selector: @- ambientColor@
ambientColor :: IsSKLightNode skLightNode => skLightNode -> IO (Id NSColor)
ambientColor skLightNode =
  sendMessage skLightNode ambientColorSelector

-- | Ambient color of the light source, defaults to black.
--
-- If you had only a single light in the scene with an ambient color of opaque white and a light color of black, it would appear as if the scene was rendered without lighting.
--
-- The alpha component of the color is ignored. The color is not affected by falloff or surface normals.
--
-- See: lightColor
--
-- ObjC selector: @- setAmbientColor:@
setAmbientColor :: (IsSKLightNode skLightNode, IsNSColor value) => skLightNode -> value -> IO ()
setAmbientColor skLightNode value =
  sendMessage skLightNode setAmbientColorSelector (toNSColor value)

-- | Color of the shadow casted on occluded objects, defaults to half opacity black.
--
-- The alpha component of the color is used for blending with the regions that are in shadow.
--
-- See: SKSpriteNode.shadowCastBitMask
--
-- See: SKSpriteNode.shadowedBitMask
--
-- ObjC selector: @- shadowColor@
shadowColor :: IsSKLightNode skLightNode => skLightNode -> IO (Id NSColor)
shadowColor skLightNode =
  sendMessage skLightNode shadowColorSelector

-- | Color of the shadow casted on occluded objects, defaults to half opacity black.
--
-- The alpha component of the color is used for blending with the regions that are in shadow.
--
-- See: SKSpriteNode.shadowCastBitMask
--
-- See: SKSpriteNode.shadowedBitMask
--
-- ObjC selector: @- setShadowColor:@
setShadowColor :: (IsSKLightNode skLightNode, IsNSColor value) => skLightNode -> value -> IO ()
setShadowColor skLightNode value =
  sendMessage skLightNode setShadowColorSelector (toNSColor value)

-- | Falloff in intensity of the light over distance, defaults to 1. The falloff does not affect the ambient color nor the shadow color.
--
-- See: lightColor
--
-- ObjC selector: @- falloff@
falloff :: IsSKLightNode skLightNode => skLightNode -> IO CDouble
falloff skLightNode =
  sendMessage skLightNode falloffSelector

-- | Falloff in intensity of the light over distance, defaults to 1. The falloff does not affect the ambient color nor the shadow color.
--
-- See: lightColor
--
-- ObjC selector: @- setFalloff:@
setFalloff :: IsSKLightNode skLightNode => skLightNode -> CDouble -> IO ()
setFalloff skLightNode value =
  sendMessage skLightNode setFalloffSelector value

-- | The category of the light, which determines the group(s) a light belongs to. Any node that has its corresponding light and shadow bitmasks set to an overlapping value will be lit, shadow casting or shadowed by this light.
--
-- See: SKSpriteNode.lightingBitMask
--
-- See: SKSpriteNode.shadowCastBitMask
--
-- See: SKSpriteNode.shadowedBitMask
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSKLightNode skLightNode => skLightNode -> IO CUInt
categoryBitMask skLightNode =
  sendMessage skLightNode categoryBitMaskSelector

-- | The category of the light, which determines the group(s) a light belongs to. Any node that has its corresponding light and shadow bitmasks set to an overlapping value will be lit, shadow casting or shadowed by this light.
--
-- See: SKSpriteNode.lightingBitMask
--
-- See: SKSpriteNode.shadowCastBitMask
--
-- See: SKSpriteNode.shadowedBitMask
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSKLightNode skLightNode => skLightNode -> CUInt -> IO ()
setCategoryBitMask skLightNode value =
  sendMessage skLightNode setCategoryBitMaskSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @lightColor@
lightColorSelector :: Selector '[] (Id NSColor)
lightColorSelector = mkSelector "lightColor"

-- | @Selector@ for @setLightColor:@
setLightColorSelector :: Selector '[Id NSColor] ()
setLightColorSelector = mkSelector "setLightColor:"

-- | @Selector@ for @ambientColor@
ambientColorSelector :: Selector '[] (Id NSColor)
ambientColorSelector = mkSelector "ambientColor"

-- | @Selector@ for @setAmbientColor:@
setAmbientColorSelector :: Selector '[Id NSColor] ()
setAmbientColorSelector = mkSelector "setAmbientColor:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector '[] (Id NSColor)
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector '[Id NSColor] ()
setShadowColorSelector = mkSelector "setShadowColor:"

-- | @Selector@ for @falloff@
falloffSelector :: Selector '[] CDouble
falloffSelector = mkSelector "falloff"

-- | @Selector@ for @setFalloff:@
setFalloffSelector :: Selector '[CDouble] ()
setFalloffSelector = mkSelector "setFalloff:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CUInt
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CUInt] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

