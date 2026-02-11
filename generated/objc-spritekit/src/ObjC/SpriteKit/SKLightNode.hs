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
  , enabledSelector
  , setEnabledSelector
  , lightColorSelector
  , setLightColorSelector
  , ambientColorSelector
  , setAmbientColorSelector
  , shadowColorSelector
  , setShadowColorSelector
  , falloffSelector
  , setFalloffSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector


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
enabled skLightNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skLightNode (mkSelector "enabled") retCULong []

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
setEnabled skLightNode  value =
  sendMsg skLightNode (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

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
lightColor skLightNode  =
  sendMsg skLightNode (mkSelector "lightColor") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setLightColor skLightNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLightNode (mkSelector "setLightColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
ambientColor skLightNode  =
  sendMsg skLightNode (mkSelector "ambientColor") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setAmbientColor skLightNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLightNode (mkSelector "setAmbientColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
shadowColor skLightNode  =
  sendMsg skLightNode (mkSelector "shadowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setShadowColor skLightNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLightNode (mkSelector "setShadowColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Falloff in intensity of the light over distance, defaults to 1. The falloff does not affect the ambient color nor the shadow color.
--
-- See: lightColor
--
-- ObjC selector: @- falloff@
falloff :: IsSKLightNode skLightNode => skLightNode -> IO CDouble
falloff skLightNode  =
  sendMsg skLightNode (mkSelector "falloff") retCDouble []

-- | Falloff in intensity of the light over distance, defaults to 1. The falloff does not affect the ambient color nor the shadow color.
--
-- See: lightColor
--
-- ObjC selector: @- setFalloff:@
setFalloff :: IsSKLightNode skLightNode => skLightNode -> CDouble -> IO ()
setFalloff skLightNode  value =
  sendMsg skLightNode (mkSelector "setFalloff:") retVoid [argCDouble (fromIntegral value)]

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
categoryBitMask skLightNode  =
  sendMsg skLightNode (mkSelector "categoryBitMask") retCUInt []

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
setCategoryBitMask skLightNode  value =
  sendMsg skLightNode (mkSelector "setCategoryBitMask:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @lightColor@
lightColorSelector :: Selector
lightColorSelector = mkSelector "lightColor"

-- | @Selector@ for @setLightColor:@
setLightColorSelector :: Selector
setLightColorSelector = mkSelector "setLightColor:"

-- | @Selector@ for @ambientColor@
ambientColorSelector :: Selector
ambientColorSelector = mkSelector "ambientColor"

-- | @Selector@ for @setAmbientColor:@
setAmbientColorSelector :: Selector
setAmbientColorSelector = mkSelector "setAmbientColor:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector
setShadowColorSelector = mkSelector "setShadowColor:"

-- | @Selector@ for @falloff@
falloffSelector :: Selector
falloffSelector = mkSelector "falloff"

-- | @Selector@ for @setFalloff:@
setFalloffSelector :: Selector
setFalloffSelector = mkSelector "setFalloff:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

