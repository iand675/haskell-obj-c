{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A SpriteKit node that applies frame buffer effects to the rendered results of its child nodes. This is done continuously on live content and is not a simple snapshot of the rendered result at one instant of time.
--
-- Generated bindings for @SKEffectNode@.
module ObjC.SpriteKit.SKEffectNode
  ( SKEffectNode
  , IsSKEffectNode(..)
  , valueForAttributeNamed
  , setValue_forAttributeNamed
  , filter_
  , setFilter
  , shouldCenterFilter
  , setShouldCenterFilter
  , shouldEnableEffects
  , setShouldEnableEffects
  , shouldRasterize
  , setShouldRasterize
  , blendMode
  , setBlendMode
  , shader
  , setShader
  , attributeValues
  , setAttributeValues
  , attributeValuesSelector
  , blendModeSelector
  , filterSelector
  , setAttributeValuesSelector
  , setBlendModeSelector
  , setFilterSelector
  , setShaderSelector
  , setShouldCenterFilterSelector
  , setShouldEnableEffectsSelector
  , setShouldRasterizeSelector
  , setValue_forAttributeNamedSelector
  , shaderSelector
  , shouldCenterFilterSelector
  , shouldEnableEffectsSelector
  , shouldRasterizeSelector
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
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKEffectNode skEffectNode, IsNSString key) => skEffectNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skEffectNode key =
  sendMessage skEffectNode valueForAttributeNamedSelector (toNSString key)

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKEffectNode skEffectNode, IsSKAttributeValue value, IsNSString key) => skEffectNode -> value -> key -> IO ()
setValue_forAttributeNamed skEffectNode value key =
  sendMessage skEffectNode setValue_forAttributeNamedSelector (toSKAttributeValue value) (toNSString key)

-- | A CIFilter to be used as an effect
--
-- Any CIFilter that requires only a single "inputImage" and produces an "outputImage" is allowed. The filter is applied to all children of the SKEffectNode. If the filter is nil, the children of this node is flattened before being drawn as long as the SKEffectNode is enabled.
--
-- ObjC selector: @- filter@
filter_ :: IsSKEffectNode skEffectNode => skEffectNode -> IO (Id CIFilter)
filter_ skEffectNode =
  sendMessage skEffectNode filterSelector

-- | A CIFilter to be used as an effect
--
-- Any CIFilter that requires only a single "inputImage" and produces an "outputImage" is allowed. The filter is applied to all children of the SKEffectNode. If the filter is nil, the children of this node is flattened before being drawn as long as the SKEffectNode is enabled.
--
-- ObjC selector: @- setFilter:@
setFilter :: (IsSKEffectNode skEffectNode, IsCIFilter value) => skEffectNode -> value -> IO ()
setFilter skEffectNode value =
  sendMessage skEffectNode setFilterSelector (toCIFilter value)

-- | @- shouldCenterFilter@
shouldCenterFilter :: IsSKEffectNode skEffectNode => skEffectNode -> IO Bool
shouldCenterFilter skEffectNode =
  sendMessage skEffectNode shouldCenterFilterSelector

-- | @- setShouldCenterFilter:@
setShouldCenterFilter :: IsSKEffectNode skEffectNode => skEffectNode -> Bool -> IO ()
setShouldCenterFilter skEffectNode value =
  sendMessage skEffectNode setShouldCenterFilterSelector value

-- | Enable the SKEffectNode.
--
-- The SKEffectNode has no effect when appliesEffects is not enabled, this is useful for setting up an effect to use later on. Defaults to YES.
--
-- ObjC selector: @- shouldEnableEffects@
shouldEnableEffects :: IsSKEffectNode skEffectNode => skEffectNode -> IO Bool
shouldEnableEffects skEffectNode =
  sendMessage skEffectNode shouldEnableEffectsSelector

-- | Enable the SKEffectNode.
--
-- The SKEffectNode has no effect when appliesEffects is not enabled, this is useful for setting up an effect to use later on. Defaults to YES.
--
-- ObjC selector: @- setShouldEnableEffects:@
setShouldEnableEffects :: IsSKEffectNode skEffectNode => skEffectNode -> Bool -> IO ()
setShouldEnableEffects skEffectNode value =
  sendMessage skEffectNode setShouldEnableEffectsSelector value

-- | Enable the rasterization on the SKEffectNode.
--
-- The SKEffectNode's output is rasterized and cached internally. This cache is reused when rendering. When the SKEffectNode's children change, the cache is updated, but changing properties on the CIFilter does *not* cause an update (you must disable rasterization and then re-enable it for the changes to apply). This is more expensive than not rasterizing if the node's children change frequently, only enable this option if you know the children is largely static.
--
-- ObjC selector: @- shouldRasterize@
shouldRasterize :: IsSKEffectNode skEffectNode => skEffectNode -> IO Bool
shouldRasterize skEffectNode =
  sendMessage skEffectNode shouldRasterizeSelector

-- | Enable the rasterization on the SKEffectNode.
--
-- The SKEffectNode's output is rasterized and cached internally. This cache is reused when rendering. When the SKEffectNode's children change, the cache is updated, but changing properties on the CIFilter does *not* cause an update (you must disable rasterization and then re-enable it for the changes to apply). This is more expensive than not rasterizing if the node's children change frequently, only enable this option if you know the children is largely static.
--
-- ObjC selector: @- setShouldRasterize:@
setShouldRasterize :: IsSKEffectNode skEffectNode => skEffectNode -> Bool -> IO ()
setShouldRasterize skEffectNode value =
  sendMessage skEffectNode setShouldRasterizeSelector value

-- | Sets the blend mode to use when composing the effect with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKEffectNode skEffectNode => skEffectNode -> IO SKBlendMode
blendMode skEffectNode =
  sendMessage skEffectNode blendModeSelector

-- | Sets the blend mode to use when composing the effect with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKEffectNode skEffectNode => skEffectNode -> SKBlendMode -> IO ()
setBlendMode skEffectNode value =
  sendMessage skEffectNode setBlendModeSelector value

-- | @- shader@
shader :: IsSKEffectNode skEffectNode => skEffectNode -> IO (Id SKShader)
shader skEffectNode =
  sendMessage skEffectNode shaderSelector

-- | @- setShader:@
setShader :: (IsSKEffectNode skEffectNode, IsSKShader value) => skEffectNode -> value -> IO ()
setShader skEffectNode value =
  sendMessage skEffectNode setShaderSelector (toSKShader value)

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKEffectNode skEffectNode => skEffectNode -> IO (Id NSDictionary)
attributeValues skEffectNode =
  sendMessage skEffectNode attributeValuesSelector

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKEffectNode skEffectNode, IsNSDictionary value) => skEffectNode -> value -> IO ()
setAttributeValues skEffectNode value =
  sendMessage skEffectNode setAttributeValuesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector '[Id NSString] (Id SKAttributeValue)
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector '[Id SKAttributeValue, Id NSString] ()
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @filter@
filterSelector :: Selector '[] (Id CIFilter)
filterSelector = mkSelector "filter"

-- | @Selector@ for @setFilter:@
setFilterSelector :: Selector '[Id CIFilter] ()
setFilterSelector = mkSelector "setFilter:"

-- | @Selector@ for @shouldCenterFilter@
shouldCenterFilterSelector :: Selector '[] Bool
shouldCenterFilterSelector = mkSelector "shouldCenterFilter"

-- | @Selector@ for @setShouldCenterFilter:@
setShouldCenterFilterSelector :: Selector '[Bool] ()
setShouldCenterFilterSelector = mkSelector "setShouldCenterFilter:"

-- | @Selector@ for @shouldEnableEffects@
shouldEnableEffectsSelector :: Selector '[] Bool
shouldEnableEffectsSelector = mkSelector "shouldEnableEffects"

-- | @Selector@ for @setShouldEnableEffects:@
setShouldEnableEffectsSelector :: Selector '[Bool] ()
setShouldEnableEffectsSelector = mkSelector "setShouldEnableEffects:"

-- | @Selector@ for @shouldRasterize@
shouldRasterizeSelector :: Selector '[] Bool
shouldRasterizeSelector = mkSelector "shouldRasterize"

-- | @Selector@ for @setShouldRasterize:@
setShouldRasterizeSelector :: Selector '[Bool] ()
setShouldRasterizeSelector = mkSelector "setShouldRasterize:"

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

