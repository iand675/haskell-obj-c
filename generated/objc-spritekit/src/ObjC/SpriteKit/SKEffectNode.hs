{-# LANGUAGE PatternSynonyms #-}
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
  , valueForAttributeNamedSelector
  , setValue_forAttributeNamedSelector
  , filterSelector
  , setFilterSelector
  , shouldCenterFilterSelector
  , setShouldCenterFilterSelector
  , shouldEnableEffectsSelector
  , setShouldEnableEffectsSelector
  , shouldRasterizeSelector
  , setShouldRasterizeSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKEffectNode skEffectNode, IsNSString key) => skEffectNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skEffectNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skEffectNode (mkSelector "valueForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKEffectNode skEffectNode, IsSKAttributeValue value, IsNSString key) => skEffectNode -> value -> key -> IO ()
setValue_forAttributeNamed skEffectNode  value key =
withObjCPtr value $ \raw_value ->
  withObjCPtr key $ \raw_key ->
      sendMsg skEffectNode (mkSelector "setValue:forAttributeNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | A CIFilter to be used as an effect
--
-- Any CIFilter that requires only a single "inputImage" and produces an "outputImage" is allowed. The filter is applied to all children of the SKEffectNode. If the filter is nil, the children of this node is flattened before being drawn as long as the SKEffectNode is enabled.
--
-- ObjC selector: @- filter@
filter_ :: IsSKEffectNode skEffectNode => skEffectNode -> IO (Id CIFilter)
filter_ skEffectNode  =
  sendMsg skEffectNode (mkSelector "filter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A CIFilter to be used as an effect
--
-- Any CIFilter that requires only a single "inputImage" and produces an "outputImage" is allowed. The filter is applied to all children of the SKEffectNode. If the filter is nil, the children of this node is flattened before being drawn as long as the SKEffectNode is enabled.
--
-- ObjC selector: @- setFilter:@
setFilter :: (IsSKEffectNode skEffectNode, IsCIFilter value) => skEffectNode -> value -> IO ()
setFilter skEffectNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEffectNode (mkSelector "setFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldCenterFilter@
shouldCenterFilter :: IsSKEffectNode skEffectNode => skEffectNode -> IO Bool
shouldCenterFilter skEffectNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skEffectNode (mkSelector "shouldCenterFilter") retCULong []

-- | @- setShouldCenterFilter:@
setShouldCenterFilter :: IsSKEffectNode skEffectNode => skEffectNode -> Bool -> IO ()
setShouldCenterFilter skEffectNode  value =
  sendMsg skEffectNode (mkSelector "setShouldCenterFilter:") retVoid [argCULong (if value then 1 else 0)]

-- | Enable the SKEffectNode.
--
-- The SKEffectNode has no effect when appliesEffects is not enabled, this is useful for setting up an effect to use later on. Defaults to YES.
--
-- ObjC selector: @- shouldEnableEffects@
shouldEnableEffects :: IsSKEffectNode skEffectNode => skEffectNode -> IO Bool
shouldEnableEffects skEffectNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skEffectNode (mkSelector "shouldEnableEffects") retCULong []

-- | Enable the SKEffectNode.
--
-- The SKEffectNode has no effect when appliesEffects is not enabled, this is useful for setting up an effect to use later on. Defaults to YES.
--
-- ObjC selector: @- setShouldEnableEffects:@
setShouldEnableEffects :: IsSKEffectNode skEffectNode => skEffectNode -> Bool -> IO ()
setShouldEnableEffects skEffectNode  value =
  sendMsg skEffectNode (mkSelector "setShouldEnableEffects:") retVoid [argCULong (if value then 1 else 0)]

-- | Enable the rasterization on the SKEffectNode.
--
-- The SKEffectNode's output is rasterized and cached internally. This cache is reused when rendering. When the SKEffectNode's children change, the cache is updated, but changing properties on the CIFilter does *not* cause an update (you must disable rasterization and then re-enable it for the changes to apply). This is more expensive than not rasterizing if the node's children change frequently, only enable this option if you know the children is largely static.
--
-- ObjC selector: @- shouldRasterize@
shouldRasterize :: IsSKEffectNode skEffectNode => skEffectNode -> IO Bool
shouldRasterize skEffectNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skEffectNode (mkSelector "shouldRasterize") retCULong []

-- | Enable the rasterization on the SKEffectNode.
--
-- The SKEffectNode's output is rasterized and cached internally. This cache is reused when rendering. When the SKEffectNode's children change, the cache is updated, but changing properties on the CIFilter does *not* cause an update (you must disable rasterization and then re-enable it for the changes to apply). This is more expensive than not rasterizing if the node's children change frequently, only enable this option if you know the children is largely static.
--
-- ObjC selector: @- setShouldRasterize:@
setShouldRasterize :: IsSKEffectNode skEffectNode => skEffectNode -> Bool -> IO ()
setShouldRasterize skEffectNode  value =
  sendMsg skEffectNode (mkSelector "setShouldRasterize:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets the blend mode to use when composing the effect with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKEffectNode skEffectNode => skEffectNode -> IO SKBlendMode
blendMode skEffectNode  =
  fmap (coerce :: CLong -> SKBlendMode) $ sendMsg skEffectNode (mkSelector "blendMode") retCLong []

-- | Sets the blend mode to use when composing the effect with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKEffectNode skEffectNode => skEffectNode -> SKBlendMode -> IO ()
setBlendMode skEffectNode  value =
  sendMsg skEffectNode (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- | @- shader@
shader :: IsSKEffectNode skEffectNode => skEffectNode -> IO (Id SKShader)
shader skEffectNode  =
  sendMsg skEffectNode (mkSelector "shader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShader:@
setShader :: (IsSKEffectNode skEffectNode, IsSKShader value) => skEffectNode -> value -> IO ()
setShader skEffectNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEffectNode (mkSelector "setShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKEffectNode skEffectNode => skEffectNode -> IO (Id NSDictionary)
attributeValues skEffectNode  =
  sendMsg skEffectNode (mkSelector "attributeValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKEffectNode skEffectNode, IsNSDictionary value) => skEffectNode -> value -> IO ()
setAttributeValues skEffectNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEffectNode (mkSelector "setAttributeValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @filter@
filterSelector :: Selector
filterSelector = mkSelector "filter"

-- | @Selector@ for @setFilter:@
setFilterSelector :: Selector
setFilterSelector = mkSelector "setFilter:"

-- | @Selector@ for @shouldCenterFilter@
shouldCenterFilterSelector :: Selector
shouldCenterFilterSelector = mkSelector "shouldCenterFilter"

-- | @Selector@ for @setShouldCenterFilter:@
setShouldCenterFilterSelector :: Selector
setShouldCenterFilterSelector = mkSelector "setShouldCenterFilter:"

-- | @Selector@ for @shouldEnableEffects@
shouldEnableEffectsSelector :: Selector
shouldEnableEffectsSelector = mkSelector "shouldEnableEffects"

-- | @Selector@ for @setShouldEnableEffects:@
setShouldEnableEffectsSelector :: Selector
setShouldEnableEffectsSelector = mkSelector "setShouldEnableEffects:"

-- | @Selector@ for @shouldRasterize@
shouldRasterizeSelector :: Selector
shouldRasterizeSelector = mkSelector "shouldRasterize"

-- | @Selector@ for @setShouldRasterize:@
setShouldRasterizeSelector :: Selector
setShouldRasterizeSelector = mkSelector "setShouldRasterize:"

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

