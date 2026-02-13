{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKBaseEffect@.
module ObjC.GLKit.GLKBaseEffect
  ( GLKBaseEffect
  , IsGLKBaseEffect(..)
  , prepareToDraw
  , colorMaterialEnabled
  , setColorMaterialEnabled
  , lightModelTwoSided
  , setLightModelTwoSided
  , useConstantColor
  , setUseConstantColor
  , transform
  , light0
  , light1
  , light2
  , lightingType
  , setLightingType
  , material
  , texture2d0
  , texture2d1
  , textureOrder
  , setTextureOrder
  , fog
  , label
  , setLabel
  , colorMaterialEnabledSelector
  , fogSelector
  , labelSelector
  , light0Selector
  , light1Selector
  , light2Selector
  , lightModelTwoSidedSelector
  , lightingTypeSelector
  , materialSelector
  , prepareToDrawSelector
  , setColorMaterialEnabledSelector
  , setLabelSelector
  , setLightModelTwoSidedSelector
  , setLightingTypeSelector
  , setTextureOrderSelector
  , setUseConstantColorSelector
  , texture2d0Selector
  , texture2d1Selector
  , textureOrderSelector
  , transformSelector
  , useConstantColorSelector

  -- * Enum types
  , GLKLightingType(GLKLightingType)
  , pattern GLKLightingTypePerVertex
  , pattern GLKLightingTypePerPixel

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.GLKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- prepareToDraw@
prepareToDraw :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO ()
prepareToDraw glkBaseEffect =
  sendMessage glkBaseEffect prepareToDrawSelector

-- | @- colorMaterialEnabled@
colorMaterialEnabled :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO CUChar
colorMaterialEnabled glkBaseEffect =
  sendMessage glkBaseEffect colorMaterialEnabledSelector

-- | @- setColorMaterialEnabled:@
setColorMaterialEnabled :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> CUChar -> IO ()
setColorMaterialEnabled glkBaseEffect value =
  sendMessage glkBaseEffect setColorMaterialEnabledSelector value

-- | @- lightModelTwoSided@
lightModelTwoSided :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO CUChar
lightModelTwoSided glkBaseEffect =
  sendMessage glkBaseEffect lightModelTwoSidedSelector

-- | @- setLightModelTwoSided:@
setLightModelTwoSided :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> CUChar -> IO ()
setLightModelTwoSided glkBaseEffect value =
  sendMessage glkBaseEffect setLightModelTwoSidedSelector value

-- | @- useConstantColor@
useConstantColor :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO CUChar
useConstantColor glkBaseEffect =
  sendMessage glkBaseEffect useConstantColorSelector

-- | @- setUseConstantColor:@
setUseConstantColor :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> CUChar -> IO ()
setUseConstantColor glkBaseEffect value =
  sendMessage glkBaseEffect setUseConstantColorSelector value

-- | @- transform@
transform :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyTransform)
transform glkBaseEffect =
  sendMessage glkBaseEffect transformSelector

-- | @- light0@
light0 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyLight)
light0 glkBaseEffect =
  sendMessage glkBaseEffect light0Selector

-- | @- light1@
light1 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyLight)
light1 glkBaseEffect =
  sendMessage glkBaseEffect light1Selector

-- | @- light2@
light2 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyLight)
light2 glkBaseEffect =
  sendMessage glkBaseEffect light2Selector

-- | @- lightingType@
lightingType :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO GLKLightingType
lightingType glkBaseEffect =
  sendMessage glkBaseEffect lightingTypeSelector

-- | @- setLightingType:@
setLightingType :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> GLKLightingType -> IO ()
setLightingType glkBaseEffect value =
  sendMessage glkBaseEffect setLightingTypeSelector value

-- | @- material@
material :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyMaterial)
material glkBaseEffect =
  sendMessage glkBaseEffect materialSelector

-- | @- texture2d0@
texture2d0 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyTexture)
texture2d0 glkBaseEffect =
  sendMessage glkBaseEffect texture2d0Selector

-- | @- texture2d1@
texture2d1 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyTexture)
texture2d1 glkBaseEffect =
  sendMessage glkBaseEffect texture2d1Selector

-- | @- textureOrder@
textureOrder :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id NSArray)
textureOrder glkBaseEffect =
  sendMessage glkBaseEffect textureOrderSelector

-- | @- setTextureOrder:@
setTextureOrder :: (IsGLKBaseEffect glkBaseEffect, IsNSArray value) => glkBaseEffect -> value -> IO ()
setTextureOrder glkBaseEffect value =
  sendMessage glkBaseEffect setTextureOrderSelector (toNSArray value)

-- | @- fog@
fog :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyFog)
fog glkBaseEffect =
  sendMessage glkBaseEffect fogSelector

-- | @- label@
label :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id NSString)
label glkBaseEffect =
  sendMessage glkBaseEffect labelSelector

-- | @- setLabel:@
setLabel :: (IsGLKBaseEffect glkBaseEffect, IsNSString value) => glkBaseEffect -> value -> IO ()
setLabel glkBaseEffect value =
  sendMessage glkBaseEffect setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareToDraw@
prepareToDrawSelector :: Selector '[] ()
prepareToDrawSelector = mkSelector "prepareToDraw"

-- | @Selector@ for @colorMaterialEnabled@
colorMaterialEnabledSelector :: Selector '[] CUChar
colorMaterialEnabledSelector = mkSelector "colorMaterialEnabled"

-- | @Selector@ for @setColorMaterialEnabled:@
setColorMaterialEnabledSelector :: Selector '[CUChar] ()
setColorMaterialEnabledSelector = mkSelector "setColorMaterialEnabled:"

-- | @Selector@ for @lightModelTwoSided@
lightModelTwoSidedSelector :: Selector '[] CUChar
lightModelTwoSidedSelector = mkSelector "lightModelTwoSided"

-- | @Selector@ for @setLightModelTwoSided:@
setLightModelTwoSidedSelector :: Selector '[CUChar] ()
setLightModelTwoSidedSelector = mkSelector "setLightModelTwoSided:"

-- | @Selector@ for @useConstantColor@
useConstantColorSelector :: Selector '[] CUChar
useConstantColorSelector = mkSelector "useConstantColor"

-- | @Selector@ for @setUseConstantColor:@
setUseConstantColorSelector :: Selector '[CUChar] ()
setUseConstantColorSelector = mkSelector "setUseConstantColor:"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Id GLKEffectPropertyTransform)
transformSelector = mkSelector "transform"

-- | @Selector@ for @light0@
light0Selector :: Selector '[] (Id GLKEffectPropertyLight)
light0Selector = mkSelector "light0"

-- | @Selector@ for @light1@
light1Selector :: Selector '[] (Id GLKEffectPropertyLight)
light1Selector = mkSelector "light1"

-- | @Selector@ for @light2@
light2Selector :: Selector '[] (Id GLKEffectPropertyLight)
light2Selector = mkSelector "light2"

-- | @Selector@ for @lightingType@
lightingTypeSelector :: Selector '[] GLKLightingType
lightingTypeSelector = mkSelector "lightingType"

-- | @Selector@ for @setLightingType:@
setLightingTypeSelector :: Selector '[GLKLightingType] ()
setLightingTypeSelector = mkSelector "setLightingType:"

-- | @Selector@ for @material@
materialSelector :: Selector '[] (Id GLKEffectPropertyMaterial)
materialSelector = mkSelector "material"

-- | @Selector@ for @texture2d0@
texture2d0Selector :: Selector '[] (Id GLKEffectPropertyTexture)
texture2d0Selector = mkSelector "texture2d0"

-- | @Selector@ for @texture2d1@
texture2d1Selector :: Selector '[] (Id GLKEffectPropertyTexture)
texture2d1Selector = mkSelector "texture2d1"

-- | @Selector@ for @textureOrder@
textureOrderSelector :: Selector '[] (Id NSArray)
textureOrderSelector = mkSelector "textureOrder"

-- | @Selector@ for @setTextureOrder:@
setTextureOrderSelector :: Selector '[Id NSArray] ()
setTextureOrderSelector = mkSelector "setTextureOrder:"

-- | @Selector@ for @fog@
fogSelector :: Selector '[] (Id GLKEffectPropertyFog)
fogSelector = mkSelector "fog"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

