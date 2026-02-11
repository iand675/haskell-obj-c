{-# LANGUAGE PatternSynonyms #-}
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
  , prepareToDrawSelector
  , colorMaterialEnabledSelector
  , setColorMaterialEnabledSelector
  , lightModelTwoSidedSelector
  , setLightModelTwoSidedSelector
  , useConstantColorSelector
  , setUseConstantColorSelector
  , transformSelector
  , light0Selector
  , light1Selector
  , light2Selector
  , lightingTypeSelector
  , setLightingTypeSelector
  , materialSelector
  , texture2d0Selector
  , texture2d1Selector
  , textureOrderSelector
  , setTextureOrderSelector
  , fogSelector
  , labelSelector
  , setLabelSelector

  -- * Enum types
  , GLKLightingType(GLKLightingType)
  , pattern GLKLightingTypePerVertex
  , pattern GLKLightingTypePerPixel

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

import ObjC.GLKit.Internal.Classes
import ObjC.GLKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- prepareToDraw@
prepareToDraw :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO ()
prepareToDraw glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "prepareToDraw") retVoid []

-- | @- colorMaterialEnabled@
colorMaterialEnabled :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO CUChar
colorMaterialEnabled glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "colorMaterialEnabled") retCUChar []

-- | @- setColorMaterialEnabled:@
setColorMaterialEnabled :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> CUChar -> IO ()
setColorMaterialEnabled glkBaseEffect  value =
  sendMsg glkBaseEffect (mkSelector "setColorMaterialEnabled:") retVoid [argCUChar (fromIntegral value)]

-- | @- lightModelTwoSided@
lightModelTwoSided :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO CUChar
lightModelTwoSided glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "lightModelTwoSided") retCUChar []

-- | @- setLightModelTwoSided:@
setLightModelTwoSided :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> CUChar -> IO ()
setLightModelTwoSided glkBaseEffect  value =
  sendMsg glkBaseEffect (mkSelector "setLightModelTwoSided:") retVoid [argCUChar (fromIntegral value)]

-- | @- useConstantColor@
useConstantColor :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO CUChar
useConstantColor glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "useConstantColor") retCUChar []

-- | @- setUseConstantColor:@
setUseConstantColor :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> CUChar -> IO ()
setUseConstantColor glkBaseEffect  value =
  sendMsg glkBaseEffect (mkSelector "setUseConstantColor:") retVoid [argCUChar (fromIntegral value)]

-- | @- transform@
transform :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyTransform)
transform glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "transform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- light0@
light0 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyLight)
light0 glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "light0") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- light1@
light1 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyLight)
light1 glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "light1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- light2@
light2 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyLight)
light2 glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "light2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lightingType@
lightingType :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO GLKLightingType
lightingType glkBaseEffect  =
  fmap (coerce :: CInt -> GLKLightingType) $ sendMsg glkBaseEffect (mkSelector "lightingType") retCInt []

-- | @- setLightingType:@
setLightingType :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> GLKLightingType -> IO ()
setLightingType glkBaseEffect  value =
  sendMsg glkBaseEffect (mkSelector "setLightingType:") retVoid [argCInt (coerce value)]

-- | @- material@
material :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyMaterial)
material glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "material") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- texture2d0@
texture2d0 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyTexture)
texture2d0 glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "texture2d0") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- texture2d1@
texture2d1 :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyTexture)
texture2d1 glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "texture2d1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textureOrder@
textureOrder :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id NSArray)
textureOrder glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "textureOrder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextureOrder:@
setTextureOrder :: (IsGLKBaseEffect glkBaseEffect, IsNSArray value) => glkBaseEffect -> value -> IO ()
setTextureOrder glkBaseEffect  value =
withObjCPtr value $ \raw_value ->
    sendMsg glkBaseEffect (mkSelector "setTextureOrder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fog@
fog :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id GLKEffectPropertyFog)
fog glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "fog") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- label@
label :: IsGLKBaseEffect glkBaseEffect => glkBaseEffect -> IO (Id NSString)
label glkBaseEffect  =
  sendMsg glkBaseEffect (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsGLKBaseEffect glkBaseEffect, IsNSString value) => glkBaseEffect -> value -> IO ()
setLabel glkBaseEffect  value =
withObjCPtr value $ \raw_value ->
    sendMsg glkBaseEffect (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareToDraw@
prepareToDrawSelector :: Selector
prepareToDrawSelector = mkSelector "prepareToDraw"

-- | @Selector@ for @colorMaterialEnabled@
colorMaterialEnabledSelector :: Selector
colorMaterialEnabledSelector = mkSelector "colorMaterialEnabled"

-- | @Selector@ for @setColorMaterialEnabled:@
setColorMaterialEnabledSelector :: Selector
setColorMaterialEnabledSelector = mkSelector "setColorMaterialEnabled:"

-- | @Selector@ for @lightModelTwoSided@
lightModelTwoSidedSelector :: Selector
lightModelTwoSidedSelector = mkSelector "lightModelTwoSided"

-- | @Selector@ for @setLightModelTwoSided:@
setLightModelTwoSidedSelector :: Selector
setLightModelTwoSidedSelector = mkSelector "setLightModelTwoSided:"

-- | @Selector@ for @useConstantColor@
useConstantColorSelector :: Selector
useConstantColorSelector = mkSelector "useConstantColor"

-- | @Selector@ for @setUseConstantColor:@
setUseConstantColorSelector :: Selector
setUseConstantColorSelector = mkSelector "setUseConstantColor:"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @light0@
light0Selector :: Selector
light0Selector = mkSelector "light0"

-- | @Selector@ for @light1@
light1Selector :: Selector
light1Selector = mkSelector "light1"

-- | @Selector@ for @light2@
light2Selector :: Selector
light2Selector = mkSelector "light2"

-- | @Selector@ for @lightingType@
lightingTypeSelector :: Selector
lightingTypeSelector = mkSelector "lightingType"

-- | @Selector@ for @setLightingType:@
setLightingTypeSelector :: Selector
setLightingTypeSelector = mkSelector "setLightingType:"

-- | @Selector@ for @material@
materialSelector :: Selector
materialSelector = mkSelector "material"

-- | @Selector@ for @texture2d0@
texture2d0Selector :: Selector
texture2d0Selector = mkSelector "texture2d0"

-- | @Selector@ for @texture2d1@
texture2d1Selector :: Selector
texture2d1Selector = mkSelector "texture2d1"

-- | @Selector@ for @textureOrder@
textureOrderSelector :: Selector
textureOrderSelector = mkSelector "textureOrder"

-- | @Selector@ for @setTextureOrder:@
setTextureOrderSelector :: Selector
setTextureOrderSelector = mkSelector "setTextureOrder:"

-- | @Selector@ for @fog@
fogSelector :: Selector
fogSelector = mkSelector "fog"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

