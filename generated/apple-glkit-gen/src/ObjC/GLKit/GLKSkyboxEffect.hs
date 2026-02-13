{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKSkyboxEffect@.
module ObjC.GLKit.GLKSkyboxEffect
  ( GLKSkyboxEffect
  , IsGLKSkyboxEffect(..)
  , prepareToDraw
  , draw
  , xSize
  , setXSize
  , ySize
  , setYSize
  , zSize
  , setZSize
  , textureCubeMap
  , transform
  , label
  , setLabel
  , drawSelector
  , labelSelector
  , prepareToDrawSelector
  , setLabelSelector
  , setXSizeSelector
  , setYSizeSelector
  , setZSizeSelector
  , textureCubeMapSelector
  , transformSelector
  , xSizeSelector
  , ySizeSelector
  , zSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- prepareToDraw@
prepareToDraw :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO ()
prepareToDraw glkSkyboxEffect =
  sendMessage glkSkyboxEffect prepareToDrawSelector

-- | @- draw@
draw :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO ()
draw glkSkyboxEffect =
  sendMessage glkSkyboxEffect drawSelector

-- | @- xSize@
xSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO CFloat
xSize glkSkyboxEffect =
  sendMessage glkSkyboxEffect xSizeSelector

-- | @- setXSize:@
setXSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> CFloat -> IO ()
setXSize glkSkyboxEffect value =
  sendMessage glkSkyboxEffect setXSizeSelector value

-- | @- ySize@
ySize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO CFloat
ySize glkSkyboxEffect =
  sendMessage glkSkyboxEffect ySizeSelector

-- | @- setYSize:@
setYSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> CFloat -> IO ()
setYSize glkSkyboxEffect value =
  sendMessage glkSkyboxEffect setYSizeSelector value

-- | @- zSize@
zSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO CFloat
zSize glkSkyboxEffect =
  sendMessage glkSkyboxEffect zSizeSelector

-- | @- setZSize:@
setZSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> CFloat -> IO ()
setZSize glkSkyboxEffect value =
  sendMessage glkSkyboxEffect setZSizeSelector value

-- | @- textureCubeMap@
textureCubeMap :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO (Id GLKEffectPropertyTexture)
textureCubeMap glkSkyboxEffect =
  sendMessage glkSkyboxEffect textureCubeMapSelector

-- | @- transform@
transform :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO (Id GLKEffectPropertyTransform)
transform glkSkyboxEffect =
  sendMessage glkSkyboxEffect transformSelector

-- | @- label@
label :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO (Id NSString)
label glkSkyboxEffect =
  sendMessage glkSkyboxEffect labelSelector

-- | @- setLabel:@
setLabel :: (IsGLKSkyboxEffect glkSkyboxEffect, IsNSString value) => glkSkyboxEffect -> value -> IO ()
setLabel glkSkyboxEffect value =
  sendMessage glkSkyboxEffect setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareToDraw@
prepareToDrawSelector :: Selector '[] ()
prepareToDrawSelector = mkSelector "prepareToDraw"

-- | @Selector@ for @draw@
drawSelector :: Selector '[] ()
drawSelector = mkSelector "draw"

-- | @Selector@ for @xSize@
xSizeSelector :: Selector '[] CFloat
xSizeSelector = mkSelector "xSize"

-- | @Selector@ for @setXSize:@
setXSizeSelector :: Selector '[CFloat] ()
setXSizeSelector = mkSelector "setXSize:"

-- | @Selector@ for @ySize@
ySizeSelector :: Selector '[] CFloat
ySizeSelector = mkSelector "ySize"

-- | @Selector@ for @setYSize:@
setYSizeSelector :: Selector '[CFloat] ()
setYSizeSelector = mkSelector "setYSize:"

-- | @Selector@ for @zSize@
zSizeSelector :: Selector '[] CFloat
zSizeSelector = mkSelector "zSize"

-- | @Selector@ for @setZSize:@
setZSizeSelector :: Selector '[CFloat] ()
setZSizeSelector = mkSelector "setZSize:"

-- | @Selector@ for @textureCubeMap@
textureCubeMapSelector :: Selector '[] (Id GLKEffectPropertyTexture)
textureCubeMapSelector = mkSelector "textureCubeMap"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Id GLKEffectPropertyTransform)
transformSelector = mkSelector "transform"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

