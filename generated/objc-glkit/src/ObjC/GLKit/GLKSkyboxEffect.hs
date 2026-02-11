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
  , prepareToDrawSelector
  , drawSelector
  , xSizeSelector
  , setXSizeSelector
  , ySizeSelector
  , setYSizeSelector
  , zSizeSelector
  , setZSizeSelector
  , textureCubeMapSelector
  , transformSelector
  , labelSelector
  , setLabelSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- prepareToDraw@
prepareToDraw :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO ()
prepareToDraw glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "prepareToDraw") retVoid []

-- | @- draw@
draw :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO ()
draw glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "draw") retVoid []

-- | @- xSize@
xSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO CFloat
xSize glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "xSize") retCFloat []

-- | @- setXSize:@
setXSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> CFloat -> IO ()
setXSize glkSkyboxEffect  value =
  sendMsg glkSkyboxEffect (mkSelector "setXSize:") retVoid [argCFloat (fromIntegral value)]

-- | @- ySize@
ySize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO CFloat
ySize glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "ySize") retCFloat []

-- | @- setYSize:@
setYSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> CFloat -> IO ()
setYSize glkSkyboxEffect  value =
  sendMsg glkSkyboxEffect (mkSelector "setYSize:") retVoid [argCFloat (fromIntegral value)]

-- | @- zSize@
zSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO CFloat
zSize glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "zSize") retCFloat []

-- | @- setZSize:@
setZSize :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> CFloat -> IO ()
setZSize glkSkyboxEffect  value =
  sendMsg glkSkyboxEffect (mkSelector "setZSize:") retVoid [argCFloat (fromIntegral value)]

-- | @- textureCubeMap@
textureCubeMap :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO (Id GLKEffectPropertyTexture)
textureCubeMap glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "textureCubeMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transform@
transform :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO (Id GLKEffectPropertyTransform)
transform glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "transform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- label@
label :: IsGLKSkyboxEffect glkSkyboxEffect => glkSkyboxEffect -> IO (Id NSString)
label glkSkyboxEffect  =
  sendMsg glkSkyboxEffect (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsGLKSkyboxEffect glkSkyboxEffect, IsNSString value) => glkSkyboxEffect -> value -> IO ()
setLabel glkSkyboxEffect  value =
withObjCPtr value $ \raw_value ->
    sendMsg glkSkyboxEffect (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareToDraw@
prepareToDrawSelector :: Selector
prepareToDrawSelector = mkSelector "prepareToDraw"

-- | @Selector@ for @draw@
drawSelector :: Selector
drawSelector = mkSelector "draw"

-- | @Selector@ for @xSize@
xSizeSelector :: Selector
xSizeSelector = mkSelector "xSize"

-- | @Selector@ for @setXSize:@
setXSizeSelector :: Selector
setXSizeSelector = mkSelector "setXSize:"

-- | @Selector@ for @ySize@
ySizeSelector :: Selector
ySizeSelector = mkSelector "ySize"

-- | @Selector@ for @setYSize:@
setYSizeSelector :: Selector
setYSizeSelector = mkSelector "setYSize:"

-- | @Selector@ for @zSize@
zSizeSelector :: Selector
zSizeSelector = mkSelector "zSize"

-- | @Selector@ for @setZSize:@
setZSizeSelector :: Selector
setZSizeSelector = mkSelector "setZSize:"

-- | @Selector@ for @textureCubeMap@
textureCubeMapSelector :: Selector
textureCubeMapSelector = mkSelector "textureCubeMap"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

