{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTextureSampler@.
module ObjC.ModelIO.MDLTextureSampler
  ( MDLTextureSampler
  , IsMDLTextureSampler(..)
  , texture
  , setTexture
  , hardwareFilter
  , setHardwareFilter
  , transform
  , setTransform
  , hardwareFilterSelector
  , setHardwareFilterSelector
  , setTextureSelector
  , setTransformSelector
  , textureSelector
  , transformSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- texture@
texture :: IsMDLTextureSampler mdlTextureSampler => mdlTextureSampler -> IO (Id MDLTexture)
texture mdlTextureSampler =
  sendMessage mdlTextureSampler textureSelector

-- | @- setTexture:@
setTexture :: (IsMDLTextureSampler mdlTextureSampler, IsMDLTexture value) => mdlTextureSampler -> value -> IO ()
setTexture mdlTextureSampler value =
  sendMessage mdlTextureSampler setTextureSelector (toMDLTexture value)

-- | @- hardwareFilter@
hardwareFilter :: IsMDLTextureSampler mdlTextureSampler => mdlTextureSampler -> IO (Id MDLTextureFilter)
hardwareFilter mdlTextureSampler =
  sendMessage mdlTextureSampler hardwareFilterSelector

-- | @- setHardwareFilter:@
setHardwareFilter :: (IsMDLTextureSampler mdlTextureSampler, IsMDLTextureFilter value) => mdlTextureSampler -> value -> IO ()
setHardwareFilter mdlTextureSampler value =
  sendMessage mdlTextureSampler setHardwareFilterSelector (toMDLTextureFilter value)

-- | @- transform@
transform :: IsMDLTextureSampler mdlTextureSampler => mdlTextureSampler -> IO (Id MDLTransform)
transform mdlTextureSampler =
  sendMessage mdlTextureSampler transformSelector

-- | @- setTransform:@
setTransform :: (IsMDLTextureSampler mdlTextureSampler, IsMDLTransform value) => mdlTextureSampler -> value -> IO ()
setTransform mdlTextureSampler value =
  sendMessage mdlTextureSampler setTransformSelector (toMDLTransform value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @texture@
textureSelector :: Selector '[] (Id MDLTexture)
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector '[Id MDLTexture] ()
setTextureSelector = mkSelector "setTexture:"

-- | @Selector@ for @hardwareFilter@
hardwareFilterSelector :: Selector '[] (Id MDLTextureFilter)
hardwareFilterSelector = mkSelector "hardwareFilter"

-- | @Selector@ for @setHardwareFilter:@
setHardwareFilterSelector :: Selector '[Id MDLTextureFilter] ()
setHardwareFilterSelector = mkSelector "setHardwareFilter:"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Id MDLTransform)
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector '[Id MDLTransform] ()
setTransformSelector = mkSelector "setTransform:"

