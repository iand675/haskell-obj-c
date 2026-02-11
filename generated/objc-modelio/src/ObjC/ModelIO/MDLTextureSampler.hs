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
  , textureSelector
  , setTextureSelector
  , hardwareFilterSelector
  , setHardwareFilterSelector
  , transformSelector
  , setTransformSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- texture@
texture :: IsMDLTextureSampler mdlTextureSampler => mdlTextureSampler -> IO (Id MDLTexture)
texture mdlTextureSampler  =
  sendMsg mdlTextureSampler (mkSelector "texture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTexture:@
setTexture :: (IsMDLTextureSampler mdlTextureSampler, IsMDLTexture value) => mdlTextureSampler -> value -> IO ()
setTexture mdlTextureSampler  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlTextureSampler (mkSelector "setTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hardwareFilter@
hardwareFilter :: IsMDLTextureSampler mdlTextureSampler => mdlTextureSampler -> IO (Id MDLTextureFilter)
hardwareFilter mdlTextureSampler  =
  sendMsg mdlTextureSampler (mkSelector "hardwareFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHardwareFilter:@
setHardwareFilter :: (IsMDLTextureSampler mdlTextureSampler, IsMDLTextureFilter value) => mdlTextureSampler -> value -> IO ()
setHardwareFilter mdlTextureSampler  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlTextureSampler (mkSelector "setHardwareFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transform@
transform :: IsMDLTextureSampler mdlTextureSampler => mdlTextureSampler -> IO (Id MDLTransform)
transform mdlTextureSampler  =
  sendMsg mdlTextureSampler (mkSelector "transform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransform:@
setTransform :: (IsMDLTextureSampler mdlTextureSampler, IsMDLTransform value) => mdlTextureSampler -> value -> IO ()
setTransform mdlTextureSampler  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlTextureSampler (mkSelector "setTransform:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @texture@
textureSelector :: Selector
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector
setTextureSelector = mkSelector "setTexture:"

-- | @Selector@ for @hardwareFilter@
hardwareFilterSelector :: Selector
hardwareFilterSelector = mkSelector "hardwareFilter"

-- | @Selector@ for @setHardwareFilter:@
setHardwareFilterSelector :: Selector
setHardwareFilterSelector = mkSelector "setHardwareFilter:"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector
setTransformSelector = mkSelector "setTransform:"

