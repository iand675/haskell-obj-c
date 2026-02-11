{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A texture to be mapped onto SKSpriteNode instances.
--
-- Generated bindings for @SKTexture@.
module ObjC.SpriteKit.SKTexture
  ( SKTexture
  , IsSKTexture(..)
  , textureWithImageNamed
  , textureWithCGImage
  , textureWithImage
  , textureByApplyingCIFilter
  , textureByGeneratingNormalMap
  , textureByGeneratingNormalMapWithSmoothness_contrast
  , cgImage
  , preloadTextures_withCompletionHandler
  , preloadWithCompletionHandler
  , filteringMode
  , setFilteringMode
  , usesMipmaps
  , setUsesMipmaps
  , textureWithImageNamedSelector
  , textureWithCGImageSelector
  , textureWithImageSelector
  , textureByApplyingCIFilterSelector
  , textureByGeneratingNormalMapSelector
  , textureByGeneratingNormalMapWithSmoothness_contrastSelector
  , cgImageSelector
  , preloadTextures_withCompletionHandlerSelector
  , preloadWithCompletionHandlerSelector
  , filteringModeSelector
  , setFilteringModeSelector
  , usesMipmapsSelector
  , setUsesMipmapsSelector

  -- * Enum types
  , SKTextureFilteringMode(SKTextureFilteringMode)
  , pattern SKTextureFilteringNearest
  , pattern SKTextureFilteringLinear

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a texture from an image file. Behaves similar to imageNamed: in UIImage or NSImage
--
-- @name@ — the name or path of the image to load.
--
-- ObjC selector: @+ textureWithImageNamed:@
textureWithImageNamed :: IsNSString name => name -> IO (Id SKTexture)
textureWithImageNamed name =
  do
    cls' <- getRequiredClass "SKTexture"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "textureWithImageNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Create a texture from a CGImageRef.
--
-- @image@ — the CGImageRef to create the texture from
--
-- ObjC selector: @+ textureWithCGImage:@
textureWithCGImage :: Ptr () -> IO (Id SKTexture)
textureWithCGImage image =
  do
    cls' <- getRequiredClass "SKTexture"
    sendClassMsg cls' (mkSelector "textureWithCGImage:") (retPtr retVoid) [argPtr image] >>= retainedObject . castPtr

-- | @+ textureWithImage:@
textureWithImage :: IsNSImage image => image -> IO (Id SKTexture)
textureWithImage image =
  do
    cls' <- getRequiredClass "SKTexture"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "textureWithImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | Create new texture by applying a CIFilter to an existing one. Any CIFilter that requires only a single "inputImage" and produces an "outputImage" is allowed.
--
-- @filter@ — the CI filter to apply in the copy.
--
-- ObjC selector: @- textureByApplyingCIFilter:@
textureByApplyingCIFilter :: (IsSKTexture skTexture, IsCIFilter filter_) => skTexture -> filter_ -> IO (Id SKTexture)
textureByApplyingCIFilter skTexture  filter_ =
withObjCPtr filter_ $ \raw_filter_ ->
    sendMsg skTexture (mkSelector "textureByApplyingCIFilter:") (retPtr retVoid) [argPtr (castPtr raw_filter_ :: Ptr ())] >>= retainedObject . castPtr

-- | Create new texture by generating a normal map texture.
--
-- ObjC selector: @- textureByGeneratingNormalMap@
textureByGeneratingNormalMap :: IsSKTexture skTexture => skTexture -> IO (Id SKTexture)
textureByGeneratingNormalMap skTexture  =
  sendMsg skTexture (mkSelector "textureByGeneratingNormalMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create new texture by generating a normal map texture.
--
-- @smoothness@ — the smooth level of the generated normal map.
--
-- @contrast@ — the scale applied to the generated normal map.
--
-- ObjC selector: @- textureByGeneratingNormalMapWithSmoothness:contrast:@
textureByGeneratingNormalMapWithSmoothness_contrast :: IsSKTexture skTexture => skTexture -> CDouble -> CDouble -> IO (Id SKTexture)
textureByGeneratingNormalMapWithSmoothness_contrast skTexture  smoothness contrast =
  sendMsg skTexture (mkSelector "textureByGeneratingNormalMapWithSmoothness:contrast:") (retPtr retVoid) [argCDouble (fromIntegral smoothness), argCDouble (fromIntegral contrast)] >>= retainedObject . castPtr

-- | Convert the current SKTexture into a CGImageRef object
--
-- ObjC selector: @- CGImage@
cgImage :: IsSKTexture skTexture => skTexture -> IO (Ptr ())
cgImage skTexture  =
  fmap castPtr $ sendMsg skTexture (mkSelector "CGImage") (retPtr retVoid) []

-- | Start a texture preload operation on an array of textures
--
-- @textures@ — an array of SKTextures to be preloaded
--
-- @completionHandler@ — will be called upon the preload completion
--
-- ObjC selector: @+ preloadTextures:withCompletionHandler:@
preloadTextures_withCompletionHandler :: IsNSArray textures => textures -> Ptr () -> IO ()
preloadTextures_withCompletionHandler textures completionHandler =
  do
    cls' <- getRequiredClass "SKTexture"
    withObjCPtr textures $ \raw_textures ->
      sendClassMsg cls' (mkSelector "preloadTextures:withCompletionHandler:") retVoid [argPtr (castPtr raw_textures :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Request that this texture be loaded into vram on the next render update, with a callback handler.
--
-- ObjC selector: @- preloadWithCompletionHandler:@
preloadWithCompletionHandler :: IsSKTexture skTexture => skTexture -> Ptr () -> IO ()
preloadWithCompletionHandler skTexture  completionHandler =
  sendMsg skTexture (mkSelector "preloadWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | The filtering mode the texture should use when not drawn at native size. Defaults to SKTextureFilteringLinear.
--
-- ObjC selector: @- filteringMode@
filteringMode :: IsSKTexture skTexture => skTexture -> IO SKTextureFilteringMode
filteringMode skTexture  =
  fmap (coerce :: CLong -> SKTextureFilteringMode) $ sendMsg skTexture (mkSelector "filteringMode") retCLong []

-- | The filtering mode the texture should use when not drawn at native size. Defaults to SKTextureFilteringLinear.
--
-- ObjC selector: @- setFilteringMode:@
setFilteringMode :: IsSKTexture skTexture => skTexture -> SKTextureFilteringMode -> IO ()
setFilteringMode skTexture  value =
  sendMsg skTexture (mkSelector "setFilteringMode:") retVoid [argCLong (coerce value)]

-- | Request that the texture have mipmaps generated if possible. Only supported for power of 2 texture sizes.
--
-- ObjC selector: @- usesMipmaps@
usesMipmaps :: IsSKTexture skTexture => skTexture -> IO Bool
usesMipmaps skTexture  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skTexture (mkSelector "usesMipmaps") retCULong []

-- | Request that the texture have mipmaps generated if possible. Only supported for power of 2 texture sizes.
--
-- ObjC selector: @- setUsesMipmaps:@
setUsesMipmaps :: IsSKTexture skTexture => skTexture -> Bool -> IO ()
setUsesMipmaps skTexture  value =
  sendMsg skTexture (mkSelector "setUsesMipmaps:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textureWithImageNamed:@
textureWithImageNamedSelector :: Selector
textureWithImageNamedSelector = mkSelector "textureWithImageNamed:"

-- | @Selector@ for @textureWithCGImage:@
textureWithCGImageSelector :: Selector
textureWithCGImageSelector = mkSelector "textureWithCGImage:"

-- | @Selector@ for @textureWithImage:@
textureWithImageSelector :: Selector
textureWithImageSelector = mkSelector "textureWithImage:"

-- | @Selector@ for @textureByApplyingCIFilter:@
textureByApplyingCIFilterSelector :: Selector
textureByApplyingCIFilterSelector = mkSelector "textureByApplyingCIFilter:"

-- | @Selector@ for @textureByGeneratingNormalMap@
textureByGeneratingNormalMapSelector :: Selector
textureByGeneratingNormalMapSelector = mkSelector "textureByGeneratingNormalMap"

-- | @Selector@ for @textureByGeneratingNormalMapWithSmoothness:contrast:@
textureByGeneratingNormalMapWithSmoothness_contrastSelector :: Selector
textureByGeneratingNormalMapWithSmoothness_contrastSelector = mkSelector "textureByGeneratingNormalMapWithSmoothness:contrast:"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @preloadTextures:withCompletionHandler:@
preloadTextures_withCompletionHandlerSelector :: Selector
preloadTextures_withCompletionHandlerSelector = mkSelector "preloadTextures:withCompletionHandler:"

-- | @Selector@ for @preloadWithCompletionHandler:@
preloadWithCompletionHandlerSelector :: Selector
preloadWithCompletionHandlerSelector = mkSelector "preloadWithCompletionHandler:"

-- | @Selector@ for @filteringMode@
filteringModeSelector :: Selector
filteringModeSelector = mkSelector "filteringMode"

-- | @Selector@ for @setFilteringMode:@
setFilteringModeSelector :: Selector
setFilteringModeSelector = mkSelector "setFilteringMode:"

-- | @Selector@ for @usesMipmaps@
usesMipmapsSelector :: Selector
usesMipmapsSelector = mkSelector "usesMipmaps"

-- | @Selector@ for @setUsesMipmaps:@
setUsesMipmapsSelector :: Selector
setUsesMipmapsSelector = mkSelector "setUsesMipmaps:"

