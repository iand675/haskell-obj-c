{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cgImageSelector
  , filteringModeSelector
  , preloadTextures_withCompletionHandlerSelector
  , preloadWithCompletionHandlerSelector
  , setFilteringModeSelector
  , setUsesMipmapsSelector
  , textureByApplyingCIFilterSelector
  , textureByGeneratingNormalMapSelector
  , textureByGeneratingNormalMapWithSmoothness_contrastSelector
  , textureWithCGImageSelector
  , textureWithImageNamedSelector
  , textureWithImageSelector
  , usesMipmapsSelector

  -- * Enum types
  , SKTextureFilteringMode(SKTextureFilteringMode)
  , pattern SKTextureFilteringNearest
  , pattern SKTextureFilteringLinear

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

-- | Create a texture from an image file. Behaves similar to imageNamed: in UIImage or NSImage
--
-- @name@ — the name or path of the image to load.
--
-- ObjC selector: @+ textureWithImageNamed:@
textureWithImageNamed :: IsNSString name => name -> IO (Id SKTexture)
textureWithImageNamed name =
  do
    cls' <- getRequiredClass "SKTexture"
    sendClassMessage cls' textureWithImageNamedSelector (toNSString name)

-- | Create a texture from a CGImageRef.
--
-- @image@ — the CGImageRef to create the texture from
--
-- ObjC selector: @+ textureWithCGImage:@
textureWithCGImage :: Ptr () -> IO (Id SKTexture)
textureWithCGImage image =
  do
    cls' <- getRequiredClass "SKTexture"
    sendClassMessage cls' textureWithCGImageSelector image

-- | @+ textureWithImage:@
textureWithImage :: IsNSImage image => image -> IO (Id SKTexture)
textureWithImage image =
  do
    cls' <- getRequiredClass "SKTexture"
    sendClassMessage cls' textureWithImageSelector (toNSImage image)

-- | Create new texture by applying a CIFilter to an existing one. Any CIFilter that requires only a single "inputImage" and produces an "outputImage" is allowed.
--
-- @filter@ — the CI filter to apply in the copy.
--
-- ObjC selector: @- textureByApplyingCIFilter:@
textureByApplyingCIFilter :: (IsSKTexture skTexture, IsCIFilter filter_) => skTexture -> filter_ -> IO (Id SKTexture)
textureByApplyingCIFilter skTexture filter_ =
  sendMessage skTexture textureByApplyingCIFilterSelector (toCIFilter filter_)

-- | Create new texture by generating a normal map texture.
--
-- ObjC selector: @- textureByGeneratingNormalMap@
textureByGeneratingNormalMap :: IsSKTexture skTexture => skTexture -> IO (Id SKTexture)
textureByGeneratingNormalMap skTexture =
  sendMessage skTexture textureByGeneratingNormalMapSelector

-- | Create new texture by generating a normal map texture.
--
-- @smoothness@ — the smooth level of the generated normal map.
--
-- @contrast@ — the scale applied to the generated normal map.
--
-- ObjC selector: @- textureByGeneratingNormalMapWithSmoothness:contrast:@
textureByGeneratingNormalMapWithSmoothness_contrast :: IsSKTexture skTexture => skTexture -> CDouble -> CDouble -> IO (Id SKTexture)
textureByGeneratingNormalMapWithSmoothness_contrast skTexture smoothness contrast =
  sendMessage skTexture textureByGeneratingNormalMapWithSmoothness_contrastSelector smoothness contrast

-- | Convert the current SKTexture into a CGImageRef object
--
-- ObjC selector: @- CGImage@
cgImage :: IsSKTexture skTexture => skTexture -> IO (Ptr ())
cgImage skTexture =
  sendMessage skTexture cgImageSelector

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
    sendClassMessage cls' preloadTextures_withCompletionHandlerSelector (toNSArray textures) completionHandler

-- | Request that this texture be loaded into vram on the next render update, with a callback handler.
--
-- ObjC selector: @- preloadWithCompletionHandler:@
preloadWithCompletionHandler :: IsSKTexture skTexture => skTexture -> Ptr () -> IO ()
preloadWithCompletionHandler skTexture completionHandler =
  sendMessage skTexture preloadWithCompletionHandlerSelector completionHandler

-- | The filtering mode the texture should use when not drawn at native size. Defaults to SKTextureFilteringLinear.
--
-- ObjC selector: @- filteringMode@
filteringMode :: IsSKTexture skTexture => skTexture -> IO SKTextureFilteringMode
filteringMode skTexture =
  sendMessage skTexture filteringModeSelector

-- | The filtering mode the texture should use when not drawn at native size. Defaults to SKTextureFilteringLinear.
--
-- ObjC selector: @- setFilteringMode:@
setFilteringMode :: IsSKTexture skTexture => skTexture -> SKTextureFilteringMode -> IO ()
setFilteringMode skTexture value =
  sendMessage skTexture setFilteringModeSelector value

-- | Request that the texture have mipmaps generated if possible. Only supported for power of 2 texture sizes.
--
-- ObjC selector: @- usesMipmaps@
usesMipmaps :: IsSKTexture skTexture => skTexture -> IO Bool
usesMipmaps skTexture =
  sendMessage skTexture usesMipmapsSelector

-- | Request that the texture have mipmaps generated if possible. Only supported for power of 2 texture sizes.
--
-- ObjC selector: @- setUsesMipmaps:@
setUsesMipmaps :: IsSKTexture skTexture => skTexture -> Bool -> IO ()
setUsesMipmaps skTexture value =
  sendMessage skTexture setUsesMipmapsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textureWithImageNamed:@
textureWithImageNamedSelector :: Selector '[Id NSString] (Id SKTexture)
textureWithImageNamedSelector = mkSelector "textureWithImageNamed:"

-- | @Selector@ for @textureWithCGImage:@
textureWithCGImageSelector :: Selector '[Ptr ()] (Id SKTexture)
textureWithCGImageSelector = mkSelector "textureWithCGImage:"

-- | @Selector@ for @textureWithImage:@
textureWithImageSelector :: Selector '[Id NSImage] (Id SKTexture)
textureWithImageSelector = mkSelector "textureWithImage:"

-- | @Selector@ for @textureByApplyingCIFilter:@
textureByApplyingCIFilterSelector :: Selector '[Id CIFilter] (Id SKTexture)
textureByApplyingCIFilterSelector = mkSelector "textureByApplyingCIFilter:"

-- | @Selector@ for @textureByGeneratingNormalMap@
textureByGeneratingNormalMapSelector :: Selector '[] (Id SKTexture)
textureByGeneratingNormalMapSelector = mkSelector "textureByGeneratingNormalMap"

-- | @Selector@ for @textureByGeneratingNormalMapWithSmoothness:contrast:@
textureByGeneratingNormalMapWithSmoothness_contrastSelector :: Selector '[CDouble, CDouble] (Id SKTexture)
textureByGeneratingNormalMapWithSmoothness_contrastSelector = mkSelector "textureByGeneratingNormalMapWithSmoothness:contrast:"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector '[] (Ptr ())
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @preloadTextures:withCompletionHandler:@
preloadTextures_withCompletionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
preloadTextures_withCompletionHandlerSelector = mkSelector "preloadTextures:withCompletionHandler:"

-- | @Selector@ for @preloadWithCompletionHandler:@
preloadWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
preloadWithCompletionHandlerSelector = mkSelector "preloadWithCompletionHandler:"

-- | @Selector@ for @filteringMode@
filteringModeSelector :: Selector '[] SKTextureFilteringMode
filteringModeSelector = mkSelector "filteringMode"

-- | @Selector@ for @setFilteringMode:@
setFilteringModeSelector :: Selector '[SKTextureFilteringMode] ()
setFilteringModeSelector = mkSelector "setFilteringMode:"

-- | @Selector@ for @usesMipmaps@
usesMipmapsSelector :: Selector '[] Bool
usesMipmapsSelector = mkSelector "usesMipmaps"

-- | @Selector@ for @setUsesMipmaps:@
setUsesMipmapsSelector :: Selector '[Bool] ()
setUsesMipmapsSelector = mkSelector "setUsesMipmaps:"

