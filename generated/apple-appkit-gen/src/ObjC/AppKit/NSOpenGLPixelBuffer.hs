{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ******************** NSOpenGLPixelBuffer*******************
--
-- Generated bindings for @NSOpenGLPixelBuffer@.
module ObjC.AppKit.NSOpenGLPixelBuffer
  ( NSOpenGLPixelBuffer
  , IsNSOpenGLPixelBuffer(..)
  , initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh
  , initWithCGLPBufferObj
  , cglpBufferObj
  , pixelsWide
  , pixelsHigh
  , textureTarget
  , textureInternalFormat
  , textureMaxMipMapLevel
  , cglpBufferObjSelector
  , initWithCGLPBufferObjSelector
  , initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector
  , pixelsHighSelector
  , pixelsWideSelector
  , textureInternalFormatSelector
  , textureMaxMipMapLevelSelector
  , textureTargetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:@
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> CUInt -> CUInt -> CInt -> CInt -> CInt -> IO (Id NSOpenGLPixelBuffer)
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh nsOpenGLPixelBuffer target format maxLevel pixelsWide pixelsHigh =
  sendOwnedMessage nsOpenGLPixelBuffer initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector target format maxLevel pixelsWide pixelsHigh

-- | @- initWithCGLPBufferObj:@
initWithCGLPBufferObj :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> Ptr () -> IO (Id NSOpenGLPixelBuffer)
initWithCGLPBufferObj nsOpenGLPixelBuffer pbuffer =
  sendOwnedMessage nsOpenGLPixelBuffer initWithCGLPBufferObjSelector pbuffer

-- | @- CGLPBufferObj@
cglpBufferObj :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO (Ptr ())
cglpBufferObj nsOpenGLPixelBuffer =
  sendMessage nsOpenGLPixelBuffer cglpBufferObjSelector

-- | @- pixelsWide@
pixelsWide :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CInt
pixelsWide nsOpenGLPixelBuffer =
  sendMessage nsOpenGLPixelBuffer pixelsWideSelector

-- | @- pixelsHigh@
pixelsHigh :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CInt
pixelsHigh nsOpenGLPixelBuffer =
  sendMessage nsOpenGLPixelBuffer pixelsHighSelector

-- | @- textureTarget@
textureTarget :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CUInt
textureTarget nsOpenGLPixelBuffer =
  sendMessage nsOpenGLPixelBuffer textureTargetSelector

-- | @- textureInternalFormat@
textureInternalFormat :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CUInt
textureInternalFormat nsOpenGLPixelBuffer =
  sendMessage nsOpenGLPixelBuffer textureInternalFormatSelector

-- | @- textureMaxMipMapLevel@
textureMaxMipMapLevel :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CInt
textureMaxMipMapLevel nsOpenGLPixelBuffer =
  sendMessage nsOpenGLPixelBuffer textureMaxMipMapLevelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:@
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector :: Selector '[CUInt, CUInt, CInt, CInt, CInt] (Id NSOpenGLPixelBuffer)
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector = mkSelector "initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:"

-- | @Selector@ for @initWithCGLPBufferObj:@
initWithCGLPBufferObjSelector :: Selector '[Ptr ()] (Id NSOpenGLPixelBuffer)
initWithCGLPBufferObjSelector = mkSelector "initWithCGLPBufferObj:"

-- | @Selector@ for @CGLPBufferObj@
cglpBufferObjSelector :: Selector '[] (Ptr ())
cglpBufferObjSelector = mkSelector "CGLPBufferObj"

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector '[] CInt
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector '[] CInt
pixelsHighSelector = mkSelector "pixelsHigh"

-- | @Selector@ for @textureTarget@
textureTargetSelector :: Selector '[] CUInt
textureTargetSelector = mkSelector "textureTarget"

-- | @Selector@ for @textureInternalFormat@
textureInternalFormatSelector :: Selector '[] CUInt
textureInternalFormatSelector = mkSelector "textureInternalFormat"

-- | @Selector@ for @textureMaxMipMapLevel@
textureMaxMipMapLevelSelector :: Selector '[] CInt
textureMaxMipMapLevelSelector = mkSelector "textureMaxMipMapLevel"

