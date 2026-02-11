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
  , initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector
  , initWithCGLPBufferObjSelector
  , cglpBufferObjSelector
  , pixelsWideSelector
  , pixelsHighSelector
  , textureTargetSelector
  , textureInternalFormatSelector
  , textureMaxMipMapLevelSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:@
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> CUInt -> CUInt -> CInt -> CInt -> CInt -> IO (Id NSOpenGLPixelBuffer)
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHigh nsOpenGLPixelBuffer  target format maxLevel pixelsWide pixelsHigh =
  sendMsg nsOpenGLPixelBuffer (mkSelector "initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:") (retPtr retVoid) [argCUInt (fromIntegral target), argCUInt (fromIntegral format), argCInt (fromIntegral maxLevel), argCInt (fromIntegral pixelsWide), argCInt (fromIntegral pixelsHigh)] >>= ownedObject . castPtr

-- | @- initWithCGLPBufferObj:@
initWithCGLPBufferObj :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> Ptr () -> IO (Id NSOpenGLPixelBuffer)
initWithCGLPBufferObj nsOpenGLPixelBuffer  pbuffer =
  sendMsg nsOpenGLPixelBuffer (mkSelector "initWithCGLPBufferObj:") (retPtr retVoid) [argPtr pbuffer] >>= ownedObject . castPtr

-- | @- CGLPBufferObj@
cglpBufferObj :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO (Ptr ())
cglpBufferObj nsOpenGLPixelBuffer  =
  fmap castPtr $ sendMsg nsOpenGLPixelBuffer (mkSelector "CGLPBufferObj") (retPtr retVoid) []

-- | @- pixelsWide@
pixelsWide :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CInt
pixelsWide nsOpenGLPixelBuffer  =
  sendMsg nsOpenGLPixelBuffer (mkSelector "pixelsWide") retCInt []

-- | @- pixelsHigh@
pixelsHigh :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CInt
pixelsHigh nsOpenGLPixelBuffer  =
  sendMsg nsOpenGLPixelBuffer (mkSelector "pixelsHigh") retCInt []

-- | @- textureTarget@
textureTarget :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CUInt
textureTarget nsOpenGLPixelBuffer  =
  sendMsg nsOpenGLPixelBuffer (mkSelector "textureTarget") retCUInt []

-- | @- textureInternalFormat@
textureInternalFormat :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CUInt
textureInternalFormat nsOpenGLPixelBuffer  =
  sendMsg nsOpenGLPixelBuffer (mkSelector "textureInternalFormat") retCUInt []

-- | @- textureMaxMipMapLevel@
textureMaxMipMapLevel :: IsNSOpenGLPixelBuffer nsOpenGLPixelBuffer => nsOpenGLPixelBuffer -> IO CInt
textureMaxMipMapLevel nsOpenGLPixelBuffer  =
  sendMsg nsOpenGLPixelBuffer (mkSelector "textureMaxMipMapLevel") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:@
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector :: Selector
initWithTextureTarget_textureInternalFormat_textureMaxMipMapLevel_pixelsWide_pixelsHighSelector = mkSelector "initWithTextureTarget:textureInternalFormat:textureMaxMipMapLevel:pixelsWide:pixelsHigh:"

-- | @Selector@ for @initWithCGLPBufferObj:@
initWithCGLPBufferObjSelector :: Selector
initWithCGLPBufferObjSelector = mkSelector "initWithCGLPBufferObj:"

-- | @Selector@ for @CGLPBufferObj@
cglpBufferObjSelector :: Selector
cglpBufferObjSelector = mkSelector "CGLPBufferObj"

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector
pixelsHighSelector = mkSelector "pixelsHigh"

-- | @Selector@ for @textureTarget@
textureTargetSelector :: Selector
textureTargetSelector = mkSelector "textureTarget"

-- | @Selector@ for @textureInternalFormat@
textureInternalFormatSelector :: Selector
textureInternalFormatSelector = mkSelector "textureInternalFormat"

-- | @Selector@ for @textureMaxMipMapLevel@
textureMaxMipMapLevelSelector :: Selector
textureMaxMipMapLevelSelector = mkSelector "textureMaxMipMapLevel"

