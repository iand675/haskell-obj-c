{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOpenGLContext@.
module ObjC.AppKit.NSOpenGLContext
  ( NSOpenGLContext
  , IsNSOpenGLContext(..)
  , initWithFormat_shareContext
  , initWithCGLContextObj
  , setView
  , setFullScreen
  , setOffScreen_width_height_rowbytes
  , clearDrawable
  , update
  , flushBuffer
  , makeCurrentContext
  , clearCurrentContext
  , copyAttributesFromContext_withMask
  , createTexture_fromView_internalFormat
  , setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen
  , pixelBuffer
  , pixelBufferCubeMapFace
  , pixelBufferMipMapLevel
  , setTextureImageToPixelBuffer_colorBuffer
  , currentContext
  , currentVirtualScreen
  , setCurrentVirtualScreen
  , cglContextObj
  , initWithFormat_shareContextSelector
  , initWithCGLContextObjSelector
  , setViewSelector
  , setFullScreenSelector
  , setOffScreen_width_height_rowbytesSelector
  , clearDrawableSelector
  , updateSelector
  , flushBufferSelector
  , makeCurrentContextSelector
  , clearCurrentContextSelector
  , copyAttributesFromContext_withMaskSelector
  , createTexture_fromView_internalFormatSelector
  , setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector
  , pixelBufferSelector
  , pixelBufferCubeMapFaceSelector
  , pixelBufferMipMapLevelSelector
  , setTextureImageToPixelBuffer_colorBufferSelector
  , currentContextSelector
  , currentVirtualScreenSelector
  , setCurrentVirtualScreenSelector
  , cglContextObjSelector

  -- * Enum types
  , NSOpenGLContextParameter(NSOpenGLContextParameter)
  , pattern NSOpenGLContextParameterSwapInterval
  , pattern NSOpenGLContextParameterSurfaceOrder
  , pattern NSOpenGLContextParameterSurfaceOpacity
  , pattern NSOpenGLContextParameterSurfaceBackingSize
  , pattern NSOpenGLContextParameterReclaimResources
  , pattern NSOpenGLContextParameterCurrentRendererID
  , pattern NSOpenGLContextParameterGPUVertexProcessing
  , pattern NSOpenGLContextParameterGPUFragmentProcessing
  , pattern NSOpenGLContextParameterHasDrawable
  , pattern NSOpenGLContextParameterMPSwapsInFlight
  , pattern NSOpenGLContextParameterSwapRectangle
  , pattern NSOpenGLContextParameterSwapRectangleEnable
  , pattern NSOpenGLContextParameterRasterizationEnable
  , pattern NSOpenGLContextParameterStateValidation
  , pattern NSOpenGLContextParameterSurfaceSurfaceVolatile

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFormat:shareContext:@
initWithFormat_shareContext :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLPixelFormat format, IsNSOpenGLContext share) => nsOpenGLContext -> format -> share -> IO (Id NSOpenGLContext)
initWithFormat_shareContext nsOpenGLContext  format share =
withObjCPtr format $ \raw_format ->
  withObjCPtr share $ \raw_share ->
      sendMsg nsOpenGLContext (mkSelector "initWithFormat:shareContext:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_share :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCGLContextObj:@
initWithCGLContextObj :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> Ptr () -> IO (Id NSOpenGLContext)
initWithCGLContextObj nsOpenGLContext  context =
  sendMsg nsOpenGLContext (mkSelector "initWithCGLContextObj:") (retPtr retVoid) [argPtr context] >>= ownedObject . castPtr

-- | @- setView:@
setView :: (IsNSOpenGLContext nsOpenGLContext, IsNSView view) => nsOpenGLContext -> view -> IO ()
setView nsOpenGLContext  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsOpenGLContext (mkSelector "setView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- setFullScreen@
setFullScreen :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
setFullScreen nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "setFullScreen") retVoid []

-- | @- setOffScreen:width:height:rowbytes:@
setOffScreen_width_height_rowbytes :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> Ptr () -> CInt -> CInt -> CInt -> IO ()
setOffScreen_width_height_rowbytes nsOpenGLContext  baseaddr width height rowbytes =
  sendMsg nsOpenGLContext (mkSelector "setOffScreen:width:height:rowbytes:") retVoid [argPtr baseaddr, argCInt (fromIntegral width), argCInt (fromIntegral height), argCInt (fromIntegral rowbytes)]

-- | @- clearDrawable@
clearDrawable :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
clearDrawable nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "clearDrawable") retVoid []

-- | @- update@
update :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
update nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "update") retVoid []

-- | @- flushBuffer@
flushBuffer :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
flushBuffer nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "flushBuffer") retVoid []

-- | @- makeCurrentContext@
makeCurrentContext :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
makeCurrentContext nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "makeCurrentContext") retVoid []

-- | @+ clearCurrentContext@
clearCurrentContext :: IO ()
clearCurrentContext  =
  do
    cls' <- getRequiredClass "NSOpenGLContext"
    sendClassMsg cls' (mkSelector "clearCurrentContext") retVoid []

-- | @- copyAttributesFromContext:withMask:@
copyAttributesFromContext_withMask :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLContext context) => nsOpenGLContext -> context -> CUInt -> IO ()
copyAttributesFromContext_withMask nsOpenGLContext  context mask =
withObjCPtr context $ \raw_context ->
    sendMsg nsOpenGLContext (mkSelector "copyAttributesFromContext:withMask:") retVoid [argPtr (castPtr raw_context :: Ptr ()), argCUInt (fromIntegral mask)]

-- | @- createTexture:fromView:internalFormat:@
createTexture_fromView_internalFormat :: (IsNSOpenGLContext nsOpenGLContext, IsNSView view) => nsOpenGLContext -> CUInt -> view -> CUInt -> IO ()
createTexture_fromView_internalFormat nsOpenGLContext  target view format =
withObjCPtr view $ \raw_view ->
    sendMsg nsOpenGLContext (mkSelector "createTexture:fromView:internalFormat:") retVoid [argCUInt (fromIntegral target), argPtr (castPtr raw_view :: Ptr ()), argCUInt (fromIntegral format)]

-- | @- setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:@
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLPixelBuffer pixelBuffer) => nsOpenGLContext -> pixelBuffer -> CUInt -> CInt -> CInt -> IO ()
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen nsOpenGLContext  pixelBuffer face level screen =
withObjCPtr pixelBuffer $ \raw_pixelBuffer ->
    sendMsg nsOpenGLContext (mkSelector "setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:") retVoid [argPtr (castPtr raw_pixelBuffer :: Ptr ()), argCUInt (fromIntegral face), argCInt (fromIntegral level), argCInt (fromIntegral screen)]

-- | @- pixelBuffer@
pixelBuffer :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO (Id NSOpenGLPixelBuffer)
pixelBuffer nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "pixelBuffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pixelBufferCubeMapFace@
pixelBufferCubeMapFace :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO CUInt
pixelBufferCubeMapFace nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "pixelBufferCubeMapFace") retCUInt []

-- | @- pixelBufferMipMapLevel@
pixelBufferMipMapLevel :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO CInt
pixelBufferMipMapLevel nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "pixelBufferMipMapLevel") retCInt []

-- | @- setTextureImageToPixelBuffer:colorBuffer:@
setTextureImageToPixelBuffer_colorBuffer :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLPixelBuffer pixelBuffer) => nsOpenGLContext -> pixelBuffer -> CUInt -> IO ()
setTextureImageToPixelBuffer_colorBuffer nsOpenGLContext  pixelBuffer source =
withObjCPtr pixelBuffer $ \raw_pixelBuffer ->
    sendMsg nsOpenGLContext (mkSelector "setTextureImageToPixelBuffer:colorBuffer:") retVoid [argPtr (castPtr raw_pixelBuffer :: Ptr ()), argCUInt (fromIntegral source)]

-- | @+ currentContext@
currentContext :: IO (Id NSOpenGLContext)
currentContext  =
  do
    cls' <- getRequiredClass "NSOpenGLContext"
    sendClassMsg cls' (mkSelector "currentContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentVirtualScreen@
currentVirtualScreen :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO CInt
currentVirtualScreen nsOpenGLContext  =
  sendMsg nsOpenGLContext (mkSelector "currentVirtualScreen") retCInt []

-- | @- setCurrentVirtualScreen:@
setCurrentVirtualScreen :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> CInt -> IO ()
setCurrentVirtualScreen nsOpenGLContext  value =
  sendMsg nsOpenGLContext (mkSelector "setCurrentVirtualScreen:") retVoid [argCInt (fromIntegral value)]

-- | @- CGLContextObj@
cglContextObj :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO (Ptr ())
cglContextObj nsOpenGLContext  =
  fmap castPtr $ sendMsg nsOpenGLContext (mkSelector "CGLContextObj") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormat:shareContext:@
initWithFormat_shareContextSelector :: Selector
initWithFormat_shareContextSelector = mkSelector "initWithFormat:shareContext:"

-- | @Selector@ for @initWithCGLContextObj:@
initWithCGLContextObjSelector :: Selector
initWithCGLContextObjSelector = mkSelector "initWithCGLContextObj:"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @setFullScreen@
setFullScreenSelector :: Selector
setFullScreenSelector = mkSelector "setFullScreen"

-- | @Selector@ for @setOffScreen:width:height:rowbytes:@
setOffScreen_width_height_rowbytesSelector :: Selector
setOffScreen_width_height_rowbytesSelector = mkSelector "setOffScreen:width:height:rowbytes:"

-- | @Selector@ for @clearDrawable@
clearDrawableSelector :: Selector
clearDrawableSelector = mkSelector "clearDrawable"

-- | @Selector@ for @update@
updateSelector :: Selector
updateSelector = mkSelector "update"

-- | @Selector@ for @flushBuffer@
flushBufferSelector :: Selector
flushBufferSelector = mkSelector "flushBuffer"

-- | @Selector@ for @makeCurrentContext@
makeCurrentContextSelector :: Selector
makeCurrentContextSelector = mkSelector "makeCurrentContext"

-- | @Selector@ for @clearCurrentContext@
clearCurrentContextSelector :: Selector
clearCurrentContextSelector = mkSelector "clearCurrentContext"

-- | @Selector@ for @copyAttributesFromContext:withMask:@
copyAttributesFromContext_withMaskSelector :: Selector
copyAttributesFromContext_withMaskSelector = mkSelector "copyAttributesFromContext:withMask:"

-- | @Selector@ for @createTexture:fromView:internalFormat:@
createTexture_fromView_internalFormatSelector :: Selector
createTexture_fromView_internalFormatSelector = mkSelector "createTexture:fromView:internalFormat:"

-- | @Selector@ for @setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:@
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector :: Selector
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector = mkSelector "setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @pixelBufferCubeMapFace@
pixelBufferCubeMapFaceSelector :: Selector
pixelBufferCubeMapFaceSelector = mkSelector "pixelBufferCubeMapFace"

-- | @Selector@ for @pixelBufferMipMapLevel@
pixelBufferMipMapLevelSelector :: Selector
pixelBufferMipMapLevelSelector = mkSelector "pixelBufferMipMapLevel"

-- | @Selector@ for @setTextureImageToPixelBuffer:colorBuffer:@
setTextureImageToPixelBuffer_colorBufferSelector :: Selector
setTextureImageToPixelBuffer_colorBufferSelector = mkSelector "setTextureImageToPixelBuffer:colorBuffer:"

-- | @Selector@ for @currentContext@
currentContextSelector :: Selector
currentContextSelector = mkSelector "currentContext"

-- | @Selector@ for @currentVirtualScreen@
currentVirtualScreenSelector :: Selector
currentVirtualScreenSelector = mkSelector "currentVirtualScreen"

-- | @Selector@ for @setCurrentVirtualScreen:@
setCurrentVirtualScreenSelector :: Selector
setCurrentVirtualScreenSelector = mkSelector "setCurrentVirtualScreen:"

-- | @Selector@ for @CGLContextObj@
cglContextObjSelector :: Selector
cglContextObjSelector = mkSelector "CGLContextObj"

