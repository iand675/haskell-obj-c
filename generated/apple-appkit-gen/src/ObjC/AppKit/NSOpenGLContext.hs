{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setValues_forParameter
  , getValues_forParameter
  , createTexture_fromView_internalFormat
  , setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen
  , pixelBuffer
  , pixelBufferCubeMapFace
  , pixelBufferMipMapLevel
  , setTextureImageToPixelBuffer_colorBuffer
  , pixelFormat
  , view
  , currentContext
  , currentVirtualScreen
  , setCurrentVirtualScreen
  , cglContextObj
  , cglContextObjSelector
  , clearCurrentContextSelector
  , clearDrawableSelector
  , copyAttributesFromContext_withMaskSelector
  , createTexture_fromView_internalFormatSelector
  , currentContextSelector
  , currentVirtualScreenSelector
  , flushBufferSelector
  , getValues_forParameterSelector
  , initWithCGLContextObjSelector
  , initWithFormat_shareContextSelector
  , makeCurrentContextSelector
  , pixelBufferCubeMapFaceSelector
  , pixelBufferMipMapLevelSelector
  , pixelBufferSelector
  , pixelFormatSelector
  , setCurrentVirtualScreenSelector
  , setFullScreenSelector
  , setOffScreen_width_height_rowbytesSelector
  , setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector
  , setTextureImageToPixelBuffer_colorBufferSelector
  , setValues_forParameterSelector
  , setViewSelector
  , updateSelector
  , viewSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithFormat:shareContext:@
initWithFormat_shareContext :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLPixelFormat format, IsNSOpenGLContext share) => nsOpenGLContext -> format -> share -> IO (Id NSOpenGLContext)
initWithFormat_shareContext nsOpenGLContext format share =
  sendOwnedMessage nsOpenGLContext initWithFormat_shareContextSelector (toNSOpenGLPixelFormat format) (toNSOpenGLContext share)

-- | @- initWithCGLContextObj:@
initWithCGLContextObj :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> Ptr () -> IO (Id NSOpenGLContext)
initWithCGLContextObj nsOpenGLContext context =
  sendOwnedMessage nsOpenGLContext initWithCGLContextObjSelector context

-- | @- setView:@
setView :: (IsNSOpenGLContext nsOpenGLContext, IsNSView view) => nsOpenGLContext -> view -> IO ()
setView nsOpenGLContext view =
  sendMessage nsOpenGLContext setViewSelector (toNSView view)

-- | @- setFullScreen@
setFullScreen :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
setFullScreen nsOpenGLContext =
  sendMessage nsOpenGLContext setFullScreenSelector

-- | @- setOffScreen:width:height:rowbytes:@
setOffScreen_width_height_rowbytes :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> Ptr () -> CInt -> CInt -> CInt -> IO ()
setOffScreen_width_height_rowbytes nsOpenGLContext baseaddr width height rowbytes =
  sendMessage nsOpenGLContext setOffScreen_width_height_rowbytesSelector baseaddr width height rowbytes

-- | @- clearDrawable@
clearDrawable :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
clearDrawable nsOpenGLContext =
  sendMessage nsOpenGLContext clearDrawableSelector

-- | @- update@
update :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
update nsOpenGLContext =
  sendMessage nsOpenGLContext updateSelector

-- | @- flushBuffer@
flushBuffer :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
flushBuffer nsOpenGLContext =
  sendMessage nsOpenGLContext flushBufferSelector

-- | @- makeCurrentContext@
makeCurrentContext :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO ()
makeCurrentContext nsOpenGLContext =
  sendMessage nsOpenGLContext makeCurrentContextSelector

-- | @+ clearCurrentContext@
clearCurrentContext :: IO ()
clearCurrentContext  =
  do
    cls' <- getRequiredClass "NSOpenGLContext"
    sendClassMessage cls' clearCurrentContextSelector

-- | @- copyAttributesFromContext:withMask:@
copyAttributesFromContext_withMask :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLContext context) => nsOpenGLContext -> context -> CUInt -> IO ()
copyAttributesFromContext_withMask nsOpenGLContext context mask =
  sendOwnedMessage nsOpenGLContext copyAttributesFromContext_withMaskSelector (toNSOpenGLContext context) mask

-- | @- setValues:forParameter:@
setValues_forParameter :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> Const RawId -> NSOpenGLContextParameter -> IO ()
setValues_forParameter nsOpenGLContext vals param =
  sendMessage nsOpenGLContext setValues_forParameterSelector vals param

-- | @- getValues:forParameter:@
getValues_forParameter :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> RawId -> NSOpenGLContextParameter -> IO ()
getValues_forParameter nsOpenGLContext vals param =
  sendMessage nsOpenGLContext getValues_forParameterSelector vals param

-- | @- createTexture:fromView:internalFormat:@
createTexture_fromView_internalFormat :: (IsNSOpenGLContext nsOpenGLContext, IsNSView view) => nsOpenGLContext -> CUInt -> view -> CUInt -> IO ()
createTexture_fromView_internalFormat nsOpenGLContext target view format =
  sendMessage nsOpenGLContext createTexture_fromView_internalFormatSelector target (toNSView view) format

-- | @- setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:@
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLPixelBuffer pixelBuffer) => nsOpenGLContext -> pixelBuffer -> CUInt -> CInt -> CInt -> IO ()
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreen nsOpenGLContext pixelBuffer face level screen =
  sendMessage nsOpenGLContext setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector (toNSOpenGLPixelBuffer pixelBuffer) face level screen

-- | @- pixelBuffer@
pixelBuffer :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO (Id NSOpenGLPixelBuffer)
pixelBuffer nsOpenGLContext =
  sendMessage nsOpenGLContext pixelBufferSelector

-- | @- pixelBufferCubeMapFace@
pixelBufferCubeMapFace :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO CUInt
pixelBufferCubeMapFace nsOpenGLContext =
  sendMessage nsOpenGLContext pixelBufferCubeMapFaceSelector

-- | @- pixelBufferMipMapLevel@
pixelBufferMipMapLevel :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO CInt
pixelBufferMipMapLevel nsOpenGLContext =
  sendMessage nsOpenGLContext pixelBufferMipMapLevelSelector

-- | @- setTextureImageToPixelBuffer:colorBuffer:@
setTextureImageToPixelBuffer_colorBuffer :: (IsNSOpenGLContext nsOpenGLContext, IsNSOpenGLPixelBuffer pixelBuffer) => nsOpenGLContext -> pixelBuffer -> CUInt -> IO ()
setTextureImageToPixelBuffer_colorBuffer nsOpenGLContext pixelBuffer source =
  sendMessage nsOpenGLContext setTextureImageToPixelBuffer_colorBufferSelector (toNSOpenGLPixelBuffer pixelBuffer) source

-- | @- pixelFormat@
pixelFormat :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO (Id NSOpenGLPixelFormat)
pixelFormat nsOpenGLContext =
  sendMessage nsOpenGLContext pixelFormatSelector

-- | @- view@
view :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO RawId
view nsOpenGLContext =
  sendMessage nsOpenGLContext viewSelector

-- | @+ currentContext@
currentContext :: IO (Id NSOpenGLContext)
currentContext  =
  do
    cls' <- getRequiredClass "NSOpenGLContext"
    sendClassMessage cls' currentContextSelector

-- | @- currentVirtualScreen@
currentVirtualScreen :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO CInt
currentVirtualScreen nsOpenGLContext =
  sendMessage nsOpenGLContext currentVirtualScreenSelector

-- | @- setCurrentVirtualScreen:@
setCurrentVirtualScreen :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> CInt -> IO ()
setCurrentVirtualScreen nsOpenGLContext value =
  sendMessage nsOpenGLContext setCurrentVirtualScreenSelector value

-- | @- CGLContextObj@
cglContextObj :: IsNSOpenGLContext nsOpenGLContext => nsOpenGLContext -> IO (Ptr ())
cglContextObj nsOpenGLContext =
  sendMessage nsOpenGLContext cglContextObjSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormat:shareContext:@
initWithFormat_shareContextSelector :: Selector '[Id NSOpenGLPixelFormat, Id NSOpenGLContext] (Id NSOpenGLContext)
initWithFormat_shareContextSelector = mkSelector "initWithFormat:shareContext:"

-- | @Selector@ for @initWithCGLContextObj:@
initWithCGLContextObjSelector :: Selector '[Ptr ()] (Id NSOpenGLContext)
initWithCGLContextObjSelector = mkSelector "initWithCGLContextObj:"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @setFullScreen@
setFullScreenSelector :: Selector '[] ()
setFullScreenSelector = mkSelector "setFullScreen"

-- | @Selector@ for @setOffScreen:width:height:rowbytes:@
setOffScreen_width_height_rowbytesSelector :: Selector '[Ptr (), CInt, CInt, CInt] ()
setOffScreen_width_height_rowbytesSelector = mkSelector "setOffScreen:width:height:rowbytes:"

-- | @Selector@ for @clearDrawable@
clearDrawableSelector :: Selector '[] ()
clearDrawableSelector = mkSelector "clearDrawable"

-- | @Selector@ for @update@
updateSelector :: Selector '[] ()
updateSelector = mkSelector "update"

-- | @Selector@ for @flushBuffer@
flushBufferSelector :: Selector '[] ()
flushBufferSelector = mkSelector "flushBuffer"

-- | @Selector@ for @makeCurrentContext@
makeCurrentContextSelector :: Selector '[] ()
makeCurrentContextSelector = mkSelector "makeCurrentContext"

-- | @Selector@ for @clearCurrentContext@
clearCurrentContextSelector :: Selector '[] ()
clearCurrentContextSelector = mkSelector "clearCurrentContext"

-- | @Selector@ for @copyAttributesFromContext:withMask:@
copyAttributesFromContext_withMaskSelector :: Selector '[Id NSOpenGLContext, CUInt] ()
copyAttributesFromContext_withMaskSelector = mkSelector "copyAttributesFromContext:withMask:"

-- | @Selector@ for @setValues:forParameter:@
setValues_forParameterSelector :: Selector '[Const RawId, NSOpenGLContextParameter] ()
setValues_forParameterSelector = mkSelector "setValues:forParameter:"

-- | @Selector@ for @getValues:forParameter:@
getValues_forParameterSelector :: Selector '[RawId, NSOpenGLContextParameter] ()
getValues_forParameterSelector = mkSelector "getValues:forParameter:"

-- | @Selector@ for @createTexture:fromView:internalFormat:@
createTexture_fromView_internalFormatSelector :: Selector '[CUInt, Id NSView, CUInt] ()
createTexture_fromView_internalFormatSelector = mkSelector "createTexture:fromView:internalFormat:"

-- | @Selector@ for @setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:@
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector :: Selector '[Id NSOpenGLPixelBuffer, CUInt, CInt, CInt] ()
setPixelBuffer_cubeMapFace_mipMapLevel_currentVirtualScreenSelector = mkSelector "setPixelBuffer:cubeMapFace:mipMapLevel:currentVirtualScreen:"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector '[] (Id NSOpenGLPixelBuffer)
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @pixelBufferCubeMapFace@
pixelBufferCubeMapFaceSelector :: Selector '[] CUInt
pixelBufferCubeMapFaceSelector = mkSelector "pixelBufferCubeMapFace"

-- | @Selector@ for @pixelBufferMipMapLevel@
pixelBufferMipMapLevelSelector :: Selector '[] CInt
pixelBufferMipMapLevelSelector = mkSelector "pixelBufferMipMapLevel"

-- | @Selector@ for @setTextureImageToPixelBuffer:colorBuffer:@
setTextureImageToPixelBuffer_colorBufferSelector :: Selector '[Id NSOpenGLPixelBuffer, CUInt] ()
setTextureImageToPixelBuffer_colorBufferSelector = mkSelector "setTextureImageToPixelBuffer:colorBuffer:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] (Id NSOpenGLPixelFormat)
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @view@
viewSelector :: Selector '[] RawId
viewSelector = mkSelector "view"

-- | @Selector@ for @currentContext@
currentContextSelector :: Selector '[] (Id NSOpenGLContext)
currentContextSelector = mkSelector "currentContext"

-- | @Selector@ for @currentVirtualScreen@
currentVirtualScreenSelector :: Selector '[] CInt
currentVirtualScreenSelector = mkSelector "currentVirtualScreen"

-- | @Selector@ for @setCurrentVirtualScreen:@
setCurrentVirtualScreenSelector :: Selector '[CInt] ()
setCurrentVirtualScreenSelector = mkSelector "setCurrentVirtualScreen:"

-- | @Selector@ for @CGLContextObj@
cglContextObjSelector :: Selector '[] (Ptr ())
cglContextObjSelector = mkSelector "CGLContextObj"

