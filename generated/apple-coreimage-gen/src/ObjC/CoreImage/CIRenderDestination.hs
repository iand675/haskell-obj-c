{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIRenderDestination@.
module ObjC.CoreImage.CIRenderDestination
  ( CIRenderDestination
  , IsCIRenderDestination(..)
  , initWithPixelBuffer
  , initWithIOSurface
  , initWithMTLTexture_commandBuffer
  , initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProvider
  , initWithGLTexture_target_width_height
  , initWithBitmapData_width_height_bytesPerRow_format
  , width
  , height
  , alphaMode
  , setAlphaMode
  , flipped
  , setFlipped
  , dithered
  , setDithered
  , clamped
  , setClamped
  , colorSpace
  , setColorSpace
  , blendKernel
  , setBlendKernel
  , blendsInDestinationColorSpace
  , setBlendsInDestinationColorSpace
  , captureTraceURL
  , setCaptureTraceURL
  , initWithPixelBufferSelector
  , initWithIOSurfaceSelector
  , initWithMTLTexture_commandBufferSelector
  , initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector
  , initWithGLTexture_target_width_heightSelector
  , initWithBitmapData_width_height_bytesPerRow_formatSelector
  , widthSelector
  , heightSelector
  , alphaModeSelector
  , setAlphaModeSelector
  , flippedSelector
  , setFlippedSelector
  , ditheredSelector
  , setDitheredSelector
  , clampedSelector
  , setClampedSelector
  , colorSpaceSelector
  , setColorSpaceSelector
  , blendKernelSelector
  , setBlendKernelSelector
  , blendsInDestinationColorSpaceSelector
  , setBlendsInDestinationColorSpaceSelector
  , captureTraceURLSelector
  , setCaptureTraceURLSelector

  -- * Enum types
  , CIRenderDestinationAlphaMode(CIRenderDestinationAlphaMode)
  , pattern CIRenderDestinationAlphaNone
  , pattern CIRenderDestinationAlphaPremultiplied
  , pattern CIRenderDestinationAlphaUnpremultiplied

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

import ObjC.CoreImage.Internal.Classes
import ObjC.CoreImage.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.IOSurface.Internal.Classes

-- | @- initWithPixelBuffer:@
initWithPixelBuffer :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Ptr () -> IO (Id CIRenderDestination)
initWithPixelBuffer ciRenderDestination  pixelBuffer =
    sendMsg ciRenderDestination (mkSelector "initWithPixelBuffer:") (retPtr retVoid) [argPtr pixelBuffer] >>= ownedObject . castPtr

-- | @- initWithIOSurface:@
initWithIOSurface :: (IsCIRenderDestination ciRenderDestination, IsIOSurface surface) => ciRenderDestination -> surface -> IO (Id CIRenderDestination)
initWithIOSurface ciRenderDestination  surface =
  withObjCPtr surface $ \raw_surface ->
      sendMsg ciRenderDestination (mkSelector "initWithIOSurface:") (retPtr retVoid) [argPtr (castPtr raw_surface :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMTLTexture:commandBuffer:@
initWithMTLTexture_commandBuffer :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> RawId -> RawId -> IO (Id CIRenderDestination)
initWithMTLTexture_commandBuffer ciRenderDestination  texture commandBuffer =
    sendMsg ciRenderDestination (mkSelector "initWithMTLTexture:commandBuffer:") (retPtr retVoid) [argPtr (castPtr (unRawId texture) :: Ptr ()), argPtr (castPtr (unRawId commandBuffer) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:@
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProvider :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> CULong -> CULong -> CInt -> RawId -> RawId -> IO (Id CIRenderDestination)
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProvider ciRenderDestination  width height pixelFormat commandBuffer block =
    sendMsg ciRenderDestination (mkSelector "initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:") (retPtr retVoid) [argCULong width, argCULong height, argCInt (fromIntegral pixelFormat), argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId block) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithGLTexture:target:width:height:@
initWithGLTexture_target_width_height :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> CUInt -> CUInt -> CULong -> CULong -> IO (Id CIRenderDestination)
initWithGLTexture_target_width_height ciRenderDestination  texture target width height =
    sendMsg ciRenderDestination (mkSelector "initWithGLTexture:target:width:height:") (retPtr retVoid) [argCUInt texture, argCUInt target, argCULong width, argCULong height] >>= ownedObject . castPtr

-- | @- initWithBitmapData:width:height:bytesPerRow:format:@
initWithBitmapData_width_height_bytesPerRow_format :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Ptr () -> CULong -> CULong -> CULong -> CInt -> IO (Id CIRenderDestination)
initWithBitmapData_width_height_bytesPerRow_format ciRenderDestination  data_ width height bytesPerRow format =
    sendMsg ciRenderDestination (mkSelector "initWithBitmapData:width:height:bytesPerRow:format:") (retPtr retVoid) [argPtr data_, argCULong width, argCULong height, argCULong bytesPerRow, argCInt format] >>= ownedObject . castPtr

-- | @- width@
width :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO CULong
width ciRenderDestination  =
    sendMsg ciRenderDestination (mkSelector "width") retCULong []

-- | @- height@
height :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO CULong
height ciRenderDestination  =
    sendMsg ciRenderDestination (mkSelector "height") retCULong []

-- | @- alphaMode@
alphaMode :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO CIRenderDestinationAlphaMode
alphaMode ciRenderDestination  =
    fmap (coerce :: CULong -> CIRenderDestinationAlphaMode) $ sendMsg ciRenderDestination (mkSelector "alphaMode") retCULong []

-- | @- setAlphaMode:@
setAlphaMode :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> CIRenderDestinationAlphaMode -> IO ()
setAlphaMode ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setAlphaMode:") retVoid [argCULong (coerce value)]

-- | @- flipped@
flipped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
flipped ciRenderDestination  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciRenderDestination (mkSelector "flipped") retCULong []

-- | @- setFlipped:@
setFlipped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setFlipped ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setFlipped:") retVoid [argCULong (if value then 1 else 0)]

-- | @- dithered@
dithered :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
dithered ciRenderDestination  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciRenderDestination (mkSelector "dithered") retCULong []

-- | @- setDithered:@
setDithered :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setDithered ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setDithered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- clamped@
clamped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
clamped ciRenderDestination  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciRenderDestination (mkSelector "clamped") retCULong []

-- | @- setClamped:@
setClamped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setClamped ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setClamped:") retVoid [argCULong (if value then 1 else 0)]

-- | @- colorSpace@
colorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO (Ptr ())
colorSpace ciRenderDestination  =
    fmap castPtr $ sendMsg ciRenderDestination (mkSelector "colorSpace") (retPtr retVoid) []

-- | @- setColorSpace:@
setColorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Ptr () -> IO ()
setColorSpace ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setColorSpace:") retVoid [argPtr value]

-- | @- blendKernel@
blendKernel :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO (Id CIBlendKernel)
blendKernel ciRenderDestination  =
    sendMsg ciRenderDestination (mkSelector "blendKernel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBlendKernel:@
setBlendKernel :: (IsCIRenderDestination ciRenderDestination, IsCIBlendKernel value) => ciRenderDestination -> value -> IO ()
setBlendKernel ciRenderDestination  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ciRenderDestination (mkSelector "setBlendKernel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- blendsInDestinationColorSpace@
blendsInDestinationColorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
blendsInDestinationColorSpace ciRenderDestination  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciRenderDestination (mkSelector "blendsInDestinationColorSpace") retCULong []

-- | @- setBlendsInDestinationColorSpace:@
setBlendsInDestinationColorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setBlendsInDestinationColorSpace ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setBlendsInDestinationColorSpace:") retVoid [argCULong (if value then 1 else 0)]

-- | Tell the next render using this destination to capture a Metal trace.
--
-- If this property is set to a file-based URL, then the next render using this  destination will capture a Metal trace, deleting any existing file if present. This property is nil by default.
--
-- ObjC selector: @- captureTraceURL@
captureTraceURL :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO RawId
captureTraceURL ciRenderDestination  =
    fmap (RawId . castPtr) $ sendMsg ciRenderDestination (mkSelector "captureTraceURL") (retPtr retVoid) []

-- | Tell the next render using this destination to capture a Metal trace.
--
-- If this property is set to a file-based URL, then the next render using this  destination will capture a Metal trace, deleting any existing file if present. This property is nil by default.
--
-- ObjC selector: @- setCaptureTraceURL:@
setCaptureTraceURL :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> RawId -> IO ()
setCaptureTraceURL ciRenderDestination  value =
    sendMsg ciRenderDestination (mkSelector "setCaptureTraceURL:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPixelBuffer:@
initWithPixelBufferSelector :: Selector
initWithPixelBufferSelector = mkSelector "initWithPixelBuffer:"

-- | @Selector@ for @initWithIOSurface:@
initWithIOSurfaceSelector :: Selector
initWithIOSurfaceSelector = mkSelector "initWithIOSurface:"

-- | @Selector@ for @initWithMTLTexture:commandBuffer:@
initWithMTLTexture_commandBufferSelector :: Selector
initWithMTLTexture_commandBufferSelector = mkSelector "initWithMTLTexture:commandBuffer:"

-- | @Selector@ for @initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:@
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector :: Selector
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector = mkSelector "initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:"

-- | @Selector@ for @initWithGLTexture:target:width:height:@
initWithGLTexture_target_width_heightSelector :: Selector
initWithGLTexture_target_width_heightSelector = mkSelector "initWithGLTexture:target:width:height:"

-- | @Selector@ for @initWithBitmapData:width:height:bytesPerRow:format:@
initWithBitmapData_width_height_bytesPerRow_formatSelector :: Selector
initWithBitmapData_width_height_bytesPerRow_formatSelector = mkSelector "initWithBitmapData:width:height:bytesPerRow:format:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @alphaMode@
alphaModeSelector :: Selector
alphaModeSelector = mkSelector "alphaMode"

-- | @Selector@ for @setAlphaMode:@
setAlphaModeSelector :: Selector
setAlphaModeSelector = mkSelector "setAlphaMode:"

-- | @Selector@ for @flipped@
flippedSelector :: Selector
flippedSelector = mkSelector "flipped"

-- | @Selector@ for @setFlipped:@
setFlippedSelector :: Selector
setFlippedSelector = mkSelector "setFlipped:"

-- | @Selector@ for @dithered@
ditheredSelector :: Selector
ditheredSelector = mkSelector "dithered"

-- | @Selector@ for @setDithered:@
setDitheredSelector :: Selector
setDitheredSelector = mkSelector "setDithered:"

-- | @Selector@ for @clamped@
clampedSelector :: Selector
clampedSelector = mkSelector "clamped"

-- | @Selector@ for @setClamped:@
setClampedSelector :: Selector
setClampedSelector = mkSelector "setClamped:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector
setColorSpaceSelector = mkSelector "setColorSpace:"

-- | @Selector@ for @blendKernel@
blendKernelSelector :: Selector
blendKernelSelector = mkSelector "blendKernel"

-- | @Selector@ for @setBlendKernel:@
setBlendKernelSelector :: Selector
setBlendKernelSelector = mkSelector "setBlendKernel:"

-- | @Selector@ for @blendsInDestinationColorSpace@
blendsInDestinationColorSpaceSelector :: Selector
blendsInDestinationColorSpaceSelector = mkSelector "blendsInDestinationColorSpace"

-- | @Selector@ for @setBlendsInDestinationColorSpace:@
setBlendsInDestinationColorSpaceSelector :: Selector
setBlendsInDestinationColorSpaceSelector = mkSelector "setBlendsInDestinationColorSpace:"

-- | @Selector@ for @captureTraceURL@
captureTraceURLSelector :: Selector
captureTraceURLSelector = mkSelector "captureTraceURL"

-- | @Selector@ for @setCaptureTraceURL:@
setCaptureTraceURLSelector :: Selector
setCaptureTraceURLSelector = mkSelector "setCaptureTraceURL:"

