{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , alphaModeSelector
  , blendKernelSelector
  , blendsInDestinationColorSpaceSelector
  , captureTraceURLSelector
  , clampedSelector
  , colorSpaceSelector
  , ditheredSelector
  , flippedSelector
  , heightSelector
  , initWithBitmapData_width_height_bytesPerRow_formatSelector
  , initWithGLTexture_target_width_heightSelector
  , initWithIOSurfaceSelector
  , initWithMTLTexture_commandBufferSelector
  , initWithPixelBufferSelector
  , initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector
  , setAlphaModeSelector
  , setBlendKernelSelector
  , setBlendsInDestinationColorSpaceSelector
  , setCaptureTraceURLSelector
  , setClampedSelector
  , setColorSpaceSelector
  , setDitheredSelector
  , setFlippedSelector
  , widthSelector

  -- * Enum types
  , CIRenderDestinationAlphaMode(CIRenderDestinationAlphaMode)
  , pattern CIRenderDestinationAlphaNone
  , pattern CIRenderDestinationAlphaPremultiplied
  , pattern CIRenderDestinationAlphaUnpremultiplied

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.CoreImage.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.IOSurface.Internal.Classes

-- | @- initWithPixelBuffer:@
initWithPixelBuffer :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Ptr () -> IO (Id CIRenderDestination)
initWithPixelBuffer ciRenderDestination pixelBuffer =
  sendOwnedMessage ciRenderDestination initWithPixelBufferSelector pixelBuffer

-- | @- initWithIOSurface:@
initWithIOSurface :: (IsCIRenderDestination ciRenderDestination, IsIOSurface surface) => ciRenderDestination -> surface -> IO (Id CIRenderDestination)
initWithIOSurface ciRenderDestination surface =
  sendOwnedMessage ciRenderDestination initWithIOSurfaceSelector (toIOSurface surface)

-- | @- initWithMTLTexture:commandBuffer:@
initWithMTLTexture_commandBuffer :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> RawId -> RawId -> IO (Id CIRenderDestination)
initWithMTLTexture_commandBuffer ciRenderDestination texture commandBuffer =
  sendOwnedMessage ciRenderDestination initWithMTLTexture_commandBufferSelector texture commandBuffer

-- | @- initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:@
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProvider :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> CULong -> CULong -> CInt -> RawId -> RawId -> IO (Id CIRenderDestination)
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProvider ciRenderDestination width height pixelFormat commandBuffer block =
  sendOwnedMessage ciRenderDestination initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector width height pixelFormat commandBuffer block

-- | @- initWithGLTexture:target:width:height:@
initWithGLTexture_target_width_height :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> CUInt -> CUInt -> CULong -> CULong -> IO (Id CIRenderDestination)
initWithGLTexture_target_width_height ciRenderDestination texture target width height =
  sendOwnedMessage ciRenderDestination initWithGLTexture_target_width_heightSelector texture target width height

-- | @- initWithBitmapData:width:height:bytesPerRow:format:@
initWithBitmapData_width_height_bytesPerRow_format :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Ptr () -> CULong -> CULong -> CULong -> CInt -> IO (Id CIRenderDestination)
initWithBitmapData_width_height_bytesPerRow_format ciRenderDestination data_ width height bytesPerRow format =
  sendOwnedMessage ciRenderDestination initWithBitmapData_width_height_bytesPerRow_formatSelector data_ width height bytesPerRow format

-- | @- width@
width :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO CULong
width ciRenderDestination =
  sendMessage ciRenderDestination widthSelector

-- | @- height@
height :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO CULong
height ciRenderDestination =
  sendMessage ciRenderDestination heightSelector

-- | @- alphaMode@
alphaMode :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO CIRenderDestinationAlphaMode
alphaMode ciRenderDestination =
  sendMessage ciRenderDestination alphaModeSelector

-- | @- setAlphaMode:@
setAlphaMode :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> CIRenderDestinationAlphaMode -> IO ()
setAlphaMode ciRenderDestination value =
  sendMessage ciRenderDestination setAlphaModeSelector value

-- | @- flipped@
flipped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
flipped ciRenderDestination =
  sendMessage ciRenderDestination flippedSelector

-- | @- setFlipped:@
setFlipped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setFlipped ciRenderDestination value =
  sendMessage ciRenderDestination setFlippedSelector value

-- | @- dithered@
dithered :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
dithered ciRenderDestination =
  sendMessage ciRenderDestination ditheredSelector

-- | @- setDithered:@
setDithered :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setDithered ciRenderDestination value =
  sendMessage ciRenderDestination setDitheredSelector value

-- | @- clamped@
clamped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
clamped ciRenderDestination =
  sendMessage ciRenderDestination clampedSelector

-- | @- setClamped:@
setClamped :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setClamped ciRenderDestination value =
  sendMessage ciRenderDestination setClampedSelector value

-- | @- colorSpace@
colorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO (Ptr ())
colorSpace ciRenderDestination =
  sendMessage ciRenderDestination colorSpaceSelector

-- | @- setColorSpace:@
setColorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Ptr () -> IO ()
setColorSpace ciRenderDestination value =
  sendMessage ciRenderDestination setColorSpaceSelector value

-- | @- blendKernel@
blendKernel :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO (Id CIBlendKernel)
blendKernel ciRenderDestination =
  sendMessage ciRenderDestination blendKernelSelector

-- | @- setBlendKernel:@
setBlendKernel :: (IsCIRenderDestination ciRenderDestination, IsCIBlendKernel value) => ciRenderDestination -> value -> IO ()
setBlendKernel ciRenderDestination value =
  sendMessage ciRenderDestination setBlendKernelSelector (toCIBlendKernel value)

-- | @- blendsInDestinationColorSpace@
blendsInDestinationColorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO Bool
blendsInDestinationColorSpace ciRenderDestination =
  sendMessage ciRenderDestination blendsInDestinationColorSpaceSelector

-- | @- setBlendsInDestinationColorSpace:@
setBlendsInDestinationColorSpace :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> Bool -> IO ()
setBlendsInDestinationColorSpace ciRenderDestination value =
  sendMessage ciRenderDestination setBlendsInDestinationColorSpaceSelector value

-- | Tell the next render using this destination to capture a Metal trace.
--
-- If this property is set to a file-based URL, then the next render using this  destination will capture a Metal trace, deleting any existing file if present. This property is nil by default.
--
-- ObjC selector: @- captureTraceURL@
captureTraceURL :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> IO RawId
captureTraceURL ciRenderDestination =
  sendMessage ciRenderDestination captureTraceURLSelector

-- | Tell the next render using this destination to capture a Metal trace.
--
-- If this property is set to a file-based URL, then the next render using this  destination will capture a Metal trace, deleting any existing file if present. This property is nil by default.
--
-- ObjC selector: @- setCaptureTraceURL:@
setCaptureTraceURL :: IsCIRenderDestination ciRenderDestination => ciRenderDestination -> RawId -> IO ()
setCaptureTraceURL ciRenderDestination value =
  sendMessage ciRenderDestination setCaptureTraceURLSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPixelBuffer:@
initWithPixelBufferSelector :: Selector '[Ptr ()] (Id CIRenderDestination)
initWithPixelBufferSelector = mkSelector "initWithPixelBuffer:"

-- | @Selector@ for @initWithIOSurface:@
initWithIOSurfaceSelector :: Selector '[Id IOSurface] (Id CIRenderDestination)
initWithIOSurfaceSelector = mkSelector "initWithIOSurface:"

-- | @Selector@ for @initWithMTLTexture:commandBuffer:@
initWithMTLTexture_commandBufferSelector :: Selector '[RawId, RawId] (Id CIRenderDestination)
initWithMTLTexture_commandBufferSelector = mkSelector "initWithMTLTexture:commandBuffer:"

-- | @Selector@ for @initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:@
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector :: Selector '[CULong, CULong, CInt, RawId, RawId] (Id CIRenderDestination)
initWithWidth_height_pixelFormat_commandBuffer_mtlTextureProviderSelector = mkSelector "initWithWidth:height:pixelFormat:commandBuffer:mtlTextureProvider:"

-- | @Selector@ for @initWithGLTexture:target:width:height:@
initWithGLTexture_target_width_heightSelector :: Selector '[CUInt, CUInt, CULong, CULong] (Id CIRenderDestination)
initWithGLTexture_target_width_heightSelector = mkSelector "initWithGLTexture:target:width:height:"

-- | @Selector@ for @initWithBitmapData:width:height:bytesPerRow:format:@
initWithBitmapData_width_height_bytesPerRow_formatSelector :: Selector '[Ptr (), CULong, CULong, CULong, CInt] (Id CIRenderDestination)
initWithBitmapData_width_height_bytesPerRow_formatSelector = mkSelector "initWithBitmapData:width:height:bytesPerRow:format:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CULong
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CULong
heightSelector = mkSelector "height"

-- | @Selector@ for @alphaMode@
alphaModeSelector :: Selector '[] CIRenderDestinationAlphaMode
alphaModeSelector = mkSelector "alphaMode"

-- | @Selector@ for @setAlphaMode:@
setAlphaModeSelector :: Selector '[CIRenderDestinationAlphaMode] ()
setAlphaModeSelector = mkSelector "setAlphaMode:"

-- | @Selector@ for @flipped@
flippedSelector :: Selector '[] Bool
flippedSelector = mkSelector "flipped"

-- | @Selector@ for @setFlipped:@
setFlippedSelector :: Selector '[Bool] ()
setFlippedSelector = mkSelector "setFlipped:"

-- | @Selector@ for @dithered@
ditheredSelector :: Selector '[] Bool
ditheredSelector = mkSelector "dithered"

-- | @Selector@ for @setDithered:@
setDitheredSelector :: Selector '[Bool] ()
setDitheredSelector = mkSelector "setDithered:"

-- | @Selector@ for @clamped@
clampedSelector :: Selector '[] Bool
clampedSelector = mkSelector "clamped"

-- | @Selector@ for @setClamped:@
setClampedSelector :: Selector '[Bool] ()
setClampedSelector = mkSelector "setClamped:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Ptr ())
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector '[Ptr ()] ()
setColorSpaceSelector = mkSelector "setColorSpace:"

-- | @Selector@ for @blendKernel@
blendKernelSelector :: Selector '[] (Id CIBlendKernel)
blendKernelSelector = mkSelector "blendKernel"

-- | @Selector@ for @setBlendKernel:@
setBlendKernelSelector :: Selector '[Id CIBlendKernel] ()
setBlendKernelSelector = mkSelector "setBlendKernel:"

-- | @Selector@ for @blendsInDestinationColorSpace@
blendsInDestinationColorSpaceSelector :: Selector '[] Bool
blendsInDestinationColorSpaceSelector = mkSelector "blendsInDestinationColorSpace"

-- | @Selector@ for @setBlendsInDestinationColorSpace:@
setBlendsInDestinationColorSpaceSelector :: Selector '[Bool] ()
setBlendsInDestinationColorSpaceSelector = mkSelector "setBlendsInDestinationColorSpace:"

-- | @Selector@ for @captureTraceURL@
captureTraceURLSelector :: Selector '[] RawId
captureTraceURLSelector = mkSelector "captureTraceURL"

-- | @Selector@ for @setCaptureTraceURL:@
setCaptureTraceURLSelector :: Selector '[RawId] ()
setCaptureTraceURLSelector = mkSelector "setCaptureTraceURL:"

